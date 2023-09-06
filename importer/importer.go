package main

import (
	"bytes"
	"flag"
	"fmt"
	"go/ast"
	"go/doc"
	"go/parser"
	"go/token"
	"log"
	"reflect"
	"sort"
	"strings"
)

var folder = flag.String("folder", "", "folder containing packages")

var SKIPPED_TYPES = []any{}

func SKIP(what any) Type {
	SKIPPED_TYPES = append(SKIPPED_TYPES, what)
	return mono("any")
}

var RESERVED_WORDS = map[string]bool{
	"match": true,
	"use":   true,
	"in":    true,
	"fn":    true,
}

// TODO find a better way to get the right module path
// instead of hardcoding it
var REWRITE_MODULES = map[string]string{
	"fs":        "io.fs",
	"url":       "net.url",
	"netip":     "net.netip",
	"http":      "net.http",
	"textproto": "net.textproto",
	"multipart": "mime.multipart",
	"tls":       "crypto.tls",
	"x509":      "crypto.x509",
}

// Packages that won't be imported, either because they're big or because they have a lot of dependencies.
// If a type is exported from a denied package, it will be converted to `any`
var DENY_LIST = map[string]bool{
	"syscall": true,
	"x509":    true,
}

type Type interface {
	IsType()
	String() string
}

func mono(name string) Type {
	return TyCon{name: name, args: []Type{}}
}

type TyCon struct {
	name string
	args []Type
}

func (t TyCon) IsType() {}

func (t TyCon) String() string {
	if t.name == "Unit" {
		return "()"
	}

	if t.name == "Ref" {
		return "*" + t.args[0].String()
	}

	if t.name == "Slice" {
		return "[" + t.args[0].String() + "]"
	}

	// Big nasty hack
	// TODO
	// if the arg references `syscall`, then turn it into `any`.
	// syscall package is huge and I'm skipping it for now
	for denied := range DENY_LIST {
		if strings.Contains(t.name, denied) {
			return "any"
		}
	}

	if len(t.args) == 0 {
		return t.name
	}

	return t.name + "<" + joinTypes(t.args) + ">"
}

func (t TyCon) AddSelector(field string) TyCon {
	name := t.name + "." + field
	return TyCon{name: name, args: t.args}
}

type TyFun struct {
	bounds []Bound
	args   []FuncArg
	ret    []Type
}

func (t TyFun) IsType() {}

func (t TyFun) String() string {
	args := functionArgsToString(t.args)
	ret := toReturnType(t.ret)
	return fmt.Sprintf("fn (%s) -> %s", args, ret)
}

type Bound struct {
	Generic string
	Type    Type
}

type Function struct {
	Name string
	Type TyFun
}

type FuncArg struct {
	Name string
	Type Type
}

func (p FuncArg) String() string {
	return p.Name + ": " + p.Type.String()
}

type Method struct {
	SelfType Type
	Func     Function
}

type Alias struct {
	Name   string
	Type   Type
	Bounds []Bound
}

type Newtype struct {
	Name   string
	Type   Type
	Bounds []Bound
}

type Struct struct {
	Name   string
	Bounds []Bound
	Fields []StructField
	Kind   TypeKind
}

type StructField struct {
	Name string
	Type Type
}

type Variable struct {
	Name string
	Type Type
}

type TypeKind int

const (
	TypeStruct TypeKind = iota
	TypeInterface
)

type Package struct {
	Name     string
	Path     string
	Imports  []string
	Types    []Struct
	Aliases  []Alias
	Newtypes []Newtype
	Funcs    []Function
	Methods  map[string][]Method // type => methods
	Vars     []Variable          // consts and vars
}

func (p *Package) AddFunction(name string, f *ast.FuncType) {
	function := Function{Name: name, Type: p.parseFunc(f)}
	p.Funcs = append(p.Funcs, function)
}

func (p *Package) AddMethod(name string, recv string, isPointer bool, list *ast.FieldList, f *ast.FuncType) {
	bounds := p.parseBounds(list)
	generics := []Type{}

	for _, b := range bounds {
		generics = append(generics, removeReferences(b.Type))
	}

	selfType := TyCon{name: recv, args: generics}

	if isPointer {
		selfType = TyCon{name: "Ref", args: []Type{selfType}}
	}

	function := Function{Name: name, Type: p.parseFunc(f)}
	method := Method{SelfType: selfType, Func: function}

	p.Methods[recv] = append(p.Methods[recv], method)
}

func (p *Package) AddTypeAlias(name string, t Type, bounds []Bound) {
	alias := Alias{Name: name, Type: t, Bounds: bounds}
	p.Aliases = append(p.Aliases, alias)
}

func (p *Package) AddNewType(name string, t Type, bounds []Bound) {
	typ := Newtype{Name: name, Type: t, Bounds: bounds}
	p.Newtypes = append(p.Newtypes, typ)
}

func (p *Package) AddStruct(name string, bounds []Bound, list []*ast.Field, kind TypeKind) {
	fields := []StructField{}

	for _, f := range list {
		name := ""

		if len(f.Names) > 0 {
			name = f.Names[0].Name
		}

		fields = append(fields, StructField{Name: name, Type: p.parseTypeExpr(f.Type)})
	}

	s := Struct{Name: name, Bounds: bounds, Fields: fields, Kind: kind}
	p.Types = append(p.Types, s)
}

func (p *Package) AddVariable(name string, ty Type) {
	p.Vars = append(p.Vars, Variable{name, ty})
}

func (p *Package) GroupMethodsByGenerics() map[string][]Method {
	ret := map[string][]Method{}

	for _, methods := range p.Methods {
		for _, m := range methods {
			ty := removeReferences(m.SelfType).String()
			ret[ty] = append(ret[ty], m)
		}
	}

	return ret
}

func (p *Package) String() string {
	var w bytes.Buffer

	for _, pkg := range p.Imports {
		name := pkg

		if rewrite, found := REWRITE_MODULES[pkg]; found {
			name = rewrite
		}

		// Big nasty hack
		// TODO
		// Don't import syscall because it's huge
		if DENY_LIST[pkg] {
			fmt.Fprintf(&w, "// ")
		}

		fmt.Fprintf(&w, "use %s\n", name)
	}

	fmt.Fprintf(&w, "\n\n")

	// collect functions into a map
	// sort functions by name to maintain stable ordering
	sortedFns := []string{}
	functions := map[string]Function{}

	for _, f := range p.Funcs {
		sortedFns = append(sortedFns, f.Name)
		functions[f.Name] = f
	}

	sort.Strings(sortedFns)

	for _, fnName := range sortedFns {
		f := functions[fnName]
		bounds := boundsToString(f.Type.bounds)
		args := functionArgsToString(f.Type.args)
		ret := toReturnType(f.Type.ret)

		fmt.Fprintf(&w, "fn %s %s (%s) -> %s { EXT }\n\n", f.Name, bounds, args, ret)
	}

	grouped := p.GroupMethodsByGenerics()
	// Sort by type name to maintain stable ordering
	sortedTypes := []string{}

	for t := range grouped {
		sortedTypes = append(sortedTypes, t)
	}

	sort.Strings(sortedTypes)

	for _, ty := range sortedTypes {
		// TODO ty is just a Go string for the type, not exactly what we need.
		fmt.Fprintf(&w, "impl (self: %s) {\n\n", ty)

		methods := grouped[ty]

		for _, m := range methods {
			f := m.Func
			bounds := boundsToString(f.Type.bounds)
			args := functionArgsToString(f.Type.args)
			ret := toReturnType(f.Type.ret)
			// self := selfTypeToString(m.SelfType)

			fmt.Fprintf(&w, "fn %s %s (%s) -> %s { EXT }\n\n", f.Name, bounds, args, ret)
		}

		fmt.Fprintf(&w, "}\n\n")
	}

	for _, a := range p.Aliases {
		fmt.Fprintf(&w, "type %s = %s\n\n", a.Name, a.Type.String())
	}

	for _, a := range p.Newtypes {
		fmt.Fprintf(&w, "struct %s(%s)\n\n", a.Name, a.Type.String())
	}

	for _, s := range p.Types {

		switch s.Kind {
		case TypeStruct:
			bounds := boundsToString(s.Bounds)
			fields := structFieldsToString(s.Fields)
			def := "struct " + s.Name + bounds + "{\n" + fields + "\n}\n\n"
			fmt.Fprint(&w, def)

		case TypeInterface:
			bounds, fields := splitBoundsAndFieldsForInterface(s.Fields)

			generics := "" // TODO asdf add generics

			newBounds := interfaceBoundsToString(bounds)
			newFields := interfaceFieldsToString(fields)

			def := "interface " + s.Name + generics + " {\n" + newBounds + "\n" + newFields + "\n}\n\n"
			fmt.Fprint(&w, def)
		}

	}

	return w.String()
}

func (p *Package) parseBounds(list *ast.FieldList) []Bound {
	bounds := []Bound{}

	if list == nil {
		return bounds
	}

	for _, param := range list.List {
		name := param.Names[0].Name // TODO this is probably very wrong
		bounds = append(bounds, Bound{Generic: name, Type: p.parseTypeExpr(param.Type)})
	}

	return bounds
}

func (p *Package) parseFunc(f *ast.FuncType) TyFun {
	// fmt.Printf("%+v\n", decl.Type)

	bounds := []Bound{}

	// function bounds
	if f.TypeParams != nil {
		for _, param := range f.TypeParams.List {
			name := param.Names[0].Name // TODO this is probably very wrong
			bounds = append(bounds, Bound{Generic: name, Type: p.parseTypeExpr(param.Type)})
		}
	}

	args := []FuncArg{}

	nextUnnamed := 0

	// function params
	for _, param := range f.Params.List {
		// closures passed in as params won't have named parameters
		// ie. func (f func (rune) bool) the rune is unnamed

		arg := FuncArg{Name: "unnamed param", Type: p.parseTypeExpr(param.Type)}

		for _, name := range param.Names {
			arg := arg
			arg.Name = replaceReserved(name.Name)
			args = append(args, arg)
		}

		if len(param.Names) == 0 {
			arg := arg
			arg.Name = fmt.Sprintf("param%d", nextUnnamed)
			args = append(args, arg)
			nextUnnamed++
		}

	}

	ret := []Type{}

	// function return
	if f.Results != nil {
		for _, param := range f.Results.List {
			ret = append(ret, p.parseTypeExpr(param.Type))
		}
	} else {
		ret = append(ret, mono("Unit"))
	}

	return TyFun{bounds: bounds, args: args, ret: ret}
}

func (p *Package) ensureImport(name string) {
	for _, i := range p.Imports {
		if i == name {
			return
		}
	}

	p.Imports = append(p.Imports, name)
}

func (p *Package) parseTypeExpr(expr ast.Expr) Type {
	switch ty := expr.(type) {

	case *ast.Ident:
		return TyCon{name: ty.Name, args: []Type{}}

	case *ast.ArrayType:
		inner := p.parseTypeExpr(ty.Elt)
		return TyCon{name: "Slice", args: []Type{inner}}

	case *ast.MapType:
		key := p.parseTypeExpr(ty.Key)
		val := p.parseTypeExpr(ty.Value)
		return TyCon{name: "Map", args: []Type{key, val}}

	case *ast.FuncType:
		return p.parseFunc(ty)

	case *ast.StarExpr:
		inner := p.parseTypeExpr(ty.X)
		return TyCon{name: "Ref", args: []Type{inner}}

	case *ast.Ellipsis:
		inner := p.parseTypeExpr(ty.Elt)
		return TyCon{name: "VarArgs", args: []Type{inner}}

	case *ast.SelectorExpr:
		inner := p.parseTypeExpr(ty.X)
		switch con := inner.(type) {
		case TyCon:
			// TODO asdf this is not sufficient, need to get the full package path
			p.ensureImport(con.name)
			name := con.name + "." + ty.Sel.Name
			return TyCon{name: name, args: []Type{}}
		default:
			log.Fatalf("expected TyCon in selector, got %T", con)
		}

	case *ast.ChanType:
		inner := p.parseTypeExpr(ty.Value)
		name := "Channel"

		switch ty.Dir {
		case ast.RECV:
			name = "Receiver"
		case ast.SEND:
			name = "Sender"
		}

		return TyCon{name: name, args: []Type{inner}}

	case *ast.StructType:
		if ty.Fields != nil && len(ty.Fields.List) > 0 {
			return SKIP(fmt.Sprintf("found struct{} declaration with fields %v, skipping", ty.Fields.List))
		}

		return mono("Unit")

	case *ast.IndexExpr:
		return p.parseTypeExpr(ty.Index)

	default:
		log.Fatalf("unhandled typeExpr %T\n%v", expr, expr)
	}

	return TyCon{}
}

/// --------------
/// Formatting to string
/// --------------

func toReturnType(types []Type) string {
	if len(types) == 2 && reflect.DeepEqual(types[1], mono("error")) {
		return TyCon{name: "Result", args: []Type{types[0]}}.String()
	}

	if len(types) == 2 && reflect.DeepEqual(types[1], mono("bool")) {
		return TyCon{name: "Option", args: []Type{types[0]}}.String()
	}

	switch len(types) {
	case 0:
		return ""
	case 1:
		return types[0].String()
	default:
		return "(" + joinTypes(types) + ")"

	}
}

func boundsToString(fbounds []Bound) string {
	if len(fbounds) == 0 {
		return ""
	}

	args := []string{}

	for _, b := range fbounds {
		arg := b.Generic

		if !reflect.DeepEqual(b.Type, mono("any")) {
			arg = arg + ": " + b.Type.String()
		}

		args = append(args, arg)
	}

	return "<" + strings.Join(args, ", ") + ">"
}

func functionArgsToString(fargs []FuncArg) string {
	args := []string{}

	for _, arg := range fargs {
		args = append(args, arg.String())
	}

	return strings.Join(args, ", ")

}

func structFieldsToString(list []StructField) string {
	fields := []string{}

	for _, f := range list {
		// Embedded structs are currently tricky, skip this field for now
		if len(f.Name) == 0 {
			fields = append(fields, "// skipping embedded field "+f.Type.String())
			continue
		}

		fields = append(fields, "  "+f.Name+": "+f.Type.String())
	}

	return strings.Join(fields, ",\n")
}

func interfaceBoundsToString(list []string) string {
	supertraits := []string{}

	for _, b := range list {
		// TODO asdf supertraits should contain Type not string
		supertraits = append(supertraits, fmt.Sprintf("impl %s", b))
	}

	return strings.Join(supertraits, "\n")
}

func interfaceFieldsToString(list []StructField) string {
	fields := []string{}

	for _, f := range list {
		rendered := f.Type.String()
		// hack hack
		rendered = strings.TrimPrefix(rendered, "fn ")

		fields = append(fields, "  fn "+f.Name+" "+rendered)
	}

	return strings.Join(fields, "\n")
}

func joinTypes(types []Type) string {
	args := []string{}

	for _, arg := range types {
		args = append(args, arg.String())
	}

	return strings.Join(args, ", ")
}

func selfTypeToString(ty Type) string {
	if con, ok := ty.(TyCon); ok {
		switch con.name {
		case "Ref":
			return "&self"
		case "RefMut":
			return "&mut self"
		}

		return "self"
	}

	panic("expected TyCon as self type")
}

func splitBoundsAndFieldsForInterface(structFields []StructField) ([]string, []StructField) {
	bounds := []string{}
	fields := []StructField{}

	for _, f := range structFields {
		if f.Name == "" {
			bounds = append(bounds, f.Type.String())
			continue
		}

		fields = append(fields, f)
	}

	return bounds, fields
}

func removeReferences(ty Type) Type {
	switch ty := ty.(type) {
	case TyCon:
		if ty.name == "Ref" || ty.name == "RefMut" {
			return ty.args[0]
		}

	case TyFun:
		panic("removeReferences on funcs shouldn't be needed...")
	}

	return ty
}

func replaceReserved(name string) string {
	if _, ok := RESERVED_WORDS[name]; ok {
		return name + "_"
	}

	return name
}

func main() {
	flag.Parse()

	dataDir := *folder

	fset := token.NewFileSet()

	pkgs, err := parser.ParseDir(fset, dataDir, nil, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}

	for _, pkg := range pkgs {
		if strings.HasSuffix(pkg.Name, "_test") || pkg.Name == "main" {
			continue
		}

		importPath := dataDir + "/" + pkg.Name

		var files []*ast.File
		for _, f := range pkg.Files {
			files = append(files, f)
		}

		doc, err := doc.NewFromFiles(fset, files, importPath, doc.Mode(0))
		if err != nil {
			log.Fatal(err)
		}

		p := &Package{
			Name:    doc.Name,
			Path:    doc.Name, // TODO how do we get the import path, ie. "net/http"?
			Methods: map[string][]Method{},
		}

		// Types
		// functions and methods are attached
		for _, t := range doc.Types {
			// fmt.Println(t.Name)

			// TODO also check Vars and Consts

			for _, f := range t.Funcs {
				p.AddFunction(f.Name, f.Decl.Type)
			}

			for _, f := range t.Methods {
				// TODO parse recv
				// fmt.Println(f.Recv) string starting with *
				isPointer := strings.HasPrefix(f.Recv, "*")

				// this is terrible, I know
				fieldList := f.Decl.Recv
				if !strings.Contains(f.Recv, "[") {
					fieldList = nil
				}

				p.AddMethod(f.Name, t.Name, isPointer, fieldList, f.Decl.Type)
			}

			for _, decl := range t.Decl.Specs {

				if spec, ok := decl.(*ast.TypeSpec); ok {

					bounds := p.parseBounds(spec.TypeParams)

					switch ty := spec.Type.(type) {

					case *ast.Ident:
						if spec.Assign > 0 {
							// if there's a "=" token, then it's an alias
							p.AddTypeAlias(spec.Name.Name, mono(ty.Name), bounds)
						} else {
							// otherwise it's a newtype
							p.AddNewType(spec.Name.Name, mono(ty.Name), bounds)
						}

					case *ast.StructType:
						p.AddStruct(spec.Name.Name, bounds, ty.Fields.List, TypeStruct)

					case *ast.InterfaceType:
						p.AddStruct(spec.Name.Name, bounds, ty.Methods.List, TypeInterface)
						// p.AddInterface(spec.Name.Name, bounds, ty.Fields.List)

					case *ast.ArrayType:
						inner := p.parseTypeExpr(ty.Elt)
						p.AddTypeAlias(spec.Name.Name, TyCon{name: "Slice", args: []Type{inner}}, bounds)

					case *ast.FuncType:
						p.AddTypeAlias(spec.Name.Name, p.parseFunc(ty), bounds)

					case *ast.MapType:
						key := p.parseTypeExpr(ty.Key)
						val := p.parseTypeExpr(ty.Value)
						p.AddTypeAlias(spec.Name.Name, TyCon{name: "Map", args: []Type{key, val}}, bounds)

					case *ast.SelectorExpr:
						pkg := p.parseTypeExpr(ty.X)
						pkg = pkg.(TyCon).AddSelector(ty.Sel.Name)
						p.AddTypeAlias(spec.Name.Name, pkg, bounds)

					case *ast.StarExpr:
						inner := p.parseTypeExpr(ty.X)
						p.AddTypeAlias(spec.Name.Name, TyCon{name: "Ref", args: []Type{inner}}, bounds)

					default:
						fmt.Println(pkg.Name, t.Name)
						log.Fatalf("unhandled TySpec, got %T", ty)
					}

				}
			}
		}

		// Functions
		// Standalone functions
		for _, f := range doc.Funcs {
			// parseFunc(f.Decl.Type)
			p.AddFunction(f.Name, f.Decl.Type)
		}

		// Consts
		// TODO consts are untyped so it's difficult to import them..
		/*
			for _, c := range doc.Consts {
				for _, name := range c.Names {
					for _, decl := range c.Decl.Specs {
						if spec, ok := decl.(*ast.ValueSpec); ok {
							if spec.Type != nil {
								ty := p.parseTypeExpr(spec.Type)
								p.AddVariable(name, ty)
							}
						}
					}
				}
			}
		*/

		fmt.Println(p)
	}

	if len(SKIPPED_TYPES) > 0 {
		fmt.Println(SKIPPED_TYPES)
	}
}
