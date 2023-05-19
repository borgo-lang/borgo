package main

import (
	"bytes"
	"flag"
	"fmt"
	"log"
	"reflect"
	"strings"

	"go/ast"
	"go/doc"
	"go/parser"
	"go/token"
)

var folder = flag.String("folder", "", "folder containing packages")

var SKIPPED_TYPES = []any{}

func SKIP(what any) Type {
	SKIPPED_TYPES = append(SKIPPED_TYPES, what)
	return mono("any")
}

func mono(name string) Type {
	return TyCon{name: name, args: []Type{}}
}

type Type interface {
	IsType()
	String() string
}

type TyCon struct {
	name string
	args []Type
}

func (t TyCon) IsType() {}

func (t TyCon) String() string {
	// TODO add args
	if t.name == "Unit" {
		return "()"
	}

	if t.name == "Ref" {
		return "*" + t.args[0].String()
	}

	if t.name == "Slice" {
		return "[" + t.args[0].String() + "]"
	}

	if t.name == "VarArgs" {
		return "..." + t.args[0].String()
	}

	if len(t.args) == 0 {
		return t.name
	}

	return t.name + "<" + joinTypes(t.args) + ">"
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

func toReturnType(types []Type) string {
	if len(types) == 2 && reflect.DeepEqual(types[1], mono("error")) {
		return TyCon{name: "Result", args: types}.String()
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

func functionArgsToString(fargs []FuncArg) string {
	args := []string{}

	for _, arg := range fargs {
		args = append(args, arg.String())
	}

	return strings.Join(args, ", ")
}

func joinTypes(types []Type) string {
	args := []string{}

	for _, arg := range types {
		args = append(args, arg.String())
	}

	return strings.Join(args, ", ")
}

type Bound struct {
	generic    string
	constraint Type
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

type Package struct {
	Types []Type
	Funcs []Function
}

func (p *Package) AddFunction(name string, f *ast.FuncType) {
	function := Function{Name: name, Type: parseFunc(f)}
	p.Funcs = append(p.Funcs, function)
}

func (p *Package) AddMethod(name string, recv string, f *ast.FuncType) {
	function := Function{Name: name, Type: parseFunc(f)}
	p.Funcs = append(p.Funcs, function)
}

func (p *Package) String() string {
	var w bytes.Buffer

	for _, f := range p.Funcs {

		args := functionArgsToString(f.Type.args)

		ret := toReturnType(f.Type.ret)
		fmt.Fprintf(&w, "fn %s (%s) -> %s;\n\n", f.Name, args, ret)
	}

	return w.String()
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
		importPath := dataDir + "/" + pkg.Name

		var files []*ast.File
		for _, f := range pkg.Files {
			files = append(files, f)
		}

		doc, err := doc.NewFromFiles(fset, files, importPath, doc.Mode(0))
		if err != nil {
			log.Fatal(err)
		}

		p := &Package{}

		// Types
		// functions and methods are attached
		for _, t := range doc.Types {
			// fmt.Println(t.Name)

			for _, f := range t.Funcs {
				p.AddFunction(f.Name, f.Decl.Type)
				// parseFunc(f.Decl.Type)
			}

			for _, f := range t.Methods {
				// fmt.Println(f.Recv) string starting with *
				p.AddMethod(f.Name, f.Recv, f.Decl.Type)
				//parseFunc(f.Decl.Type)
			}

			for _, decl := range t.Decl.Specs {
				// TODO use switch
				if ty, ok := decl.(*ast.TypeSpec); ok {

					if s, ok := ty.Type.(*ast.StructType); ok {
						if s.Fields != nil {
							for _, param := range s.Fields.List {
								// fmt.Println(param.Names, param.Type)
								parseTypeExpr(param.Type)
								// TODO
								// p.AddType(f.Name, f.Recv, f.Decl.Type)
							}
						}
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

		fmt.Println(p)
	}

	if len(SKIPPED_TYPES) > 0 {
		fmt.Println(SKIPPED_TYPES)
	}
}

func parseFunc(f *ast.FuncType) TyFun {
	// fmt.Printf("%+v\n", decl.Type)

	bounds := []Bound{}

	// function bounds
	if f.TypeParams != nil {
		for _, param := range f.TypeParams.List {
			// TODO
			parseTypeExpr(param.Type)
		}
	}

	args := []FuncArg{}

	// function params
	for _, param := range f.Params.List {
		name := param.Names[0].Name // TODO this is probably very wrong
		args = append(args, FuncArg{Name: name, Type: parseTypeExpr(param.Type)})
	}

	ret := []Type{}

	// function return
	if f.Results != nil {
		for _, param := range f.Results.List {
			ret = append(ret, parseTypeExpr(param.Type))
		}
	} else {
		ret = append(ret, mono("Unit"))
	}

	return TyFun{bounds: bounds, args: args, ret: ret}
}

func parseTypeExpr(expr ast.Expr) Type {
	switch ty := expr.(type) {

	case *ast.Ident:
		return TyCon{name: ty.Name, args: []Type{}}

	case *ast.ArrayType:
		inner := parseTypeExpr(ty.Elt)
		return TyCon{name: "Slice", args: []Type{inner}}

	case *ast.MapType:
		key := parseTypeExpr(ty.Key)
		val := parseTypeExpr(ty.Value)
		return TyCon{name: "Map", args: []Type{key, val}}

	case *ast.FuncType:
		return parseFunc(ty)

	case *ast.StarExpr:
		inner := parseTypeExpr(ty.X)
		return TyCon{name: "Ref", args: []Type{inner}}

	case *ast.Ellipsis:
		inner := parseTypeExpr(ty.Elt)
		return TyCon{name: "VarArgs", args: []Type{inner}}

	case *ast.SelectorExpr:
		inner := parseTypeExpr(ty.X)
		switch con := inner.(type) {
		case TyCon:
			name := con.name + "::" + ty.Sel.Name
			return TyCon{name: name, args: []Type{}}
		default:
			log.Fatalf("expected TyCon in selector, got %T", con)
		}

	case *ast.ChanType:
		inner := parseTypeExpr(ty.Value)
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

	default:
		log.Fatalf("unhandled typeExpr %T\n%v", expr, expr)
	}

	return TyCon{}
}
