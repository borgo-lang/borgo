package main

import (
	"flag"
	"fmt"
	"log"

	"go/ast"
	"go/doc"
	"go/parser"
	"go/token"
)

var folder = flag.String("folder", "", "folder containing packages")

type Type interface {
	IsType()
}

type TyCon struct {
	name string
	args []Type
}

func (t TyCon) IsType() {}

type TyFun struct {
	bounds []Bound
	args   []Type
	ret    []Type
}

func (t TyFun) IsType() {}

type Bound struct {
	generic    string
	constraint Type
}

var SKIPPED_TYPES = []any{}

func SKIP(what any) Type {
	SKIPPED_TYPES = append(SKIPPED_TYPES, what)
	return TyCon{name: "SKIP", args: []Type{}}
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

		// Types
		// functions and methods are attached
		for _, t := range doc.Types {
			fmt.Println(t.Name)

			for _, f := range t.Funcs {
				parseFunc(f.Decl.Type)
			}

			for _, f := range t.Methods {
				// fmt.Println(f.Recv) string starting with *
				parseFunc(f.Decl.Type)
			}

			for _, decl := range t.Decl.Specs {
				if ty, ok := decl.(*ast.TypeSpec); ok {

					if s, ok := ty.Type.(*ast.StructType); ok {
						if s.Fields != nil {
							for _, param := range s.Fields.List {
								// fmt.Println(param.Names, param.Type)
								parseTypeExpr(param.Type)
							}
						}
					}

				}
			}
		}

		// Functions
		// Standalone functions
		for _, f := range doc.Funcs {
			fmt.Println(f.Name)
			parseFunc(f.Decl.Type)
		}

	}

	if len(SKIPPED_TYPES) > 0 {
		fmt.Println(SKIPPED_TYPES)
	}
}

func parseFunc(f *ast.FuncType) Type {
	// fmt.Printf("%+v\n", decl.Type)

	// function bounds
	if f.TypeParams != nil {
		for _, param := range f.TypeParams.List {
			// fmt.Println(param.Names, param.Type)
			parseTypeExpr(param.Type)
		}
	}

	// function params
	for _, param := range f.Params.List {
		// fmt.Println(param.Names, param.Type)
		parseTypeExpr(param.Type)
	}

	// function return
	if f.Results != nil {
		for _, param := range f.Results.List {
			// fmt.Println(param.Names, param.Type)
			parseTypeExpr(param.Type)
		}
	}

	return TyFun{}
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

		return TyCon{name: "Unit", args: []Type{}}

	default:
		log.Fatalf("unhandled typeExpr %T\n%v", expr, expr)
	}

	return TyCon{}
}
