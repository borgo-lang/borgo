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

	default:
		log.Fatalf("unhandled typeExpr %T", expr)
	}

	return TyCon{}
}
