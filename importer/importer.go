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

// ast.GenDecl
// ast.FuncDecl

var folder = flag.String("folder", "", "folder containing packages")

func main() {
	fmt.Println("hi")

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
				fmt.Println("  ", f.Name)
			}

			for _, f := range t.Methods {
				fmt.Println("  ", f.Name)
			}

			for _, decl := range t.Decl.Specs {
				if ty, ok := decl.(*ast.TypeSpec); ok {

					if s, ok := ty.Type.(*ast.StructType); ok {
						if s.Fields != nil {
							for _, param := range s.Fields.List {
								fmt.Println(param.Names, param.Type)
							}
						}
					}

				}
			}
		}

		// Functions
		// Standalone functions
		for _, f := range doc.Funcs {
			decl := f.Decl
			fmt.Println(f.Name)
			// fmt.Printf("%+v\n", decl.Type)

			fmt.Println("PARAMS:")
			for _, param := range decl.Type.Params.List {
				fmt.Println(param.Names, param.Type)
			}

			fmt.Println("")

			fmt.Println("RETURN:")
			if decl.Type.Results != nil {
				for _, param := range decl.Type.Results.List {
					fmt.Println(param.Names, param.Type)
				}
			}

			fmt.Println("")
		}

	}
}
