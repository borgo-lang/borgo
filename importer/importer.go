package main

import (
	// "bytes"
	// "flag"
	"flag"
	"fmt"
	"log"

	"go/ast"
	"go/doc"
	"go/parser"
	// "go/printer"
	"go/token"
	// "io/fs"
	// "os"
	// "path/filepath"
	// "regexp"
	// "strings"
	// "testing"
	// "text/template"
)

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
		for _, t := range doc.Types {
			// name := t.Name
			for _, decl := range t.Decl.Specs {
				fmt.Printf("%+v\n", decl)
			}
		}

		// Functions
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
			for _, param := range decl.Type.Results.List {
				fmt.Println(param.Names, param.Type)
			}

			fmt.Println("")
		}
	}
}
