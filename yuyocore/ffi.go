package yuyocore

/* !!!!
The codes in this file are not used yet, for now.
*/

/*
package main

scm
(define x 24)
(define-proc (fn n m) hoge)

scm

import "fmt"

func hoge (n int, m int) int {
 do something
 return n
}

*/

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
)

type funcInfo struct {
	name string
	args []nameAndType
	rets []nameAndType
}

type nameAndType struct {
	name string
	typ  string
}

var fncs = make([]funcInfo, 0)

func funcInspect(node ast.Node) bool {
	if file, ok := node.(*ast.File); ok {
		decls := file.Decls
		for _, decl := range decls {
			if fdecl, ok := decl.(*ast.FuncDecl); ok {
				elm := funcInfo{name: fdecl.Name.Name, args: make([]nameAndType, 0), rets: make([]nameAndType, 0)}

				if fdecl.Type.Params != nil {
					parms := fdecl.Type.Params.List
					for _, parm := range parms {
						for _, pname := range parm.Names {
							elm.args = append(elm.args, nameAndType{name: pname.Name, typ: parm.Type.(*ast.Ident).Name})
						}
					}
				}

				if fdecl.Type.Results != nil {
					ress := fdecl.Type.Results.List
					for _, res := range ress {
						for _, rname := range res.Names {
							elm.rets = append(elm.rets, nameAndType{name: rname.Name, typ: res.Type.(*ast.Ident).Name})
						}
					}
				}

				fncs = append(fncs, elm)
			}
		}
	}
	return true
}

type inspector func(ast.Node) bool

func (f inspector) Visit(node ast.Node) ast.Visitor {
	if f(node) {
		return f
	}
	return nil
}

func inspect() {
	src := `
    package main

    var i = 0
    func hoge(i,j int, f float) (s string) {
      return s
    }

    func main() {
	  println("Hello, World!")
    }`

	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "", src, 0)
	if err != nil {
		panic(err)
	}
	ast.Walk(inspector(funcInspect), f)
	//ast.Inspect(f, funcInspect)
}

func TestInspect() {
	inspect()
	for _, f := range fncs {
		fmt.Println(f)
	}
}
