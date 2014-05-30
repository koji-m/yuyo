package main

import (
	"bufio"
	"fmt"
	"github.com/koji-m/yuyo/yuyocore"
	"io/ioutil"
	"os"
	"regexp"
)

var commandhelp = `
  yuyo command usage
  
  1.To run REPL.
    $yuyo
  2.To run scheme program file.
    $yuyo <filename>
  3.To execute other operation.
    $yuyo <command> [argumets]
  
  <command>
  version    print yuyo version
  run        run yuyo program (not implemented yet)
  shell      run REPL (same as $yuyo)
  makelib    (not implemented yet)`

func main() {
	args := os.Args
	argc := len(args)

	if argc > 1 {
		validFname := regexp.MustCompile(`.+[.]scm$`)
		if validFname.MatchString(args[1]) {
			if argc != 2 {
				fmt.Println("too much arguments")
				return
			}
			src, err := ioutil.ReadFile(args[1])
			if err != nil {
				fmt.Println(err.Error())
				return
			}
			prt := yuyocore.NewPort(string(src))
			m := yuyocore.Init()
			_, err = yuyocore.ReadEvalFile(prt, m)
			if err != nil {
				fmt.Println(err.Error())
				return
			}
			return
		}
		switch args[1] {
		case "version":
			fmt.Println("yuyo version 0.0.1.a")
		case "run":
			fmt.Println("not implemented yet")
		case "shell":
			goto REPL
		case "makelib":
			fmt.Println("not implemented yet")
		default:
			fmt.Println(commandhelp)

		}
		return
	}

REPL:
	rdr := bufio.NewReader(os.Stdin)
	yuyocore.ReadEvalPrintLoop(rdr)
}
