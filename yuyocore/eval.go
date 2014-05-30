package yuyocore

import (
	"bufio"
	"fmt"
)

func ReadEvalPrintLoop(rdr *bufio.Reader) {
	m := newVM(topEnv)

	for {
		fmt.Print("yuyo> ")
		rcvStr := ""
		for {
			str, err := rdr.ReadString('\n')
			if err != nil {
				fmt.Println()
				return //maybe EOF, before '\n'
			}
			rcvStr += str
			retObj, readDone := readEval(rcvStr, m)
			if readDone {
				if retObj == yEOF {
					break
				}
				fmt.Println(extern(retObj))
				break
			}
			fmt.Print("      ")
		}
	}
}

func readEval(str string, m *vm) (yObj, bool) {
	obj, err := readExp(NewPort(str), nullFlgSyms)
	if err != nil {
		yErr := err.(yError)
		if yErr.desc[1] == errIncompExp {
			return nil, false
		}
		return mkError(yErr), true
	}

	cc, err := compile(obj, locTable{}, m, 0, &insn{op: opHALT})
	//cc.disasm2(0, "main", true, map[*insn]int{})
	if err != nil {
		return mkError(err.(yError)), true
	}
	return m.exec(cc), true
}

func ReadEvalFile(prt *port, m *vm) (yObj, error) {
	res := yEOF
	for {
		exp, err := readExp(prt, nullFlgSyms)
		if err != nil {
			return nil, err
		}
		if exp == yEOF {
			return res, nil
		}

		cc, err := compile(exp, locTable{}, m, 0, &insn{op: opHALT})
		if err != nil {
			return nil, err
		}
		//cc.disasm2(0, "main", true, map[*insn]int{})
		res = m.exec(cc)
	}
}
