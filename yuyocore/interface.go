package yuyocore

import (
	"errors"
	"strconv"
)

/*
<example>

inum := 2
fnum := 3.14
res, err := yuyo.Run("(let ((x #:i) (y #:f)) (+ x y))", inum, fnum)
if err != nil {
  fmt.Println(err.Error())
}
fmt.Println("result is", res.(float64))

<special symbols>
#:i  int64
#:f  float64
#:c  rune
#:s  string
#:b  bool

*/

func Run(src string, parms ...interface{}) (interface{}, error) {
	flgSyms := make(map[string]yObj)

	for i, parm := range parms {
		idx := strconv.Itoa(i)
		switch x := parm.(type) {
		case int64:
			flgSyms["#:i"+idx] = mkIntnum(x)
		case float64:
			flgSyms["#:f"+idx] = mkFlonum(x)
		case rune:
			flgSyms["#:c"+idx] = mkChar(x)
		case string:
			flgSyms["#:s"+idx] = mkString(x)
		case bool:
			if x {
				flgSyms["#:b"+idx] = yTRUE
			}
			flgSyms["#:b"+idx] = yFALSE
		default:
			return nil, errors.New("unsupported type")
		}
	}

	exp, err := readExpWithFlags(src, flgSyms)
	if err != nil {
		return nil, err
	}

	m := newVM(topEnv)
	cc, err := compile(exp, locTable{}, m, 0, &insn{op: opHALT})
	if err != nil {
		return nil, err
	}
	res := m.exec(cc)

	typ := typeOf(res)
	switch typ {
	case tIntnum:
		return intnumVal(res), nil
	case tFlonum:
		return flonumVal(res), nil
	case tChar:
		return charVal(res), nil
	case tString:
		return stringVal(res), nil
	case tConstant:
		if res == yTRUE {
			return true, nil
		}
		if res == yFALSE {
			return false, nil
		}
		return nil, errors.New("unsuported type")
	}

	return nil, errors.New("unsuported type")
}

func Init() *vm {
	return newVM(topEnv)
}


var (
  IntnumVal = intnumVal
  FlonumVal = flonumVal
  RatinumVal = ratinumVal
  CmplnumVal = cmplnumVal
  SymbolVal = symbolVal
  StringVal = stringVal
  VectorVal = vectorVal
  CharVal = charVal
  ExtRepr = extern
  ReadEval = readEval
  
)

