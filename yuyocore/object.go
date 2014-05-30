package yuyocore

import (
	"strconv"
	"unsafe"
)

type ptr unsafe.Pointer

/*----------------------------------------------------
 * type value
 *
 *  type constant definition
 *  0   - 99 : able to be in car of pair
 *  100 -    : unable to be in car of pair
 *
 */

const (
	tIntnum   int64 = 1
	tConstant int64 = 3
	tChar     int64 = 5
	tString   int64 = 7
	tSymbol   int64 = 15
	tVector   int64 = 23
	tFlonum   int64 = 31
	tRatinum  int64 = 39
	tCmplnum  int64 = 47
	tClosure  int64 = 55
	tProc     int64 = 63
	tInsn     int64 = 71
	tError    int64 = 79

	tSyntaxQuote           = 111
	tSyntaxLambda          = 119
	tSyntaxLet             = 127
	tSyntaxLetseq          = 135
	tSyntaxLetrec          = 143
	tSyntaxIf              = 151
	tSyntaxSet             = 159
	tSyntaxDefine          = 167
	tSyntaxBegin           = 175
	tSyntaxCond            = 183
	tSyntaxCase            = 191
	tSyntaxAnd             = 199
	tSyntaxOr              = 207
	tSyntaxDo              = 215
	tSyntaxDelay           = 223
	tSyntaxQuasiquote      = 231
	tSyntaxLetsyntax       = 239
	tSyntaxLetrecsyntax    = 247
	tSyntaxUnquote         = 255
	tSyntaxUnquoteSplicing = 263
	tSyntaxCallwithcc      = 275 // not used.

	tPair int64 = -1 //must be compared with typeOf(obj)
)

func imTypeOf(i yObj) int64 {
	return int64(uintptr(i) & 7)
}

func typeOf(i yObj) int64 {
	objT := imTypeOf(i)
	if objT == 0 { /* cell type */
		cellT := uintptr((*yCell)(i).car) & 7
		if cellT < 7 { /* pair type */
			return tPair
		}
		/* none pair type */
		cellT = uintptr((*yCell)(i).car) & 65535
		return int64(cellT)
	}
	/* none cell type */
	return objT
}

/*----------------------------------------------------
 * rational number type
 */
type rational struct {
	nume  int64
	denom int64
}

/*----------------------------------------------------
 * constant value
 *
 */
const (
	cFALSE int = iota
	cTRUE
	cNULL
	cEOF
	cUNDEFINED
	cUNBOUND
	cERROR
)

var constTable = [7]string{"#f", "#t", "()", "#eof", "#undefined", "#unbound", "#error"}

// yXXX = ptr(uintptr(cXXX<<3+tConstant))
var (
	yFALSE     yObj = yObj(uintptr(3))
	yTRUE      yObj = yObj(uintptr(11))
	yNULL      yObj = yObj(uintptr(19))
	yEOF       yObj = yObj(uintptr(27))
	yUNDEFINED yObj = yObj(uintptr(35))
	yUNBOUND   yObj = yObj(uintptr(43))
	yERROR     yObj = yObj(uintptr(51))
)

var (
	yIntNum0 yObj = yObj(uintptr(1))
	yIntNum1 yObj = yObj(uintptr(9))
)

/*----------------------------------------------------
 * yObj
 *
 *  <type>    <value>
 *  intnum    yObj(uintptr(int) << 3 + tIntnum)  (range: -2^60 <= intnum < 2^60)
 *  constant  yObj(uintptr(int) << 3 + tConstant)
 *  char      yObj(uintptr(rune) << 3 + tChar)
 *  p2cell    yObj(*yCell)
 *
 */
type yObj unsafe.Pointer

var yZERO = yObj(uintptr(0)<<3 + 1)

/*----------------------------------------------------
 * helper functions for yObj
 *
 */
func mkIntnum(val int64) yObj {
	return yObj(uintptr(val)<<3 + 1)
}

func mkConstant(val int64) yObj {
	return yObj(uintptr(val)<<3 + 3)
}

func mkChar(chr rune) yObj {
	return yObj(uintptr(chr)<<3 + 5)
}

func mkP2cell(p *yCell) yObj {
	return yObj(p)
}

func intnumVal(yobj yObj) int64 {
	return int64(uintptr(yobj)) >> 3
}

func constantVal(yobj yObj) int64 {
	return int64(uintptr(yobj)) >> 3
}

func charVal(yobj yObj) rune {
	return rune(uintptr(yobj)) >> 3
}

func charValStr(yobj yObj) string {
	c := charVal(yobj)
	if c == '\n' {
		return "newline"
	}
	if c == ' ' {
		return "space"
	}
	return string(c)
}

func p2cellVal(yobj yObj) *yCell {
	return (*yCell)(yobj)
}

// alias of p2cellVal
func pair(yobj yObj) *yCell {
	return (*yCell)(yobj)
}

func length(yobj yObj) int {
	if yobj == yNULL {
		return 0
	}
	p := (*yCell)(yobj)
	i := 1
	for {
		pCdr := p.cdr
		typ := typeOf(pCdr)
		switch typ {
		case tPair:
			i++
			p = pair(pCdr)
			continue
		case tConstant:
			if pCdr == yNULL {
				return i
			}
			panic("length() require proper list")
		default:
			panic("length() require proper list")
		}
	}
}

type ccRec struct {
	num   int64
	first bool
}

func chkCirculation(obj yObj, cctbl map[yObj]bool, tbl map[yObj]*ccRec) {
	var (
		typ int64
	)

	typ = typeOf(obj)

	switch typ {
	case tPair:
		if _, exist := cctbl[obj]; exist {
			if _, exist := tbl[obj]; exist {
				return
			}
			tbl[obj] = &ccRec{num: int64(len(tbl)), first: true}
			return
		}
		cctbl[obj] = true
		chkCirculation(pair(obj).car, cctbl, tbl)
		chkCirculation(pair(obj).cdr, cctbl, tbl)
	case tVector:
		if _, exist := cctbl[obj]; exist {
			if _, exist := tbl[obj]; exist {
				return
			}
			tbl[obj] = &ccRec{num: int64(len(tbl)), first: true}
			return
		}
		cctbl[obj] = true
		vec := vectorVal(obj)
		for _, v := range vec {
			chkCirculation(v, cctbl, tbl)
		}
	default:
		return
	}
}

func extRepr(yobj yObj) string {
	var (
		obj  yObj = yobj
		typ  int64
		sign string = ""
	)

	typ = typeOf(obj)
	//Deb("extRepr():type:"+strconv.FormatInt(typ,10))
	//if typ == tIntnum {Deb("extRepr():intnum:"+strconv.FormatInt(intnumVal(obj),10))}
	switch typ {
	case tIntnum:
		return strconv.FormatInt(intnumVal(obj), 10)
	case tFlonum:
		return strconv.FormatFloat(flonumVal(obj), 'f', -1, 64)
	case tRatinum:
		ratnum := ratinumVal(obj)
		return strconv.FormatInt(ratnum.nume, 10) + "/" + strconv.FormatInt(ratnum.denom, 10)
	case tCmplnum:
		cmplx := cmplnumVal(obj)
		if imag(cmplx) >= 0 {
			sign = "+"
		}
		return strconv.FormatFloat(real(cmplx), 'f', -1, 64) + sign + strconv.FormatFloat(imag(cmplx), 'f', -1, 64) + "i"
	case tConstant:
		return constTable[int(constantVal(obj))]
	case tChar:
		return "#\\" + charValStr(obj)
	case tString:
		return "\"" + stringVal(obj) + "\""
	case tSymbol:
		return symbolVal(obj)
	case tPair:
		return printList(obj)
	case tVector:
		vec := vectorVal(obj)
		last := len(vec) - 1
		vals := ""
		for idx, elm := range vec {
			vals += extRepr(elm)
			if idx == last {
				break
			}
			vals += " "
		}
		return "#(" + vals + ")"
	case tClosure:
		return "#<closure>"
	case tProc:
		return "#<procedure>"
	case tError:
		return errorVal(obj).Error()
	default:
		return "#error"
	}
}

func extRepr2(yobj yObj, tbl map[yObj]*ccRec) string {
	var (
		obj  yObj = yobj
		typ  int64
		sign string = ""
	)
	typ = typeOf(obj)
	switch typ {
	case tIntnum:
		return strconv.FormatInt(intnumVal(obj), 10)
	case tFlonum:
		return strconv.FormatFloat(flonumVal(obj), 'f', -1, 64)
	case tRatinum:
		ratnum := ratinumVal(obj)
		return strconv.FormatInt(ratnum.nume, 10) + "/" + strconv.FormatInt(ratnum.denom, 10)
	case tCmplnum:
		cmplx := cmplnumVal(obj)
		if imag(cmplx) >= 0 {
			sign = "+"
		}
		return strconv.FormatFloat(real(cmplx), 'f', -1, 64) + sign + strconv.FormatFloat(imag(cmplx), 'f', -1, 64) + "i"
	case tConstant:
		return constTable[int(constantVal(obj))]
	case tChar:
		return "#\\" + charValStr(obj)
	case tString:
		return "\"" + stringVal(obj) + "\""
	case tSymbol:
		return symbolVal(obj)
	case tPair:
		if rec, exist := tbl[obj]; exist {
			if rec.first {
				rec.first = false
				return "#" + strconv.FormatInt(rec.num, 10) + "=" + printList2(obj, tbl)
			}
			return "#" + strconv.FormatInt(rec.num, 10) + "#"
		}
		return printList2(obj, tbl)
	case tVector:
		head := ""
		if rec, exist := tbl[obj]; exist {
			if rec.first {
				rec.first = false
				head = "#" + strconv.FormatInt(rec.num, 10) + "="
			} else {
				return "#" + strconv.FormatInt(rec.num, 10) + "#"
			}
		}
		vec := vectorVal(obj)
		last := len(vec) - 1
		vals := ""
		for idx, elm := range vec {
			vals += extRepr2(elm, tbl)
			if idx == last {
				break
			}
			vals += " "
		}
		return head + "#(" + vals + ")"
	case tClosure:
		return "#<closure>"
	case tProc:
		return "#<procedure>"
	case tSyntaxQuote:
		return "#<syntax quote>"
	case tSyntaxLambda:
		return "#<syntax lambda>"
	case tSyntaxLet:
		return "#<syntax let>"
	case tSyntaxLetseq:
		return "#<syntax let*>"
	case tSyntaxLetrec:
		return "#<syntax letrec>"
	case tSyntaxIf:
		return "#<syntax if>"
	case tSyntaxSet:
		return "#<syntax set!>"
	case tSyntaxDefine:
		return "#<syntax define>"
	case tSyntaxBegin:
		return "#<syntax begin>"
	case tSyntaxCond:
		return "#<syntax cond>"
	case tSyntaxCase:
		return "#<syntax case>"
	case tSyntaxDo:
		return "#<syntax do>"
	case tSyntaxAnd:
		return "#<syntax and>"
	case tSyntaxOr:
		return "#<syntax or>"
	case tSyntaxQuasiquote:
		return "#<syntax quasiquote>"
	case tSyntaxUnquote:
		return "#<syntax unquote>"
	case tSyntaxUnquoteSplicing:
		return "#<syntax unquote-splicing>"
	case tError:
		return errorVal(obj).Error()
	default:
		return "#error"
	}
}

func extern(obj yObj) string {
	cctbl := map[yObj]bool{}
	tbl := map[yObj]*ccRec{}
	chkCirculation(obj, cctbl, tbl)
	return extRepr2(obj, tbl)
}

func String(yobj yObj) string {
	return extRepr(yobj)
}

func mkList(yobjs []yObj) yObj {
	root := yNULL
	last := yNULL
	for i := 0; i < len(yobjs); i++ {
		root, last = scmAppend(root, last, yobjs[i])
	}
	return root
}

func printList(yobj yObj) string {
	typ := typeOf(pair(yobj).cdr)
	//Deb("printList():type of car:"+strconv.FormatInt(typeOf(pair(yobj).car),10))
	//Deb("printList():type of cdr:"+strconv.FormatInt(typ,10))
	switch typ {
	case tConstant:
		if pair(yobj).cdr == yNULL {
			return "(" + extRepr(pair(yobj).car) + ")"
		} else {
			return "(" + extRepr(pair(yobj).car) + " . " + extRepr(pair(yobj).cdr) + ")"
		}

	case tPair:
		repr := "(" + extRepr(pair(yobj).car)

	LIST_LOOP:
		for last := yobj; ; {
			next := pair(last).cdr
			repr += (" " + extRepr(pair(next).car))

			dr := pair(next).cdr
			t := typeOf(dr)
			switch t {
			case tPair:
				last = next
				continue
			case tConstant:
				if dr == yNULL {
					repr += ")"
					break LIST_LOOP
				} else {
					repr += (" . " + constTable[int(constantVal(dr))] + ")")
					break LIST_LOOP
				}
			default:
				repr += (" . " + extRepr(dr) + ")")
				break LIST_LOOP
			}
		}
		return repr
	}

	return "(" + extRepr(pair(yobj).car) + " . " + extRepr(pair(yobj).cdr) + ")"

}

func printList2(yobj yObj, tbl map[yObj]*ccRec) string {
	typ := typeOf(pair(yobj).cdr)
	switch typ {
	case tConstant:
		if pair(yobj).cdr == yNULL {
			return "(" + extRepr2(pair(yobj).car, tbl) + ")"
		} else {
			return "(" + extRepr2(pair(yobj).car, tbl) + " . " + extRepr2(pair(yobj).cdr, tbl) + ")"
		}

	case tPair:
		repr := "(" + extRepr2(pair(yobj).car, tbl)
	LIST_LOOP:
		for last := yobj; ; {
			next := pair(last).cdr
			if rec, exist := tbl[next]; exist {
				if rec.first {
					rec.first = false
					repr += " . #" + strconv.FormatInt(rec.num, 10) + "=" + printList2(next, tbl)
				} else {
					repr += " . #" + strconv.FormatInt(rec.num, 10) + "#"
				}
				return repr + ")"
			}
			repr += (" " + extRepr2(pair(next).car, tbl))

			dr := pair(next).cdr
			t := typeOf(dr)
			switch t {
			case tPair:
				last = next
				continue
			case tConstant:
				if dr == yNULL {
					repr += ")"
					break LIST_LOOP
				} else {
					repr += (" . " + constTable[int(constantVal(dr))] + ")")
					break LIST_LOOP
				}
			default:
				repr += (" . " + extRepr2(dr, tbl) + ")")
				break LIST_LOOP
			}
		}
		return repr
	}

	return "(" + extRepr2(pair(yobj).car, tbl) + " . " + extRepr2(pair(yobj).cdr, tbl) + ")"

}

/*----------------------------------------------------
 * symbolTable
 */
var symbolTable = make(map[string]yObj)

/*----------------------------------------------------
 * symbol
 */
type symbol struct {
	name  string
	intrn bool
}

/*----------------------------------------------------
 * yCell
 *  <type>      <car>                    <cdr>
 *  pair        yObj                     yObj
 *  string      yObj(uintptr(tString))   yObj(*string)
 *  symbol      yObj(uintptr(tSymbol))   yObj(*symbol)
 *  vector      yObj(uintptr(tVector))   yObj(*[]yObj)
 *  flonum      yObj(uintptr(tFlonum))   yObj(*float64)
 *  ratinum     yObj(uintptr(tRatinum))  yObj(*rational)
 *  cmplnum     yObj(uintptr(tCmplnum))  yObj(*complex128)
 *  closure     yObj(uintptr(tClosure))  yObj(*closure)
 *  procedure   yObj(uintptr(tProc))     yObj(*proc)
 *  insn        yObj(uintptr(tInsn))     yObj(*insn)
 *  error       yObj(uintptr(tError))    yObj(*yError)
 */
type yCell struct {
	car yObj
	cdr yObj
}

var (
	typeStr yObj = yObj(uintptr(tString))
	typeSym yObj = yObj(uintptr(tSymbol))
	typeVec yObj = yObj(uintptr(tVector))
	typeFlo yObj = yObj(uintptr(tFlonum))
	typeRat yObj = yObj(uintptr(tRatinum))
	typeCmp yObj = yObj(uintptr(tCmplnum))
	typeClo yObj = yObj(uintptr(tClosure))
	typePrc yObj = yObj(uintptr(tProc))
	typeIns yObj = yObj(uintptr(tInsn))
	typeErr yObj = yObj(uintptr(tError))
)

/*----------------------------------------------------
 * helper functions for yCell
 *
 */
func mkCell(arVal yObj, drVal yObj) *yCell {
	return &yCell{car: arVal, cdr: drVal}
}

func mkPair(arVal yObj, drVal yObj) yObj {
	return yObj(&yCell{car: arVal, cdr: drVal})
}

func mkString(str string) yObj {
	return yObj(&yCell{car: typeStr, cdr: yObj(&str)})
}

func mkSymbol(name string) yObj {
	sym, exist := symbolTable[name]
	if exist {
		return sym
	}
	sym = yObj(&yCell{car: typeSym, cdr: yObj(&symbol{name: name, intrn: true})})
	symbolTable[name] = sym
	return sym
}

func mkUninternedSym(name string) yObj {
	return yObj(&yCell{car: typeSym, cdr: yObj(&symbol{name: name})})
}

func mkVector(vec []yObj) yObj {
	return yObj(&yCell{car: typeVec, cdr: yObj(&vec)})
}

func vectorVal(yobj yObj) []yObj {
	return *(*[]yObj)((*yCell)(yobj).cdr)
}

func cellVal(ycell *yCell) (yObj, yObj) {
	return ycell.car, ycell.cdr
}

func stringVal(yobj yObj) string {
	return *(*string)((*yCell)(yobj).cdr)
}

func symbolVal(yobj yObj) string {
	sym := (*symbol)((*yCell)(yobj).cdr)
	if sym.intrn {
		return sym.name
	}
	return "#:" + sym.name
}

func flonumVal(yobj yObj) float64 {
	return *(*float64)((*yCell)(yobj).cdr)
}

func mkFlonum(flonum float64) yObj {
	return yObj(&yCell{car: typeFlo, cdr: yObj(&flonum)})
}

func mkRatinum(ratnum rational) yObj {
	return yObj(&yCell{car: typeRat, cdr: yObj(&ratnum)})
}

func ratinumVal(yobj yObj) rational {
	return *(*rational)((*yCell)(yobj).cdr)
}

func mkCmplnum(cmplx complex128) yObj {
	return yObj(&yCell{car: typeCmp, cdr: yObj(&cmplx)})
}

func cmplnumVal(yobj yObj) complex128 {
	return *(*complex128)((*yCell)(yobj).cdr)
}

func mkError(err yError) yObj {
	return yObj(&yCell{car: typeErr, cdr: yObj(&err)})
}

func errorVal(yobj yObj) yError {
	return *(*yError)((*yCell)(yobj).cdr)
}

/*----------------------------------------------------
 * helper functions
 *
 */
func scmEqv(obj1 yObj, obj2 yObj) bool {
	return obj1 == obj2
}

func yNumNegative(ynum yObj) (yObj, error) {
	typ := typeOf(ynum)

	switch typ {
	case tIntnum:
		return mkIntnum(-1 * intnumVal(ynum)), nil
	case tFlonum:
		return mkFlonum(-1 * flonumVal(ynum)), nil
	case tRatinum:
		rnum := ratinumVal(ynum)
		rnum.nume *= -1
		return mkRatinum(rnum), nil
	default:
		return nil, mkYerror(errRead, errBadNumber)
	}
}

func constCmplnum(ynum1 yObj, ynum2 yObj, sign1 int, sign2 int) (yObj, error) {
	var rpart, ipart float64
	typ1 := typeOf(ynum1)
	typ2 := typeOf(ynum2)

	switch typ1 {
	case tIntnum:
		rpart = float64(sign1) * float64(intnumVal(ynum1))
	case tFlonum:
		rpart = float64(sign1) * flonumVal(ynum1)
	case tRatinum:
		rnum := ratinumVal(ynum1)
		rpart = float64(sign1) * float64(rnum.nume) / float64(rnum.denom)
	default:
		return nil, mkYerror(errRead, errBadNumber)
	}

	switch typ2 {
	case tIntnum:
		ipart = float64(sign2) * float64(intnumVal(ynum2))
	case tFlonum:
		ipart = float64(sign2) * flonumVal(ynum2)
	case tRatinum:
		rnum := ratinumVal(ynum2)
		ipart = float64(sign2) * float64(rnum.nume) / float64(rnum.denom)
	default:
		return nil, mkYerror(errRead, errBadNumber)
	}

	return mkCmplnum(complex(rpart, ipart)), nil
}
