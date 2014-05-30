package yuyocore

import (
	"strconv"
	"strings"
)

func lstCopy(lst yObj) (root yObj, last yObj) {
	root = mkPair(nil, nil)
	last = nil

	dst := root
	src := lst
	for {
		pair(dst).car = pair(src).car
		srcCdr := pair(src).cdr
		if srcCdr == yNULL {
			pair(dst).cdr = yNULL
			last = dst
			break
		}
		if typeOf(srcCdr) != tPair {
			pair(dst).cdr = srcCdr
			last = dst
			break
		}
		src = srcCdr
		pair(dst).cdr = mkPair(nil, nil)
		dst = pair(dst).cdr
	}
	return root, last
}

func lstAppend(lst yObj, obj yObj) (res yObj, err error) {
	if lst == yNULL {
		return obj, nil
	}
	if typeOf(lst) != tPair {
		return nil, mkYerror(errListReq)
	}

	root, last := lstCopy(lst)
	pair(last).cdr = obj

	return root, nil
}

func mkList2(lst []yObj, obj yObj) (res yObj) {
	res = mkPair(obj, yNULL)
	for i := len(lst) - 1; i > -1; i-- {
		res = mkPair(lst[i], res)
	}
	return res
}

func vector2list(vec yObj) yObj {
	root, last := yNULL, yNULL
	v := vectorVal(vec)
	for _, elm := range v {
		root, last = scmAppend(root, last, elm)
	}
	return root
}

func list2vector(lst yObj) yObj {
	l := lst
	llen := length(l)
	vec := make([]yObj, llen)
	for i := 0; i < llen; i++ {
		vec[i] = pair(l).car
		l = pair(l).cdr
	}
	return mkVector(vec)
}

func addRatInt(rat rational, i int64) rational {
	rnume := rat.nume + (rat.denom * i)
	rdenom := rat.denom

	cd := gcd(rnume, rdenom)
	rnume /= cd
	rdenom /= cd

	return rational{nume: rnume, denom: rdenom}
}

func addRatRat(rat0 rational, rat1 rational) rational {
	rnume := rat0.nume*rat1.denom + rat1.nume*rat0.denom
	rdenom := rat0.denom * rat1.denom

	cd := gcd(rnume, rdenom)
	rnume /= cd
	rdenom /= cd

	return rational{nume: rnume, denom: rdenom}
}

func addRatFlo(rat rational, flo float64) float64 {
	fnum := float64(rat.nume) / float64(rat.denom)
	return fnum + flo
}

func yAddRatInt(rat rational, i int64) yObj {
	rnum := addRatInt(rat, i)

	if rnum.denom == 1 {
		return mkIntnum(rnum.nume)
	}
	return mkRatinum(rnum)
}

func yAddRatRat(rat0 rational, rat1 rational) yObj {
	rnum := addRatRat(rat0, rat1)

	if rnum.denom == 1 {
		return mkIntnum(rnum.nume)
	}
	return mkRatinum(rnum)
}

func subRatInt(rat rational, i int64) rational {
	rnume := rat.nume - (rat.denom * i)
	rdenom := rat.denom

	cd := gcd(rnume, rdenom)
	rnume /= cd
	rdenom /= cd

	return rational{nume: rnume, denom: rdenom}
}

func subIntRat(i int64, rat rational) rational {
	rnume := (rat.denom * i) - rat.nume
	rdenom := rat.denom

	cd := gcd(rnume, rdenom)
	rnume /= cd
	rdenom /= cd

	return rational{nume: rnume, denom: rdenom}
}

func subRatRat(rat0 rational, rat1 rational) rational {
	rnume := rat0.nume*rat1.denom - rat1.nume*rat0.denom
	rdenom := rat0.denom * rat1.denom

	cd := gcd(rnume, rdenom)
	rnume /= cd
	rdenom /= cd

	return rational{nume: rnume, denom: rdenom}
}

func subRatFlo(rat rational, flo float64) float64 {
	fnum := float64(rat.nume) / float64(rat.denom)
	return fnum - flo
}

func subFloRat(flo float64, rat rational) float64 {
	fnum := float64(rat.nume) / float64(rat.denom)
	return flo - fnum
}

func ySubRatInt(rat rational, i int64) yObj {
	rnum := subRatInt(rat, i)

	if rnum.denom == 1 {
		return mkIntnum(rnum.nume)
	}
	return mkRatinum(rnum)
}

func ySubIntRat(i int64, rat rational) yObj {
	rnum := subIntRat(i, rat)

	if rnum.denom == 1 {
		return mkIntnum(rnum.nume)
	}
	return mkRatinum(rnum)
}

func ySubRatRat(rat0 rational, rat1 rational) yObj {
	rnum := subRatRat(rat0, rat1)

	if rnum.denom == 1 {
		return mkIntnum(rnum.nume)
	}
	return mkRatinum(rnum)
}

const (
	ltProp = iota
	ltDot
	ltCirc
)

func listTypeOf(obj yObj) int {
	r, t := obj, obj

	for {
		if typeOf(r) == tPair {
			r = pair(r).cdr
			if typeOf(r) == tPair {
				r = pair(r).cdr
				t = pair(t).cdr
				if r == t {
					return ltCirc
				}
			} else {
				if r == yNULL {
					return ltProp
				}
				return ltDot
			}
		} else {
			if r == yNULL {
				return ltProp
			}
			return ltDot
		}
	}
}

func listLength(obj yObj) yObj {
	if listTypeOf(obj) != ltProp {
		return mkError(mkYerror(errListReq, extern(obj)))
	}
	if obj == yNULL {
		return yZERO
	}
	p := pair(obj).cdr
	var i int64 = 1
	for {
		typ := typeOf(p)
		switch typ {
		case tPair:
			i++
			p = pair(p).cdr
			continue
		case tConstant:
			if p == yNULL {
				return mkIntnum(i)
			}
			return mkError(mkYerror(errListReq, extern(obj)))
		default:
			return mkError(mkYerror(errListReq, extern(obj)))
		}
	}
}

func exactP(obj yObj) bool {
	typ := typeOf(obj)
	if typ == tIntnum || typ == tRatinum {
		return true
	}
	return false
}

func numP(typ int64) bool {
	return typ == tIntnum || typ == tRatinum || typ == tFlonum || typ == tCmplnum
}

func numberP(obj yObj) bool {
	return intnumP(obj) || ratinumP(obj) || flonumP(obj) || cmplnumP(obj)
}

func intnumP(obj yObj) bool {
	return typeOf(obj) == tIntnum
}

func ratinumP(obj yObj) bool {
	return typeOf(obj) == tRatinum
}

func flonumP(obj yObj) bool {
	return typeOf(obj) == tFlonum
}

func cmplnumP(obj yObj) bool {
	return typeOf(obj) == tCmplnum
}

func numEq(val0 yObj, val1 yObj) bool {
	typ0 := typeOf(val0)
	typ1 := typeOf(val1)
	switch typ0 {
	case tIntnum:
		switch typ1 {
		case tIntnum:
			return val0 == val1
		case tFlonum:
			return float64(intnumVal(val0)) == flonumVal(val1)
		case tRatinum:
			return false
		case tCmplnum:
			cmplx := cmplnumVal(val1)
			if imag(cmplx) != 0 {
				return false
			} else {
				return float64(intnumVal(val0)) == real(cmplx)
			}
		}
	case tFlonum:
		switch typ1 {
		case tIntnum:
			return float64(intnumVal(val1)) == flonumVal(val0)
		case tFlonum:
			return flonumVal(val0) == flonumVal(val1)
		case tRatinum:
			rnum := ratinumVal(val1)
			return flonumVal(val0) == (float64(rnum.nume) / float64(rnum.denom))
		case tCmplnum:
			cmplx := cmplnumVal(val1)
			if imag(cmplx) != 0 {
				return false
			} else {
				return flonumVal(val0) == real(cmplx)
			}
		}
	case tRatinum:
		switch typ1 {
		case tIntnum:
			return false
		case tFlonum:
			rnum := ratinumVal(val0)
			return flonumVal(val1) == (float64(rnum.nume) / float64(rnum.denom))
		case tRatinum:
			rnum0 := ratinumVal(val0)
			rnum1 := ratinumVal(val1)
			return rnum0.nume == rnum1.nume && rnum0.denom == rnum1.denom
		case tCmplnum:
			rnum := ratinumVal(val0)
			cmplx := cmplnumVal(val1)
			if imag(cmplx) != 0 {
				return false
			} else {
				return real(cmplx) == (float64(rnum.nume) / float64(rnum.denom))
			}
		}
	case tCmplnum:
		switch typ1 {
		case tIntnum:
			cmplx := cmplnumVal(val0)
			if imag(cmplx) != 0 {
				return false
			} else {
				return float64(intnumVal(val1)) == real(cmplx)
			}
		case tFlonum:
			cmplx := cmplnumVal(val0)
			if imag(cmplx) != 0 {
				return false
			} else {
				return flonumVal(val1) == real(cmplx)
			}
		case tRatinum:
			rnum := ratinumVal(val1)
			cmplx := cmplnumVal(val0)
			if imag(cmplx) != 0 {
				return false
			} else {
				return real(cmplx) == (float64(rnum.nume) / float64(rnum.denom))
			}
		case tCmplnum:
			return cmplnumVal(val0) == cmplnumVal(val1)
		}
	}
	return false
}

func objPrint(yobj yObj) string {
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
		return string(charVal(obj))
	case tString:
		str := strings.Replace(stringVal(obj), "\\\"", "\"", -1)
		str = strings.Replace(str, "\\\\", "\\", -1)
		str = strings.Replace(str, "\\n", "\n", -1)
		str = strings.Replace(str, "\\t", "\t", -1)
		return str
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
