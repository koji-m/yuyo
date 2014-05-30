package yuyocore

import (
	"fmt"
	"os"
)

/*----------------------------------------------------
 * procedure
 */
type procedure struct {
	typ  int                //1-999: pre-compiled procedure, 1000-: Go function
	bdy  func(...yObj) yObj //Go funtion
	ret  *insn              //return code for Go function
	pcc  *insn              //pre-compiled code
	reqc int                //num of required arguments
	optc int                //num of optional arguments
	locc int
}

func (self *procedure) extRepr() string {
	return "#<procedure>"
}

/*----------------------------------------------------
 * procedure identifier
 */
const (
	tLSS = iota
	tCONS
	tCAR
	tCDR
	tLIST
	tCALLWITHCC
)

const (
	tGOPROC = iota + 1000
	tADD
	tSUB
	tPAIRP
	tNULLP
	tMEMV
	tLST2VEC
	tLENGTH
	tAPPEND
	tNUMEQ
	tEQV
)

/*----------------------------------------------------
 * pre-compiled codes
 */

/** pccADD
*(define (ADD lst sum)
*  (if (null? lst)
*      sum
*      (ADD (cdr lst) (+ (car lst) sum))))
**/
/** pccSUB **/

/** pccLSS **/
var pccLSS = &insn{op: opLSS, i0: &insn{op: opRETURN, n0: 1}}

/** pccCONS **/
var pccCONS = &insn{op: opCONSPROC, i0: &insn{op: opRETURN, n0: 2}}

/** pccCAR **/
var pccCAR = &insn{op: opCARPROC, i0: &insn{op: opRETURN, n0: 1}}

/** pccCDR **/
var pccCDR = &insn{op: opCDRPROC, i0: &insn{op: opRETURN, n0: 1}}

/** pccLIST **/
var pccLIST = &insn{op: opPOP, i0: &insn{op: opRETURN, n0: 0}}

/** pccCALLWITHCC **/
var pccCALLWITHCC = &insn{op: opCONTI}

/*----------------------------------------------------
 * pre-compiled go func
 */

/** goAdd **/
func goAdd(args ...yObj) yObj {
	var (
		sumt int64 = tIntnum
		isum int64
		fsum float64
		rsum rational
		csum complex128
	)

	args0 := args[0]
	l := length(args0)
	if l == 0 {
		return yZERO
	}
	if l == 1 {
		return pair(args0).car
	}
	p := args0
	for i := 0; i < l; i++ {
		val := pair(p).car
		typ := typeOf(val)

		switch typ {
		case tIntnum:
			switch sumt {
			case tIntnum:
				isum += intnumVal(val)
			case tFlonum:
				fsum += float64(intnumVal(val))
			case tRatinum:
				rsum = addRatInt(rsum, intnumVal(val))
			case tCmplnum:
				csum = complex(real(csum)+float64(intnumVal(val)), imag(csum))
			}
		case tFlonum:
			switch sumt {
			case tIntnum:
				fsum = float64(isum) + flonumVal(val)
				sumt = tFlonum
			case tFlonum:
				fsum += flonumVal(val)
			case tRatinum:
				fsum = addRatFlo(rsum, flonumVal(val))
				sumt = tFlonum
			case tCmplnum:
				csum = complex(real(csum)+flonumVal(val), imag(csum))
				sumt = tCmplnum
			}
		case tRatinum:
			switch sumt {
			case tIntnum:
				rsum = addRatInt(ratinumVal(val), isum)
				sumt = tRatinum
			case tFlonum:
				fsum = addRatFlo(ratinumVal(val), fsum)
			case tRatinum:
				rsum = addRatRat(rsum, ratinumVal(val))
			case tCmplnum:
				csum = complex(addRatFlo(ratinumVal(val), real(csum)), imag(csum))
			}
		case tCmplnum:
			cmplx := cmplnumVal(val)
			switch sumt {
			case tIntnum:
				csum = complex(real(cmplx)+float64(isum), imag(cmplx))
				sumt = tCmplnum
			case tFlonum:
				csum = complex(real(cmplx)+fsum, imag(cmplx))
				sumt = tCmplnum
			case tRatinum:
				csum = complex(addRatFlo(rsum, real(cmplx)), imag(cmplx))
				sumt = tCmplnum
			case tCmplnum:
				csum += cmplx
			}
		}
		p = pair(p).cdr
	}

	switch sumt {
	case tIntnum:
		return mkIntnum(isum)
	case tFlonum:
		return mkFlonum(fsum)
	case tRatinum:
		return mkRatinum(rsum)
	case tCmplnum:
		return mkCmplnum(csum)
	default:
		return mkError(mkYerror(errArgTypeMis))
	}
}

/** goSub **/
func goSub(args ...yObj) yObj {
	var (
		sumt int64 = tIntnum
		isum int64
		fsum float64
		rsum rational
		csum complex128
	)

	args0 := args[0]
	args1 := args[1]
	l := length(args1)
	if l == 0 {
		args1 = mkPair(args0, yNULL)
		args0 = yZERO
		l++
	}
	switch typeOf(args0) {
	case tIntnum:
		isum = intnumVal(args0)
	case tFlonum:
		fsum = flonumVal(args0)
		sumt = tFlonum
	case tRatinum:
		rsum = ratinumVal(args0)
		sumt = tRatinum
	case tCmplnum:
		csum = cmplnumVal(args0)
		sumt = tCmplnum
	}
	p := args1
	for i := 0; i < l; i++ {
		val := pair(p).car
		typ := typeOf(val)

		switch typ {
		case tIntnum:
			switch sumt {
			case tIntnum:
				isum -= intnumVal(val)
			case tFlonum:
				fsum -= float64(intnumVal(val))
			case tRatinum:
				rsum = subRatInt(rsum, intnumVal(val))
			case tCmplnum:
				csum = complex(real(csum)-float64(intnumVal(val)), imag(csum))
			}
		case tFlonum:
			switch sumt {
			case tIntnum:
				fsum = float64(isum) - flonumVal(val)
				sumt = tFlonum
			case tFlonum:
				fsum -= flonumVal(val)
			case tRatinum:
				fsum = subRatFlo(rsum, flonumVal(val))
				sumt = tFlonum
			case tCmplnum:
				csum = complex(real(csum)-flonumVal(val), imag(csum))
				sumt = tCmplnum
			}
		case tRatinum:
			switch sumt {
			case tIntnum:
				rsum = subIntRat(isum, ratinumVal(val))
				sumt = tRatinum
			case tFlonum:
				fsum = subFloRat(fsum, ratinumVal(val))
			case tRatinum:
				rsum = subRatRat(rsum, ratinumVal(val))
			case tCmplnum:
				csum = complex(subFloRat(real(csum), ratinumVal(val)), imag(csum))
			}
		case tCmplnum:
			cmplx := cmplnumVal(val)
			switch sumt {
			case tIntnum:
				csum = complex(float64(isum)-real(cmplx), -imag(cmplx))
				sumt = tCmplnum
			case tFlonum:
				csum = complex(fsum-real(cmplx), -imag(cmplx))
				sumt = tCmplnum
			case tRatinum:
				csum = complex(subRatFlo(rsum, real(cmplx)), -imag(cmplx))
				sumt = tCmplnum
			case tCmplnum:
				csum -= cmplx
			}
		}
		p = pair(p).cdr
	}

	switch sumt {
	case tIntnum:
		return mkIntnum(isum)
	case tFlonum:
		return mkFlonum(fsum)
	case tRatinum:
		return mkRatinum(rsum)
	case tCmplnum:
		return mkCmplnum(csum)
	default:
		return mkError(mkYerror(errArgTypeMis))
	}
}

/** goMemv **/
func goMemv(args ...yObj) yObj {
	arg0 := args[0]
	arg1 := args[1]
	if arg1 == yNULL {
		return yFALSE
	}
	if typeOf(arg1) != tPair {
		return yErrArgTypeMis
	}
	lstCdr := arg1
	for {
		val := pair(lstCdr).car
		if scmEqv(arg0, val) {
			return lstCdr
		}
		if pair(lstCdr).cdr == yNULL {
			return yFALSE
		}
		lstCdr = pair(lstCdr).cdr
		if typeOf(lstCdr) != tPair {
			return yFALSE
		}
	}
}

/** goLst2vec **/
func goLst2vec(args ...yObj) yObj {
	return list2vector(args[0])
}

/** goPairp **/
func goPairp(args ...yObj) yObj {
	if typeOf(args[0]) == tPair {
		return yTRUE
	}
	return yFALSE
}

/** goNullp **/
func goNullp(args ...yObj) yObj {
	if args[0] == yNULL {
		return yTRUE
	}
	return yFALSE
}

/** goSetCar **/
func goSetCar(args ...yObj) yObj {
	p := args[0]
	if typeOf(p) != tPair {
		return mkError(mkYerror(errArgTypeMis, "(set-car! "+extRepr(p)+" "+extRepr(args[1])+")"))
	}
	pair(p).car = args[1]

	return yUNDEFINED
}

/** goSetCdr **/
func goSetCdr(args ...yObj) yObj {
	p := args[0]
	if typeOf(p) != tPair {
		return mkError(mkYerror(errArgTypeMis, "(set-cdr! "+extRepr(p)+" "+extRepr(args[1])+")"))
	}
	pair(p).cdr = args[1]

	return yUNDEFINED
}

/** goListp **/
func goListp(args ...yObj) yObj {
	if listTypeOf(args[0]) == ltProp {
		return yTRUE
	}
	return yFALSE
}

/** goLength **/
func goLength(args ...yObj) yObj {
	return listLength(args[0])
}

/** goAppend **/
func goAppend(args ...yObj) yObj {
	var err error
	l := length(args[0])
	res := yNULL
	var p yObj
	if typeOf(args[0]) == tPair {
		p = args[0]
	}
	for i := 0; i < l; i++ {
		res, err = lstAppend(res, pair(p).car)
		if err != nil {
			return mkError(err.(yError))
		}
		p = pair(p).cdr
	}
	return res
}

/** goReverse **/
func goReverse(args ...yObj) yObj {
	lst := args[0]
	if lst == yNULL {
		return yNULL
	}
	if typeOf(lst) != tPair || listTypeOf(lst) != ltProp {
		return mkError(mkYerror(errListReq, extern(lst)))
	}
	res := yNULL
	for {
		res = mkPair(pair(lst).car, res)
		lst = pair(lst).cdr
		if lst == yNULL {
			return res
		}
	}
}

/** goNumeq **/
func goNumeq(args ...yObj) yObj {
	l := len(args)
	arg0 := args[0]
	arg1 := args[1]
	if !numEq(arg0, arg1) {
		return yFALSE
	}
	if l == 2 {
		return yTRUE
	}

	argr := args[2]
	for {
		if argr == yNULL {
			return yTRUE
		}
		if !numEq(arg0, pair(argr).car) {
			return yFALSE
		}
		argr = pair(argr).cdr
	}
	return yTRUE
}

/** goExactp **/
func goExactp(args ...yObj) yObj {
	if exactP(args[0]) {
		return yTRUE
	}
	return yFALSE
}

/** goInexactp **/
func goInexactp(args ...yObj) yObj {
	if exactP(args[0]) {
		return yFALSE
	}
	return yTRUE
}

/** goNumberp **/
func goNumberp(args ...yObj) yObj {
	if numP(typeOf(args[0])) {
		return yTRUE
	}
	return yFALSE
}

/** goIntegerp **/
func goIntegerp(args ...yObj) yObj {
	if intnumP(args[0]) {
		return yTRUE
	}
	return yFALSE
}

/** goRationalp **/
func goRationalp(args ...yObj) yObj {
	if ratinumP(args[0]) {
		return yTRUE
	}
	return yFALSE
}

/** goRealp **/
func goRealp(args ...yObj) yObj {
	if flonumP(args[0]) {
		return yTRUE
	}
	return yFALSE
}

/** goComplexp **/
func goComplexp(args ...yObj) yObj {
	if cmplnumP(args[0]) {
		return yTRUE
	}
	return yFALSE
}

/** goChareq **/
func goChareq(args ...yObj) yObj {
	if args[0] == args[1] {
		return yTRUE
	}
	return yFALSE
}

/** goEqv **/
func goEqv(args ...yObj) yObj {
	arg0 := args[0]
	arg1 := args[1]
	typ0 := typeOf(arg0)
	typ1 := typeOf(arg1)

	if numP(typ0) {
		if numP(typ1) {
			if exactP(arg0) == exactP(arg1) {
				if numEq(arg0, arg1) {
					return yTRUE
				}
			}
		}
		return yFALSE
	}

	if arg0 == arg1 {
		return yTRUE
	}
	return yFALSE
}

/** goDisplay **/
func goDisplay(args ...yObj) yObj {
	fmt.Fprint(os.Stdout, objPrint(args[0]))
	return yUNDEFINED
}

/*----------------------------------------------------
 * pre-compiled procedures
 */
var (
	procADD = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tADD, bdy: goAdd, ret: &insn{op: opRETURN, n0: 1},
			reqc: 0, optc: 1})})
	procSUB = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tSUB, bdy: goSub, ret: &insn{op: opRETURN, n0: 2},
			reqc: 1, optc: 1})})
	procLSS = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tLSS, pcc: pccLSS, reqc: 2, optc: 1})})
	procMEMV = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tMEMV, bdy: goMemv, ret: &insn{op: opRETURN, n0: 2},
			reqc: 2, optc: 0})})
	procLST2VEC = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tLST2VEC, bdy: goLst2vec, ret: &insn{op: opRETURN, n0: 1},
			reqc: 1, optc: 0})})
	procPAIRP = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tPAIRP, bdy: goPairp, ret: &insn{op: opRETURN, n0: 1},
			reqc: 1, optc: 0})})
	procNULLP = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tNULLP, bdy: goNullp, ret: &insn{op: opRETURN, n0: 1},
			reqc: 1, optc: 0})})
	procCONS = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tCONS, pcc: pccCONS, reqc: 2, optc: 0})})
	procCAR = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tCAR, pcc: pccCAR, reqc: 1, optc: 0})})
	procCDR = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tCDR, pcc: pccCDR, reqc: 1, optc: 0})})
	procSETCAR = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tGOPROC, bdy: goSetCar, ret: &insn{op: opRETURN, n0: 2},
			reqc: 2, optc: 0})})
	procSETCDR = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tGOPROC, bdy: goSetCdr, ret: &insn{op: opRETURN, n0: 2},
			reqc: 2, optc: 0})})
	procLIST = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tLIST, pcc: pccLIST, reqc: 0, optc: 1})})
	procLISTP = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tGOPROC, bdy: goListp, ret: &insn{op: opRETURN, n0: 1},
			reqc: 1, optc: 0})})
	procLENGTH = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tLENGTH, bdy: goLength, ret: &insn{op: opRETURN, n0: 1},
			reqc: 1, optc: 0})})
	procAPPEND = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tAPPEND, bdy: goAppend, ret: &insn{op: opRETURN, n0: 1},
			reqc: 0, optc: 1})})
	procREVERSE = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tGOPROC, bdy: goReverse, ret: &insn{op: opRETURN, n0: 1},
			reqc: 1, optc: 0})})
	procNUMEQ = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tNUMEQ, bdy: goNumeq, ret: &insn{op: opRETURN, n0: 3},
			reqc: 2, optc: 1})})
	procEQV = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tEQV, bdy: goEqv, ret: &insn{op: opRETURN, n0: 2},
			reqc: 2, optc: 0})})
	procCHAREQ = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tGOPROC, bdy: goChareq, ret: &insn{op: opRETURN, n0: 2},
			reqc: 2, optc: 0})})
	procEXACTP = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tGOPROC, bdy: goExactp, ret: &insn{op: opRETURN, n0: 1},
			reqc: 1, optc: 0})})
	procINEXACTP = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tGOPROC, bdy: goInexactp, ret: &insn{op: opRETURN, n0: 1},
			reqc: 1, optc: 0})})
	procNUMBERP = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tGOPROC, bdy: goNumberp, ret: &insn{op: opRETURN, n0: 1},
			reqc: 1, optc: 0})})
	procINTEGERP = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tGOPROC, bdy: goIntegerp, ret: &insn{op: opRETURN, n0: 1},
			reqc: 1, optc: 0})})
	procRATIONALP = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tGOPROC, bdy: goRationalp, ret: &insn{op: opRETURN, n0: 1},
			reqc: 1, optc: 0})})
	procREALP = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tGOPROC, bdy: goRealp, ret: &insn{op: opRETURN, n0: 1},
			reqc: 1, optc: 0})})
	procCOMPLEXP = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tGOPROC, bdy: goComplexp, ret: &insn{op: opRETURN, n0: 1},
			reqc: 1, optc: 0})})
	procCALLWITHCC = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tCALLWITHCC, pcc: pccCALLWITHCC, reqc: 1, optc: 0})})
	procDISPLAY = yObj(&yCell{car: yObj(uintptr(tProc)),
		cdr: yObj(&procedure{typ: tGOPROC, bdy: goDisplay, ret: &insn{op: opRETURN, n0: 1},
			reqc: 1, optc: 0})})
)
