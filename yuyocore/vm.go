package yuyocore

import (
	"fmt"
)

/*----------------------------------------------------
 * instruction set
 */
const (
	opHALT = iota
	opREFERGLOBAL
	opREFERLOCAL
	opREFERFREE
	opLOOKUPGLOBAL
	opINDIRECT
	opCONSTANT
	opCLOSE
	opBOX
	opTEST
	opASSIGNLOCAL
	opASSIGNFREE
	opASSIGNGLOBAL
	opLOOKUPASSIGNGLOBAL
	opCONTI
	opNUATE
	opFRAME
	opARGUMENT
	opSHIFT
	opEXTEND
	opSHRINK
	opDEFINE
	opPOP
	opRECOVER
	opPUSHLOCAL
	opAPPLY
	opRETURN
	opPAIRP
	opNULLP
	opCONS
	opCONSPROC
	opCAR
	opCARPROC
	opCDR
	opCDRPROC
	opADD2
	opADD
	opSUB2
	opSUB
	opLSS
	opMEMV
	opAPPEND
	opLIST
	opLENGTH
	opLST2VEC
	opEQN
	opEQV
)

var optable = []string{"halt", "referglobal", "referlocal", "referfree", "lookupglobal", "indirect", "constant",
	"close", "box", "test", "assignlocal", "assignfree", "assignglobal", "lookupassignglobal",
	"conti", "nuate", "frame", "argument", "shift", "extend", "shrink", "define", "pop",
	"recover", "pushlocal", "apply", "return", "pairp", "nullp", "cons", "consproc", "car",
	"carproc", "cdr", "cdrproc", "add2", "add", "sub2", "sub", "lss", "memv", "append", "list",
	"length", "list2vector", "eqn", "eqv"}

type insn struct {
	op int
	n0 int
	n1 int
	i0 *insn
	i1 *insn
	o0 yObj
}

/*----------------------------------------------------
 * closure
 */
type closure struct {
	bdy  *insn
	free []yObj
	reqc int
	optc int
	locc int
}

func (self *closure) extRepr() string {
	return "#<closure>"
}

/*----------------------------------------------------
 * vm
 */
type vm struct {
	acm yObj
	frm int
	arg int
	ccl yObj
	stp int
	stk []yObj
	env map[yObj]*glocRec
}

/*----------------------------------------------------
 * newVM
 */
func newVM(topEnv map[yObj]*glocRec) *vm {
	// init toplevel environment
	stack := make([]yObj, 1000)
	envTable := make(map[yObj]*glocRec)

	for sym, obj := range topEnv {
		envTable[sym] = obj
	}

	return &vm{frm: 0, stp: 0, stk: stack, env: envTable}
}

/*----------------------------------------------------
 * cpStack
 */
func cpStack(stp int, stk []yObj) *[]yObj {
	s := make([]yObj, stp-1)
	for i := 0; i < stp-1; i++ {
		s[i] = stk[i]
	}
	return &s
}

/*----------------------------------------------------
 * recovStack
 */
func (self *vm) recovStack(src []yObj, dst []yObj) int {
	i := 0
	for i < len(src) {
		dst[i] = src[i]
		i++
	}
	return i
}

/*----------------------------------------------------
 * retTmpl
 */
var retTmpl = &insn{op: opRETURN}

/*----------------------------------------------------
 * exec
 */
func (self *vm) exec(cc *insn) yObj {
	var err error
	next := cc
	acum, fram, argp, ccls, stop, stak, genv := self.acm, self.frm, self.arg, self.ccl, self.stp, self.stk, self.env
	for {
		//fmt.Println(next.disasm(map[*insn]string{}))
		//fmt.Println()
		//fmt.Println("stop:", stop, "opcode:", optable[next.op])
		nxtOp := next.op
		//fmt.Println("!!!", optable[nxtOp], "!!!")
		switch nxtOp {
		case opHALT:
			self.frm = fram
			self.ccl = ccls
			self.stp = stop
			return acum
		case opREFERGLOBAL:
			//acum = stak[next.n0]
			acum = *(*yObj)(next.o0)
			next = next.i0
			continue
		case opREFERLOCAL:
			acum = stak[fram+next.n0]
			next = next.i0
			continue
		case opREFERFREE:
			acum = (*closure)((*yCell)(ccls).cdr).free[next.n0]
			next = next.i0
			continue
		case opLOOKUPGLOBAL:
			gRec, ok := genv[next.o0]
			if !ok {
				return mkError(mkYerror(errUnboundVar, extRepr(next.o0)))
			}
			next.op = opREFERGLOBAL
			next.o0 = yObj(gRec.gloc)
			acum = *(*yObj)(gRec.gloc)
			next = next.i0
			continue
		case opINDIRECT:
			acum = *(*yObj)(acum) //error handling needed
			next = next.i0
			continue
		case opCONSTANT:
			acum = next.o0
			next = next.i0
			continue
		case opCLOSE:
			n0 := next.n0
			argc := (*[2]int)(next.o0)
			cl := &closure{bdy: next.i0, free: make([]yObj, n0), reqc: (*argc)[0], optc: (*argc)[1], locc: next.n1}
			for i := 0; i < n0; i++ {
				cl.free[i] = stak[stop-i-1]
			}
			acum = yObj(&yCell{car: yObj(uintptr(tClosure)), cdr: yObj(cl)})
			next = next.i1
			stop -= n0
			continue
		case opBOX:
			tmp := stak[fram+next.n0]
			stak[fram+next.n0] = yObj(&tmp)
			next = next.i0
			continue
		case opTEST:
			if acum == yFALSE {
				next = next.i1
			} else {
				next = next.i0
			}
			continue
		case opASSIGNLOCAL:
			*(*yObj)(stak[fram+next.n0]) = acum
			next = next.i0
			continue
		case opASSIGNFREE:
			*(*yObj)((*closure)((*yCell)(ccls).cdr).free[next.n0]) = acum
			next = next.i0
			continue
		case opASSIGNGLOBAL:
			*(*yObj)(next.o0) = acum
			next = next.i0
			continue
		case opLOOKUPASSIGNGLOBAL:
			gRec, ok := genv[next.o0]
			if !ok {
				return mkError(mkYerror(errUnboundVar, extRepr(next.o0)))
			}
			if gRec.immutable {
				return yErrImmutableVar
			}
			next.op = opASSIGNGLOBAL
			next.o0 = yObj(gRec.gloc)
			*(*yObj)(gRec.gloc) = acum
			next = next.i0
			continue
		case opCONTI:
			fn := stak[fram]
			typ := typeOf(fn)
			if typ == tClosure { //closure
				cl := (*closure)((*yCell)(fn).cdr)
				if cl.reqc != 1 || cl.optc != 0 {
					return yErrNumArg
				}
				next = cl.bdy
			} else if typ == tProc {
				prc := (*procedure)((*yCell)(fn).cdr)
				if prc.reqc != 1 || prc.optc != 0 {
					return yErrNumArg
				}
				next = prc.pcc
			} else {
				return yErrApply
			}
			capStack := cpStack(stop, stak)
			cl := &closure{bdy: &insn{op: opREFERLOCAL, n0: 0,
				i0: &insn{op: opNUATE, o0: yObj(capStack),
					i0: &insn{op: opRETURN, n0: 0}}}, reqc: 1}
			stak[fram] = yObj(&yCell{car: yObj(uintptr(tClosure)), cdr: yObj(cl)})
			continue
		case opNUATE:
			stop = self.recovStack(*(*[]yObj)(next.o0), stak)
			next = next.i0
			continue
		case opFRAME:
			stak[stop] = yObj(ccls)
			stop++
			stak[stop] = yObj(uintptr(argp))
			stop++
			stak[stop] = yObj(uintptr(fram))
			stop++
			stak[stop] = yObj(next.i0)
			stop++
			argp = stop
			next = next.i1
			continue
		case opARGUMENT:
			stak[stop] = acum
			stop++
			next = next.i0
			continue
		case opSHIFT:
			stk := stak
			top := stop
			mov := next.n1
			j := top - mov
			for i := next.n0; i > 0; i-- {
				stk[j-i] = stk[top-i]
			}
			stop -= mov
			next = next.i0
			continue
		case opEXTEND:
			stop += next.n0
			next = next.i0
			continue
		case opSHRINK:
			stop -= next.n0
			next = next.i0
			continue
		case opDEFINE:
			di := (*defInfo)(next.o0)
			genv[di.sym] = &glocRec{false, di.gloc}
			*(*yObj)(di.gloc) = acum
			next = next.i0
			continue
		case opPOP:
			acum = stak[stop-1]
			stop--
			next = next.i0
			continue
		case opRECOVER: //dangerous instruction
			stop += next.n0
			next = next.i0
			continue
		case opPUSHLOCAL:
			stak[fram+next.n0] = acum
			next = next.i0
			continue
		case opAPPLY:
			fn := acum
			argc := stop - argp
			typ := typeOf(fn)
			if typ == tClosure { //closure
				cl := (*closure)((*yCell)(fn).cdr)
				reqc := cl.reqc
				if cl.optc > 0 {
					diff := argc - reqc
					shift := diff - 1
					if diff > 0 {
						stak[argp+reqc] = mkList(stak[argp+reqc : stop])
						stop -= shift
					} else if diff == 0 {
						stak[argp+reqc] = yNULL
						stop++
					} else {
						return yErrNumArg
					}
				} else {
					if argc != reqc {
						return yErrNumArg
					}
				}
				next = cl.bdy
				fram = argp
				ccls = fn
				stop += cl.locc
				continue
			} else if typ == tProc { //procedure
				prc := (*procedure)((*yCell)(fn).cdr)
				reqc := prc.reqc
				if prc.optc > 0 {
					diff := argc - reqc
					shift := diff - 1
					if diff > 0 {
						stak[argp+reqc] = mkList(stak[argp+reqc : stop])
						stop -= shift
					} else if diff == 0 {
						stak[argp+reqc] = yNULL
						stop++
					} else {
						return yErrNumArg
					}
				} else {
					if argc != reqc {
						return yErrNumArg
					}
				}
				if prc.typ > 999 { //Go function
					acum = (func(...yObj) yObj)(prc.bdy)(stak[stop-(reqc+prc.optc) : stop]...)
					next = prc.ret
					continue
				} else { //pre-compiled procedure
					next = prc.pcc
					fram = argp
					stop += prc.locc
					continue
				}
			}
			return yErrApply
		case opRETURN:
			stop -= next.n0
			next = (*insn)(stak[stop-1])
			fram = int(uintptr(stak[stop-2]))
			argp = int(uintptr(stak[stop-3]))
			ccls = (yObj)(stak[stop-4])
			stop -= 4
			continue
		case opPAIRP:
			if typeOf(acum) == tPair {
				acum = yTRUE
			} else {
				acum = yFALSE
			}
			next = next.i0
			continue
		case opNULLP:
			if acum == yNULL {
				acum = yTRUE
			} else {
				acum = yFALSE
			}
			next = next.i0
			continue
		case opCONS:
			acum = mkPair(stak[stop-1], acum)
			stop--
			next = next.i0
			continue
		case opCONSPROC:
			acum = mkPair(stak[stop-2], stak[stop-1])
			next = next.i0
			continue
		case opCAR:
			if typeOf(acum) != tPair {
				return mkError(mkYerror(errArgTypeMis, "(car "+extRepr(acum)+")"))
			}
			acum = pair(acum).car
			next = next.i0
			continue
		case opCARPROC:
			if typeOf(stak[stop-1]) != tPair {
				return mkError(mkYerror(errArgTypeMis, "(car "+extRepr(acum)+")"))
			}
			acum = pair(stak[stop-1]).car
			next = next.i0
			continue
		case opCDR:
			if typeOf(acum) != tPair {
				return mkError(mkYerror(errArgTypeMis, "(cdr "+extRepr(acum)+")"))
			}
			acum = pair(acum).cdr
			next = next.i0
			continue
		case opCDRPROC:
			if typeOf(stak[stop-1]) != tPair {
				return mkError(mkYerror(errArgTypeMis, "(cdr "+extRepr(acum)+")"))
			}
			acum = pair(stak[stop-1]).cdr
			next = next.i0
			continue
		case opADD2:
			val0 := stak[stop-1]
			typ0 := typeOf(val0)
			typ1 := typeOf(acum)
			switch typ0 {
			case tIntnum:
				switch typ1 {
				case tIntnum:
					acum = mkIntnum(intnumVal(val0) + intnumVal(acum))
				case tFlonum:
					acum = mkFlonum(float64(intnumVal(val0)) + flonumVal(acum))
				case tRatinum:
					acum = yAddRatInt(ratinumVal(acum), intnumVal(val0))
				case tCmplnum:
					cmplx := cmplnumVal(acum)
					acum = mkCmplnum(complex(real(cmplx)+float64(intnumVal(val0)), imag(cmplx)))
				default:
					return mkError(mkYerror(errArgTypeMis, "(+", extRepr(stak[stop-1]), extRepr(acum), ")"))
				}
			case tFlonum:
				switch typ1 {
				case tIntnum:
					acum = mkFlonum(flonumVal(val0) + float64(intnumVal(acum)))
				case tFlonum:
					acum = mkFlonum(flonumVal(val0) + flonumVal(acum))
				case tRatinum:
					acum = mkFlonum(addRatFlo(ratinumVal(acum), flonumVal(val0)))
				case tCmplnum:
					cmplx := cmplnumVal(acum)
					acum = mkCmplnum(complex(real(cmplx)+flonumVal(val0), imag(cmplx)))
				default:
					return mkError(mkYerror(errArgTypeMis, "(+", extRepr(stak[stop-1]), extRepr(acum), ")"))
				}
			case tRatinum:
				switch typ1 {
				case tIntnum:
					acum = yAddRatInt(ratinumVal(val0), intnumVal(acum))
				case tFlonum:
					acum = mkFlonum(addRatFlo(ratinumVal(val0), flonumVal(acum)))
				case tRatinum:
					acum = yAddRatRat(ratinumVal(val0), ratinumVal(acum))
				case tCmplnum:
					cmplx := cmplnumVal(acum)
					acum = mkCmplnum(complex(addRatFlo(ratinumVal(val0), real(cmplx)), imag(cmplx)))
				default:
					return mkError(mkYerror(errArgTypeMis, "(+", extRepr(stak[stop-1]), extRepr(acum), ")"))
				}
			case tCmplnum:
				cmplx0 := cmplnumVal(val0)
				switch typ1 {
				case tIntnum:
					acum = mkCmplnum(complex(real(cmplx0)+float64(intnumVal(acum)), imag(cmplx0)))
				case tFlonum:
					acum = mkCmplnum(complex(real(cmplx0)+flonumVal(acum), imag(cmplx0)))
				case tRatinum:
					acum = mkCmplnum(complex(addRatFlo(ratinumVal(acum), real(cmplx0)), imag(cmplx0)))
				case tCmplnum:
					acum = mkCmplnum(cmplx0 + cmplnumVal(acum))
				default:
					return mkError(mkYerror(errArgTypeMis, "(+", extRepr(stak[stop-1]), extRepr(acum), ")"))
				}
			}
			stop--
			next = next.i0
			continue
		case opSUB2:
			val0 := stak[stop-1]
			typ0 := typeOf(val0)
			typ1 := typeOf(acum)
			switch typ0 {
			case tIntnum:
				switch typ1 {
				case tIntnum:
					acum = mkIntnum(intnumVal(val0) - intnumVal(acum))
				case tFlonum:
					acum = mkFlonum(float64(intnumVal(val0)) - flonumVal(acum))
				case tRatinum:
					acum = ySubIntRat(intnumVal(val0), ratinumVal(acum))
				case tCmplnum:
					cmplx := cmplnumVal(acum)
					acum = mkCmplnum(complex(float64(intnumVal(val0))-real(cmplx), -1*imag(cmplx)))
				default:
					return mkError(mkYerror(errArgTypeMis, "(-", extRepr(stak[stop-1]), extRepr(acum), ")"))
				}
			case tFlonum:
				switch typ1 {
				case tIntnum:
					acum = mkFlonum(flonumVal(val0) - float64(intnumVal(acum)))
				case tFlonum:
					acum = mkFlonum(flonumVal(val0) - flonumVal(acum))
				case tRatinum:
					acum = mkFlonum(subFloRat(flonumVal(val0), ratinumVal(acum)))
				case tCmplnum:
					cmplx := cmplnumVal(acum)
					acum = mkCmplnum(complex(flonumVal(val0)-real(cmplx), -1*imag(cmplx)))
				default:
					return mkError(mkYerror(errArgTypeMis, "(-", extRepr(stak[stop-1]), extRepr(acum), ")"))
				}
			case tRatinum:
				switch typ1 {
				case tIntnum:
					acum = ySubRatInt(ratinumVal(val0), intnumVal(acum))
				case tFlonum:
					acum = mkFlonum(subRatFlo(ratinumVal(val0), flonumVal(acum)))
				case tRatinum:
					acum = ySubRatRat(ratinumVal(val0), ratinumVal(acum))
				case tCmplnum:
					cmplx := cmplnumVal(acum)
					acum = mkCmplnum(complex(subRatFlo(ratinumVal(val0), real(cmplx)), -1*imag(cmplx)))
				default:
					return mkError(mkYerror(errArgTypeMis, "(-", extRepr(stak[stop-1]), extRepr(acum), ")"))
				}
			case tCmplnum:
				cmplx0 := cmplnumVal(val0)
				switch typ1 {
				case tIntnum:
					acum = mkCmplnum(complex(real(cmplx0)-float64(intnumVal(acum)), imag(cmplx0)))
				case tFlonum:
					acum = mkCmplnum(complex(real(cmplx0)-flonumVal(acum), imag(cmplx0)))
				case tRatinum:
					acum = mkCmplnum(complex(subFloRat(real(cmplx0), ratinumVal(acum)), imag(cmplx0)))
				case tCmplnum:
					acum = mkCmplnum(cmplx0 - cmplnumVal(acum))
				default:
					return mkError(mkYerror(errArgTypeMis, "(-", extRepr(stak[stop-1]), extRepr(acum), ")"))
				}
			}
			stop--
			next = next.i0
			continue
		case opEQN:
			val0 := stak[stop-1]
			typ0 := typeOf(val0)
			typ1 := typeOf(acum)
			switch typ0 {
			case tIntnum:
				switch typ1 {
				case tIntnum:
					if val0 == acum {
						acum = yTRUE
					} else {
						acum = yFALSE
					}
				case tFlonum:
					if float64(intnumVal(val0)) == flonumVal(acum) {
						acum = yTRUE
					} else {
						acum = yFALSE
					}
				case tRatinum:
					acum = yFALSE
				case tCmplnum:
					cmplx := cmplnumVal(acum)
					if imag(cmplx) != 0 {
						acum = yFALSE
					} else {
						if float64(intnumVal(val0)) == real(cmplx) {
							acum = yTRUE
						} else {
							acum = yFALSE
						}
					}
				}
			case tFlonum:
				switch typ1 {
				case tIntnum:
					if float64(intnumVal(acum)) == flonumVal(val0) {
						acum = yTRUE
					} else {
						acum = yFALSE
					}
				case tFlonum:
					if flonumVal(val0) == flonumVal(acum) {
						acum = yTRUE
					} else {
						acum = yFALSE
					}
				case tRatinum:
					rnum := ratinumVal(acum)
					if flonumVal(val0) == (float64(rnum.nume) / float64(rnum.denom)) {
						acum = yTRUE
					} else {
						acum = yFALSE
					}
				case tCmplnum:
					cmplx := cmplnumVal(acum)
					if imag(cmplx) != 0 {
						acum = yFALSE
					} else {
						if flonumVal(val0) == real(cmplx) {
							acum = yTRUE
						} else {
							acum = yFALSE
						}
					}
				}
			case tRatinum:
				switch typ1 {
				case tIntnum:
					acum = yFALSE
				case tFlonum:
					rnum := ratinumVal(val0)
					if flonumVal(acum) == (float64(rnum.nume) / float64(rnum.denom)) {
						acum = yTRUE
					} else {
						acum = yFALSE
					}
				case tRatinum:
					rnum0 := ratinumVal(val0)
					rnum1 := ratinumVal(acum)
					if rnum0.nume == rnum1.nume && rnum0.denom == rnum1.denom {
						acum = yTRUE
					} else {
						acum = yFALSE
					}
				case tCmplnum:
					rnum := ratinumVal(val0)
					cmplx := cmplnumVal(acum)
					if imag(cmplx) != 0 {
						acum = yFALSE
					} else {
						if real(cmplx) == (float64(rnum.nume) / float64(rnum.denom)) {
							acum = yTRUE
						} else {
							acum = yFALSE
						}
					}
				}
			case tCmplnum:
				switch typ1 {
				case tIntnum:
					cmplx := cmplnumVal(val0)
					if imag(cmplx) != 0 {
						acum = yFALSE
					} else {
						if float64(intnumVal(acum)) == real(cmplx) {
							acum = yTRUE
						} else {
							acum = yFALSE
						}
					}
				case tFlonum:
					cmplx := cmplnumVal(val0)
					if imag(cmplx) != 0 {
						acum = yFALSE
					} else {
						if flonumVal(acum) == real(cmplx) {
							acum = yTRUE
						} else {
							acum = yFALSE
						}
					}
				case tRatinum:
					rnum := ratinumVal(acum)
					cmplx := cmplnumVal(val0)
					if imag(cmplx) != 0 {
						acum = yFALSE
					} else {
						if real(cmplx) == (float64(rnum.nume) / float64(rnum.denom)) {
							acum = yTRUE
						} else {
							acum = yFALSE
						}
					}
				case tCmplnum:
					if cmplnumVal(val0) == cmplnumVal(acum) {
						acum = yTRUE
					} else {
						acum = yFALSE
					}
				}
			}
			stop--
			next = next.i0
			continue
		case opLSS:
			if typeOf(stak[stop-1]) == tIntnum {
				if typeOf(acum) == tIntnum {
					if intnumVal(stak[stop-1]) < intnumVal(acum) {
						acum = yTRUE
					} else {
						acum = yFALSE
					}
				} else if typeOf(acum) == tFlonum {
					if float64(intnumVal(stak[stop-1])) < flonumVal(acum) {
						acum = yTRUE
					} else {
						acum = yFALSE
					}
				}
			} else if typeOf(stak[stop-1]) == tFlonum {
				if typeOf(acum) == tFlonum {
					if flonumVal(stak[stop-1]) < flonumVal(acum) {
						acum = yTRUE
					} else {
						acum = yFALSE
					}
				} else if typeOf(acum) == tIntnum {
					if flonumVal(stak[stop-1]) < float64(intnumVal(acum)) {
						acum = yTRUE
					} else {
						acum = yFALSE
					}
				}
			} else {
				return yErrArgTypeMis
			}
			stop--
			next = next.i0
			continue
		case opMEMV:
			if acum == yNULL {
				acum = yFALSE
				stop--
				next = next.i0
				continue
			}
			if typeOf(acum) != tPair {
				return yErrArgTypeMis
			}
			obj := stak[stop-1]
			lstCdr := acum
			for {
				val := pair(lstCdr).car
				if scmEqv(obj, val) {
					acum = lstCdr
					break
				}
				if pair(lstCdr).cdr == yNULL {
					acum = yFALSE
					break
				}
				lstCdr = pair(lstCdr).cdr
				if typeOf(lstCdr) != tPair {
					acum = yFALSE
					break
				}
			}
			stop--
			next = next.i0
			continue
		case opAPPEND:
			num := next.n0 - 1
			res := yNULL
			for i := num; i > 0; i-- {
				res, err = lstAppend(res, stak[stop-i])
				if err != nil {
					acum = mkError(err.(yError))
				}
			}
			acum, err = lstAppend(res, acum)
			if err != nil {
				acum = mkError(err.(yError))
			}
			stop -= num
			next = next.i0
			continue
		case opLIST:
			numstk := next.n0 - 1
			acum = mkList2(stak[stop-numstk:stop], acum)
			stop -= numstk
			next = next.i0
			continue
		case opLENGTH:
			acum = listLength(acum)
			next = next.i0
			continue
		case opLST2VEC:
			acum = list2vector(acum)
			next = next.i0
			continue
		case opEQV:
			val0 := stak[stop-1]
			if numP(typeOf(val0)) {
				if numP(typeOf(acum)) {
					if exactP(val0) == exactP(acum) {
						if numEq(val0, acum) {
							acum = yTRUE
						} else {
							acum = yFALSE
						}
					} else {
						acum = yFALSE
					}
				} else {
					acum = yFALSE
				}
			} else {
				if val0 == acum {
					acum = yTRUE
				} else {
					acum = yFALSE
				}
			}
			stop--
			next = next.i0
			continue
		default:
			fmt.Println("invalid state", self.acm, self.frm, self.ccl, self.stk)
			return nil
		}
	}
}
