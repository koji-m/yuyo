package yuyocore

import (
	"fmt"
	"strconv"
)

/*----------------------------------------------------
 * disasm2  --list view
 */
func (code *insn) disasm2(lv int, name string, top bool, iTbl map[*insn]int) {
	op := code.op
	lnum := 0
	ident := ""
	fn := make(map[int]*insn)
	c := code

	for i := 0; i < lv; i++ {
		ident += "  "
	}
	if top {
		fmt.Println("***", name, "***")
	}
LOOP:
	for {
		op = c.op
		lnum++
		if n, exist := iTbl[c]; exist {
			fmt.Println("<jmp", n, ">")
			break LOOP
		}

		iTbl[c] = lnum
		fmt.Print(ident)
		fmt.Print(lnum)
		fmt.Print(" ")
		switch op {
		case opHALT:
			fmt.Println("halt")
			break LOOP
		case opREFERGLOBAL:
			fmt.Println("refer-global", "<gloc>")
			c = c.i0
			continue
		case opREFERLOCAL:
			fmt.Println("refer-local", c.n0)
			c = c.i0
			continue
		case opREFERFREE:
			fmt.Println("refer-free", c.n0)
			c = c.i0
			continue
		case opLOOKUPGLOBAL:
			fmt.Println("lookup-global", extRepr(c.o0))
			c = c.i0
			continue
		case opINDIRECT:
			fmt.Println("indirect")
			c = c.i0
			continue
		case opCONSTANT:
			fmt.Println("constant", extRepr(c.o0))
			c = c.i0
			continue
		case opCLOSE:
			fmt.Println("close", c.n0, "<closure:", lnum, ">")
			fn[lnum] = c.i0
			c = c.i1
			continue
		case opBOX:
			fmt.Println("box", c.n0)
			c = c.i0
			continue
		case opTEST:
			fmt.Println("test")
			fmt.Println("  " + ident + ident + "<then>")
			c.i0.disasm2(lv+1, name, false, iTbl)
			fmt.Println("  " + ident + ident + "<else>")
			c.i1.disasm2(lv+1, name, false, iTbl)
			break LOOP
		case opASSIGNLOCAL:
			fmt.Println("assign-local", c.n0)
			c = c.i0
			continue
		case opASSIGNFREE:
			fmt.Println("assign-free", c.n0)
			c = c.i0
			continue
		case opASSIGNGLOBAL:
			fmt.Println("assign-global", "<gloc>")
			c = c.i0
			continue
		case opLOOKUPASSIGNGLOBAL:
			fmt.Println("lookup-assign-global", extRepr(c.o0))
			c = c.i0
			continue
		case opCONTI:
			fmt.Println("conti")
			c = c.i0
			continue
		case opNUATE:
			fmt.Println("nuate", len(*(*[]yObj)(c.o0)))
			c = c.i0
			continue
		case opFRAME:
			fmt.Println("frame")
			fmt.Println("<return to>")
			c.i0.disasm2(lv+1, name, false, iTbl)
			c = c.i1
			continue
		case opARGUMENT:
			fmt.Println("argument")
			c = c.i0
			continue
		case opSHIFT:
			fmt.Println("shift", c.n0, c.n1)
			c = c.i0
			continue
		case opEXTEND:
			fmt.Println("extend", c.n0)
			c = c.i0
			continue
		case opSHRINK:
			fmt.Println("shrink", c.n0)
			c = c.i0
			continue
		case opDEFINE:
			fmt.Println("define")
			c = c.i0
			continue
		case opPOP:
			fmt.Println("pop", c.n0)
			c = c.i0
			continue
		case opRECOVER:
			fmt.Println("recover", c.n0)
			c = c.i0
			continue
		case opPUSHLOCAL:
			fmt.Println("push-local", c.n0)
			c = c.i0
			continue
		case opAPPLY:
			fmt.Println("apply")
			break LOOP
		case opRETURN:
			fmt.Println("return", c.n0)
			break LOOP
		case opADD2:
			fmt.Println("add2")
			c = c.i0
			continue
		case opADD:
			fmt.Println("add")
			c = c.i0
			continue
		case opSUB2:
			fmt.Println("sub2")
			c = c.i0
			continue
		case opSUB:
			fmt.Println("sub")
			c = c.i0
			continue
		case opLSS:
			fmt.Println("lss")
			c = c.i0
			continue
		case opMEMV:
			fmt.Println("memv")
			c = c.i0
			continue
		case opAPPEND:
			fmt.Println("append")
			c = c.i0
			continue
		case opPAIRP:
			fmt.Println("pairp")
			c = c.i0
			continue
		case opNULLP:
			fmt.Println("nullp")
			c = c.i0
			continue
		case opCONS:
			fmt.Println("cons")
			c = c.i0
			continue
		case opCONSPROC:
			fmt.Println("consproc")
			c = c.i0
			continue
		case opCAR:
			fmt.Println("car")
			c = c.i0
			continue
		case opCARPROC:
			fmt.Println("carproc")
			c = c.i0
			continue
		case opCDR:
			fmt.Println("cdr")
			c = c.i0
			continue
		case opCDRPROC:
			fmt.Println("cdrproc")
			c = c.i0
			continue
		case opLIST:
			fmt.Println("list", c.n0)
			c = c.i0
			continue
		case opLST2VEC:
			fmt.Println("list2vector")
			c = c.i0
			continue
		case opEQN:
			fmt.Println("eqn")
			c = c.i0
			continue
		case opEQV:
			fmt.Println("eqv")
			c = c.i0
			continue
		default:
			fmt.Println("illegal op code")
			break LOOP
		}
	}

	for fnum, ins := range fn {
		ins.disasm2(0, "<closure "+strconv.FormatInt(int64(fnum), 10)+">", true, iTbl)
	}
}

/*----------------------------------------------------
 * disasm  --simple view
 */
func (self *insn) disasm(iTable map[*insn]string) string {
	var (
		op = self.op
	)
	str, exist := iTable[self]
	if exist {
		return str
	}
	switch op {
	case opHALT:
		iTable[self] = "#<REP halt>"
		return "(halt)"
	case opREFERGLOBAL:
		iTable[self] = "#<REP refer-global>"
		return "(refer-global " + "PtoG" + " " + self.i0.disasm(iTable) + ")"
	case opREFERLOCAL:
		iTable[self] = "#<REP refer-local>"
		return "(refer-local " + strconv.FormatInt(int64(self.n0), 10) + " " + self.i0.disasm(iTable) + ")"
	case opREFERFREE:
		iTable[self] = "#<REP refer-free>"
		return "(refer-free " + strconv.FormatInt(int64(self.n0), 10) + " " + self.i0.disasm(iTable) + ")"
	case opLOOKUPGLOBAL:
		iTable[self] = "#<REP lookup-global>"
		return "(lookup-global " + extRepr(self.o0) + " " + self.i0.disasm(iTable) + ")"
	case opINDIRECT:
		iTable[self] = "#<REP indirect>"
		return "(indirect " + self.i0.disasm(iTable) + ")"
	case opCONSTANT:
		iTable[self] = "#<REP constant>"
		return "(constant " + extRepr(self.o0) + " " + self.i0.disasm(iTable) + ")"
	case opCLOSE:
		iTable[self] = "#<REP close>"
		return "(close " + strconv.FormatInt(int64(self.n0), 10) + " " + self.i0.disasm(iTable) + " " + self.i1.disasm(iTable) + ")"
	case opBOX:
		iTable[self] = "#<REP box>"
		return "(box " + strconv.FormatInt(int64(self.n0), 10) + " " + self.i0.disasm(iTable) + ")"
	case opTEST:
		iTable[self] = "#<REP test>"
		return "(test " + self.i0.disasm(iTable) + " " + self.i1.disasm(iTable) + ")"
	case opASSIGNLOCAL:
		iTable[self] = "#<REP assign-local>"
		return "(assign-local " + strconv.FormatInt(int64(self.n0), 10) + " " + self.i0.disasm(iTable) + ")"
	case opASSIGNFREE:
		iTable[self] = "#<REP assign-free>"
		return "(assign-free " + strconv.FormatInt(int64(self.n0), 10) + " " + self.i0.disasm(iTable) + ")"
	case opASSIGNGLOBAL:
		iTable[self] = "#<REP assign-global>"
		return "(assign-global " + "PtoG" + " " + self.i0.disasm(iTable) + ")"
	case opLOOKUPASSIGNGLOBAL:
		iTable[self] = "#<REP lookup-assign-global>"
		return "(lookup-assign-global " + extRepr(self.o0) + " " + self.i0.disasm(iTable) + ")"
	case opCONTI:
		iTable[self] = "#<REP conti>"
		return "(conti " + self.i0.disasm(iTable) + ")"
	case opNUATE:
		iTable[self] = "#<REP nuate>"
		return "(nuate #<stack " + strconv.FormatInt(int64(len(*(*[]yObj)(self.o0))), 10) + "> " + self.i0.disasm(iTable) + ")"
	case opFRAME:
		iTable[self] = "#<REP frame>"
		return "(frame " + self.i0.disasm(iTable) + " " + self.i1.disasm(iTable) + ")"
	case opARGUMENT:
		iTable[self] = "#<REP argument>"
		return "(argument " + self.i0.disasm(iTable) + ")"
	case opSHIFT:
		iTable[self] = "#<REP shift>"
		return "(shift " + strconv.FormatInt(int64(self.n0), 10) + " " + strconv.FormatInt(int64(self.n1), 10) +
			" " + self.i0.disasm(iTable) + ")"
	case opEXTEND:
		iTable[self] = "#<REP extend>"
		return "(extend " + strconv.FormatInt(int64(self.n0), 10) + " " + self.i0.disasm(iTable) + ")"
	case opSHRINK:
		iTable[self] = "#<REP shrink>"
		return "(shrink " + strconv.FormatInt(int64(self.n0), 10) + " " + self.i0.disasm(iTable) + ")"
	case opDEFINE:
		iTable[self] = "#<REP define>"
		return "(define " + self.i0.disasm(iTable) + ")"
	case opPOP:
		iTable[self] = "#<REP pop>"
		return "(pop " + strconv.FormatInt(int64(self.n0), 10) + " " + self.i0.disasm(iTable) + ")"
	case opRECOVER:
		iTable[self] = "#<REP recover>"
		return "(recover " + strconv.FormatInt(int64(self.n0), 10) + " " + self.i0.disasm(iTable) + ")"
	case opPUSHLOCAL:
		iTable[self] = "#<REP push-local>"
		return "(push-local " + strconv.FormatInt(int64(self.n0), 10) + " " + self.i0.disasm(iTable) + ")"
	case opAPPLY:
		iTable[self] = "#<REP apply>"
		return "(apply)"
	case opRETURN:
		iTable[self] = "#<REP return>"
		return "(return " + strconv.FormatInt(int64(self.n0), 10) + ")"
	case opPAIRP:
		iTable[self] = "#<REP pairp>"
		return "(pairp " + self.i0.disasm(iTable) + ")"
	case opNULLP:
		iTable[self] = "#<REP nullp>"
		return "(nullp " + self.i0.disasm(iTable) + ")"
	case opCONS:
		iTable[self] = "#<REP cons>"
		return "(cons " + self.i0.disasm(iTable) + ")"
	case opCONSPROC:
		iTable[self] = "#<REP consproc>"
		return "(consproc " + self.i0.disasm(iTable) + ")"
	case opCAR:
		iTable[self] = "#<REP car>"
		return "(car " + self.i0.disasm(iTable) + ")"
	case opCARPROC:
		iTable[self] = "#<REP carproc>"
		return "(carproc " + self.i0.disasm(iTable) + ")"
	case opCDR:
		iTable[self] = "#<REP cdr>"
		return "(cdr " + self.i0.disasm(iTable) + ")"
	case opCDRPROC:
		iTable[self] = "#<REP cdrproc>"
		return "(cdrproc " + self.i0.disasm(iTable) + ")"
	case opADD2:
		iTable[self] = "#<REP add2>"
		return "(add2 " + self.i0.disasm(iTable) + ")"
	case opADD:
		iTable[self] = "#<REP add>"
		return "(add " + self.i0.disasm(iTable) + ")"
	case opSUB2:
		iTable[self] = "#<REP sub2>"
		return "(sub2 " + self.i0.disasm(iTable) + ")"
	case opSUB:
		iTable[self] = "#<REP sub>"
		return "(sub " + self.i0.disasm(iTable) + ")"
	case opLSS:
		iTable[self] = "#<REP lss>"
		return "(lss " + self.i0.disasm(iTable) + ")"
	case opMEMV:
		iTable[self] = "#<REP memv>"
		return "(memv " + self.i0.disasm(iTable) + ")"
	case opAPPEND:
		iTable[self] = "#<REP append>"
		return "(append " + self.i0.disasm(iTable) + ")"
	case opLIST:
		iTable[self] = "#<REP list>"
		return "(list " + strconv.FormatInt(int64(self.n0), 10) + self.i0.disasm(iTable) + ")"
	case opLENGTH:
		iTable[self] = "#<REP memv>"
		return "(memv " + self.i0.disasm(iTable) + ")"
	case opLST2VEC:
		iTable[self] = "#<REP list2vector>"
		return "(list2vector " + self.i0.disasm(iTable) + ")"
	default:
		return "<ERROR END>"
	}
}
