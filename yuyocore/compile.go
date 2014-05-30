package yuyocore

import (
	"fmt"
	"strconv"
)

/*----------------------------------------------------
 * context
 */
const (
	ctxNOTOP      = 1
	ctxINNER      = 2
	ctxTopOuter   = 0
	ctxNotopOuter = 1
	ctxTopInner   = 2
	ctxNotopInner = 3
)

/*----------------------------------------------------
 * locTable
 */
const (
	sLOCAL = false
	sFREE  = true
)

type locTable map[yObj]*varInfo

type varInfo struct {
	index    int
	lorf     bool
	indirect bool
}

func (self locTable) show() {
	for key, val := range self {
		fmt.Println("["+extRepr(key)+"]", val.getval())
	}
}

func (self locTable) numOfLocal() int {
	num := 0
	for _, val := range self {
		if val.lorf == sLOCAL {
			num++
		}
	}
	return num
}

func (self locTable) topOfLocal() int {
	top := -1
	for _, val := range self {
		if val.lorf == sLOCAL {
			if val.index > top {
				top = val.index
			}
		}
	}
	return top
}

func (self *varInfo) getval() string {
	var str string
	str += "{index: " + strconv.FormatInt(int64(self.index), 10) + ", "
	if self.lorf {
		str += "lorf: FREE, "
	} else {
		str += "lorf: LOCAL, "
	}
	if self.indirect {
		str += "INDIRECT, "
	} else {
		str += "DIRECT, "
	}
	return str
}

func (self locTable) reverse() {
	length := len(self) - 1
	for _, info := range self {
		info.index = length - info.index
	}
}

func (self locTable) addToTable(sym yObj, dupchk bool) error {
	if typeOf(sym) != tSymbol {
		return mkYerror(errComp, errSyntax, "symbol required")
	}
	if dupchk {
		if _, exist := self[sym]; exist {
			return mkYerror(errComp, errSyntax, "multiple occurence of same symbol")
		}
	}
	self[sym] = &varInfo{index: len(self)} //as local&direct
	return nil
}

func locTblUnion(tbl1 locTable, tbl2 locTable) locTable {
	newTbl := make(locTable)
	for key, val := range tbl1 {
		newTbl[key] = val
	}
	for key, val := range tbl2 {
		newTbl[key] = val
	}
	return newTbl
}

func locTblExtend(tbl1 locTable, tbl2 locTable) (locTable, locTable) {
	newTbl := make(locTable)
	modTbl := make(locTable)
	ext := tbl1.numOfLocal()
	for key, val := range tbl1 {
		newTbl[key] = &varInfo{index: val.index, lorf: val.lorf, indirect: val.indirect}
	}
	for key, val := range tbl2 {
		x := &varInfo{index: val.index + ext, lorf: val.lorf, indirect: val.indirect}
		modTbl[key] = x
		newTbl[key] = x
	}
	return newTbl, modTbl
}

func localExtend(env1 locTable, env2 locTable) (locTable, locTable) {
	newTbl := make(locTable)
	modTbl := make(locTable)
	ext := env1.topOfLocal() + 1
	for key, val := range env1 {
		newTbl[key] = &varInfo{index: val.index, lorf: val.lorf, indirect: val.indirect}
	}
	for key, val := range env2 {
		x := &varInfo{index: val.index + ext, lorf: val.lorf, indirect: val.indirect}
		modTbl[key] = x
		newTbl[key] = x
	}
	return newTbl, modTbl
}

func locTblMinus(tbl1 locTable, tbl2 locTable) locTable {
	newTbl := make(locTable)
	for key, val := range tbl1 {
		newTbl[key] = val
	}
	for key, _ := range tbl2 {
		delete(newTbl, key)
	}
	return newTbl
}

func locTblIntersect(tbl1 locTable, tbl2 locTable) locTable {
	newTbl := make(locTable)
	for key, val := range tbl1 {
		if _, exist := tbl2[key]; exist {
			newTbl[key] = val
		}
	}
	return newTbl
}

func mkLocTable(list yObj) (tbl locTable, reqc int, optc int, err error) {
	argErr := mkYerror(errComp, errSyntax, "malformed argument list")
	tbl = make(locTable)
	lst := list

	typ := typeOf(list)
	if typ == tConstant {
		if list == yNULL {
			return tbl, 0, 0, nil
		}
		err = argErr
		goto ERROR
	}

	if typ == tSymbol {
		err = tbl.addToTable(list, false)
		if err != nil {
			goto ERROR
		}
		return tbl, 0, 1, nil
	}

	if typ != tPair {
		err = argErr
		goto ERROR
	}

	for {
		typ = typeOf(lst)
		switch typ {
		case tConstant:
			if lst == yNULL {
				return tbl, reqc, optc, nil
			}
			err = argErr
			goto ERROR
		case tPair:
			err = tbl.addToTable(pair(lst).car, true)
			if err != nil {
				goto ERROR
			}
			reqc++
			lst = pair(lst).cdr
		case tSymbol:
			err = tbl.addToTable(lst, true)
			if err != nil {
				goto ERROR
			}
			optc++
			return tbl, reqc, optc, nil
		default:
			err = argErr
			goto ERROR
		}
	}
ERROR:
	return nil, 0, 0, err
}

/*----------------------------------------------------
 * xlocTable  (ordered locTable)
 */
type xlocTable struct {
	sym  yObj
	info *varInfo
}

/*----------------------------------------------------
 * defInfo
 */
type defInfo struct {
	sym  yObj
	gloc *yObj
}

/*----------------------------------------------------
 * compile
 */

func compile(exp yObj, env locTable, m *vm, ctx int, next *insn) (*insn, error) {
	var err error
	x := exp
	nx := next
	for {
		typ := typeOf(x)
		switch typ {
		case tSymbol:
			return compileRefer(x, env, m, nx), nil
		case tVector:
			err = mkYerror(errComp, errSyntax)
			goto ERROR
		case tPair:
			xCar := pair(x).car
			if typeOf(xCar) == tSymbol {
				op := lookUp(xCar, env, m)
				switch op {
				case tSyntaxQuote:
					return &insn{op: opCONSTANT, o0: pair(pair(x).cdr).car, i0: nx}, nil
				case tSyntaxLambda:
					var (
						lmbdVars           locTable
						lmbdBody           yObj
						lmbdReqc, lmbdOptc int
					)
					lmbdVars, lmbdBody, lmbdReqc, lmbdOptc, err = recordLambda(x)
					if err != nil {
						goto ERROR
					}
					return compileLambda(lmbdVars, lmbdBody, lmbdReqc, lmbdOptc, env, m, ctx, nx)
				case tSyntaxLet:
					var (
						l1vars locTable
						l1vals []yObj
						l1body yObj
						l1tag  yObj
					)
					l1vars, l1vals, l1body, l1tag, err = recordLet(x, false)
					if err != nil {
						goto ERROR
					}
					if l1tag != nil { //if named let, expand to letrec and compile.
						x = namedLetExpand(l1vars, l1vals, l1body, l1tag)
						continue
					}
					err = findSets(l1body, l1vars, env, m)
					if err != nil {
						goto ERROR
					}
					nxt := nx
					var locc int
					locc, err = findLocal(x, env, m, ctx)
					if err != nil {
						goto ERROR
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opSHRINK, n0: locc, i0: nxt}
						}
					}
					extEnv, modVars := localExtend(env, l1vars)
					var letBody *insn
					letBody, err = compileLetBody(l1body, extEnv, m, nxt)
					if err != nil {
						goto ERROR
					}

					nxt = makeBoxes(modVars, letBody)
					ext := env.topOfLocal() + 1
					for i, val := range l1vals {
						nxt, err = compile(val, env, m, ctx|ctxINNER, &insn{op: opPUSHLOCAL, n0: ext + i, i0: nxt})
						if err != nil {
							goto ERROR
						}
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opEXTEND, n0: locc, i0: nxt}
						}
					}
					return nxt, nil
				case tSyntaxLetseq:
					var (
						lsvars locTable
						lsvals []yObj
						lsbody yObj
					)
					lsvars, lsvals, lsbody, _, err = recordLet(x, true)
					if err != nil {
						goto ERROR
					}
					length := len(lsvars)
					ext := env.numOfLocal()
					orderedVars := make([]*xlocTable, length)
					for vr, info := range lsvars {
						tmp := locTable{vr: info}
						idx := info.index
						for i := idx + 1; i < length; i++ {
							err = findSets(lsvals[i], tmp, env, m)
							if err != nil {
								goto ERROR
							}
						}
						info.index += ext
						orderedVars[idx] = &xlocTable{sym: vr, info: info}
					}
					err = findSets(lsbody, lsvars, env, m)
					if err != nil {
						goto ERROR
					}

					nxt := nx
					var locc int
					locc, err = findLocal(x, env, m, ctx)
					if err != nil {
						goto ERROR
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opSHRINK, n0: locc, i0: nxt}
						}
					}
					extEnv := locTblUnion(env, lsvars)
					var letBody *insn
					letBody, err = compileLetBody(lsbody, extEnv, m, nxt)
					if err != nil {
						goto ERROR
					}

					nxt = makeBoxes(lsvars, letBody)
					envs := make([]locTable, length)
					for i := 0; i < length; i++ {
						envs[i] = make(locTable)
						for j := 0; j < i; j++ {
							v := orderedVars[j]
							inf := v.info
							envs[i][v.sym] = &varInfo{index: inf.index, lorf: inf.lorf, indirect: inf.indirect}
						}
					}
					for i := length - 1; i > -1; i-- {
						extEnv = locTblUnion(env, envs[i])
						nxt, err = compile(lsvals[i], extEnv, m, ctx|ctxINNER, &insn{op: opPUSHLOCAL, n0: ext + i, i0: nxt})
						if err != nil {
							goto ERROR
						}
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opEXTEND, n0: locc, i0: nxt}
						}
					}
					return nxt, nil
				case tSyntaxLetrec:
					var (
						lrvars          locTable
						lrvals, tmpVars []yObj
						lrbody          yObj
					)
					lrvars, lrvals, lrbody, _, err = recordLet(x, true)
					if err != nil {
						goto ERROR
					}
					tmps := make(locTable)
					tmpVars = make([]yObj, len(lrvars))
					i := 0
					for _, info := range lrvars { //making inner tmp binds, and flag all of vars as indirect.
						info.indirect = true
						t := mkUninternedSym("t" + strconv.FormatInt(int64(i), 10))
						tmps[t] = &varInfo{index: i}
						tmpVars[i] = t
						i++
					}

					nxt := nx
					var locc int
					locc, err = findLocal(x, env, m, ctx)
					if err != nil {
						goto ERROR
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opSHRINK, n0: locc, i0: nxt}
						}
					}
					ext1Env, _ := locTblExtend(env, lrvars)
					ext2Env, _ := locTblExtend(ext1Env, tmps)
					var letBody *insn
					letBody, err = compileLetBody(lrbody, ext2Env, m, nxt)
					if err != nil {
						goto ERROR
					}

					nxt = letBody
					setSym := mkSymbol("set!")
					for vr, vl := range lrvars {
						setExp := mkPair(setSym, mkPair(vr, mkPair(tmpVars[vl.index], yNULL)))
						nxt, err = compile(setExp, ext2Env, m, ctxNotopInner, nxt)
						if err != nil {
							goto ERROR
						}
					}
					ext := ext1Env.numOfLocal()
					for i, val := range lrvals {
						nxt, err = compile(val, ext1Env, m, ctxNotopInner, &insn{op: opPUSHLOCAL, n0: ext + i, i0: nxt})
						if err != nil {
							goto ERROR
						}
					}
					nxt = makeBoxes(ext1Env, nxt)
					ext = env.numOfLocal()
					for i, _ := range lrvals {
						nxt = &insn{op: opCONSTANT, o0: yUNDEFINED, i0: &insn{op: opPUSHLOCAL, n0: ext + i, i0: nxt}}
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opEXTEND, n0: locc, i0: nxt}
						}
					}
					return nxt, nil
				case tSyntaxIf:
					nxt := nx
					var locc int
					locc, err = findLocal(x, env, m, ctx)
					if err != nil {
						goto ERROR
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opSHRINK, n0: locc, i0: nxt}
						}
					}
					var tst, thn, els yObj
					tst, thn, els, err = recordIf(x)
					if err != nil {
						goto ERROR
					}
					var thnc, elsc *insn
					thnc, err = compile(thn, env, m, ctx|ctxINNER, nxt)
					if err != nil {
						goto ERROR
					}
					elsc, err = compile(els, env, m, ctx|ctxINNER, nxt)
					if err != nil {
						goto ERROR
					}
					nxt, err = compile(tst, env, m, ctx|ctxINNER, &insn{op: opTEST, i0: thnc, i1: elsc})
					if err != nil {
						goto ERROR
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opEXTEND, n0: locc, i0: nxt}
						}
					}
					return nxt, nil
				case tSyntaxDefine:
					if (ctx & ctxNOTOP) != 0 {
						err = mkYerror(errComp, errSyntax, "define must be in toplevel")
						goto ERROR
					}
					var defvar, defval yObj
					defvar, defval, err = recordDefine(x)
					if err != nil {
						goto ERROR
					}
					nxt := nx
					var locc int
					locc, err = findLocal(x, env, m, ctx)
					if err != nil {
						goto ERROR
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opSHRINK, n0: locc, i0: nxt}
						}
					}
					nxt, err = compileDefine(defvar, env, m, nxt)
					if err != nil {
						goto ERROR
					}
					nxt, err = compile(defval, env, m, ctxTopInner, nxt)
					if err != nil {
						goto ERROR
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opEXTEND, n0: locc, i0: nxt}
						}
					}
					return nxt, nil
				case tSyntaxSet:
					var svar, sval yObj
					svar, sval, err = recordSet(x)
					if err != nil {
						goto ERROR
					}
					nxt := nx
					var locc int
					locc, err = findLocal(x, env, m, ctx)
					if err != nil {
						goto ERROR
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opSHRINK, n0: locc, i0: nxt}
						}
					}
					nxt, err = compileAssign(svar, env, m.env, nx)
					if err != nil {
						goto ERROR
					}
					nxt, err = compile(sval, env, m, ctx|ctxINNER, nxt)
					if err != nil {
						goto ERROR
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opEXTEND, n0: locc, i0: nxt}
						}
					}
					return nxt, nil
				case tSyntaxBegin:
					var exps []yObj
					exps, err = recordBegin(x)
					if err != nil {
						goto ERROR
					}
					nxt := nx
					var locc int
					locc, err = findLocal(x, env, m, ctx)
					if err != nil {
						goto ERROR
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opSHRINK, n0: locc, i0: nxt}
						}
					}
					for i := len(exps) - 1; i >= 0; i-- {
						nxt, err = compile(exps[i], env, m, ctx|ctxINNER, nxt)
						if err != nil {
							goto ERROR
						}
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opEXTEND, n0: locc, i0: nxt}
						}
					}
					return nxt, nil
				case tSyntaxCond:
					if typeOf(pair(x).cdr) != tPair {
						err = mkYerror(errComp, errSyntax, "malformed cond")
						goto ERROR
					}

					var clauses []yObj
					clauses, err = recordBegin(x)
					if err != nil {
						goto ERROR
					}
					nxt := nx
					var locc int
					locc, err = findLocal(x, env, m, ctx)
					if err != nil {
						goto ERROR
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opSHRINK, n0: locc, i0: nxt}
						}
					}
					nxt, err = compileCond(clauses, env, m, ctx|ctxINNER, nxt)
					if err != nil {
						goto ERROR
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opEXTEND, n0: locc, i0: nxt}
						}
					}
					return nxt, nil
				case tSyntaxCase:
					if typeOf(pair(x).cdr) != tPair {
						err = mkYerror(errComp, errSyntax, "malformed case")
						goto ERROR
					}
					var keyAndClauses []yObj
					keyAndClauses, err = recordBegin(x)
					if err != nil {
						goto ERROR
					}
					nxt := nx
					var locc int
					locc, err = findLocal(x, env, m, ctx)
					if err != nil {
						goto ERROR
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opSHRINK, n0: locc, i0: nxt}
						}
					}
					nxt, err = compileCase(keyAndClauses, env, m, ctx|ctxINNER, true, nx)
					if err != nil {
						goto ERROR
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opEXTEND, n0: locc, i0: nxt}
						}
					}
					return nxt, nil
				case tSyntaxDo:
					xCdr := pair(x).cdr
					if length(xCdr) < 2 {
						err = mkYerror(errComp, errSyntax, "malformed do")
						goto ERROR
					}
					nxt := nx
					var locc int
					locc, err = findLocal(x, env, m, ctx)
					if err != nil {
						goto ERROR
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opSHRINK, n0: locc, i0: nxt}
						}
					}
					nxt, err = compileDo(xCdr, env, m, ctx|ctxINNER, nx)
					if err != nil {
						goto ERROR
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opEXTEND, n0: locc, i0: nxt}
						}
					}
					return nxt, nil
				case tSyntaxAnd:
					var exps []yObj
					exps, err = recordBegin(x)
					if err != nil {
						goto ERROR
					}
					nxt := nx
					var locc int
					locc, err = findLocal(x, env, m, ctx)
					if err != nil {
						goto ERROR
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opSHRINK, n0: locc, i0: nxt}
						}
					}
					if len(exps) == 0 {
						return &insn{op: opCONSTANT, o0: yTRUE, i0: nx}, nil
					}
					for i := len(exps) - 1; i > -1; i-- {
						nxt = &insn{op: opTEST, i0: nxt, i1: nx}
						nxt, err = compile(exps[i], env, m, ctx|ctxINNER, nxt)
						if err != nil {
							goto ERROR
						}
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opEXTEND, n0: locc, i0: nxt}
						}
					}
					return nxt, nil
				case tSyntaxOr:
					var exps []yObj
					exps, err = recordBegin(x)
					if err != nil {
						goto ERROR
					}
					nxt := nx
					var locc int
					locc, err = findLocal(x, env, m, ctx)
					if err != nil {
						goto ERROR
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opSHRINK, n0: locc, i0: nxt}
						}
					}
					if len(exps) == 0 {
						return &insn{op: opCONSTANT, o0: yFALSE, i0: nx}, nil
					}
					for i := len(exps) - 1; i > -1; i-- {
						nxt = &insn{op: opTEST, i0: nx, i1: nxt}
						nxt, err = compile(exps[i], env, m, ctx|ctxINNER, nxt)
						if err != nil {
							goto ERROR
						}
					}
					if ctx == ctxTopOuter {
						if locc > 0 {
							nxt = &insn{op: opEXTEND, n0: locc, i0: nxt}
						}
					}
					return nxt, nil
				case tSyntaxQuasiquote:
					return compileQq(x, 0, env, m, ctx, nx)
				case tSyntaxCallwithcc:
					rest := pair(x).cdr
					if typeOf(rest) != tPair || (length(rest) > 1) {
						err = mkYerror(errComp, errSyntax, "malformed call/cc")
						goto ERROR
					}
					body := pair(rest).car
					var nxt *insn = &insn{op: opCONTI, i0: &insn{op: opAPPLY}}
					tail := (nx.op == opRETURN)
					if tail {
						nxt = &insn{op: opSHRINK, n0: nx.n0, i0: nxt}
					}
					var c *insn
					c, err = compile(body, env, m, ctx|ctxINNER, nxt)
					if err != nil {
						goto ERROR
					}
					if tail {
						return c, nil
					}
					return &insn{op: opFRAME, i0: nx, i1: c}, nil
				case tProc:
					nxt := nx
					var locc int
					locc, err = findLocal(x, env, m, ctx)
					if err != nil {
						goto ERROR
					}
					needLocExt := (ctx == ctxTopOuter && locc > 0)
					if needLocExt {
						nxt = &insn{op: opSHRINK, n0: locc, i0: nxt}
					}
					nxt, err = compileProc(x, env, m, ctx|ctxINNER, nxt)
					if err != nil {
						goto ERROR
					}
					if needLocExt {
						nxt = &insn{op: opEXTEND, n0: locc, i0: nxt}
					}
					return nxt, nil
				}
			}

			return compileFuncall(x, env, m, ctx, nx)
		default:
			return &insn{op: opCONSTANT, o0: x, i0: nx}, nil
		}
	}

ERROR:
	return nil, err
}

/*----------------------------------------------------
 * lookUp
 */
func lookUp(sym yObj, env locTable, m *vm) int64 {
	if gRec, exist := m.env[sym]; exist {
		typ := typeOf(*(*yObj)(gRec.gloc))
		return typ
	}
	return 0
}

/*----------------------------------------------------
 * maybeBegin
 */
func maybeBegin(body yObj) yObj {
	if body == yNULL {
		return yNULL
	}
	var ret yObj
	if length(body) > 1 {
		ret = yObj(&yCell{car: mkSymbol("begin"), cdr: body})
	} else {
		ret = pair(body).car
	}
	return ret
}

/*----------------------------------------------------
 * recordLambda
 */
func recordLambda(lmbd yObj) (vars locTable, body yObj, reqc int, optc int, err error) {
	var typ int64
	malErr := mkYerror(errComp, errSyntax, "malformed lambda")
	rest := pair(lmbd).cdr
	if typeOf(rest) != tPair {
		err = malErr
		goto ERROR
	}
	vars, reqc, optc, err = mkLocTable(pair(rest).car)
	if err != nil {
		goto ERROR
	}

	rest = pair(rest).cdr
	typ = typeOf(rest)
	switch typ {
	case tPair:
		body = maybeBegin(rest)
	case tConstant:
		if rest == yNULL {
			body = yNULL
		} else {
			err = malErr
			goto ERROR
		}
	default:
		err = malErr
		goto ERROR
	}
	return vars, body, reqc, optc, nil

ERROR:
	return nil, nil, 0, 0, err
}

/*----------------------------------------------------
 * recordLet
 *
 */
func recordLet(let yObj, rec bool) (vars locTable, vals []yObj, body yObj, tag yObj, err error) {
	var (
		typ               int64
		cadrLet, bindsLet yObj
	)
	rest := pair(let).cdr
	if typeOf(rest) != tPair {
		goto ERROR
	}

	//making tag
	cadrLet = pair(rest).car
	typ = typeOf(cadrLet)
	if typ == tSymbol {
		if rec {
			return nil, nil, nil, nil, mkYerror(errComp, errSyntax, "malformed letrec")
		}
		tag = cadrLet
		rest = pair(rest).cdr
		if typeOf(rest) != tPair {
			goto ERROR
		}
	}

	//making vars and vals
	vars = make(locTable)
	vals = make([]yObj, 0)
	bindsLet = pair(rest).car
	typ = typeOf(bindsLet)
	switch typ {
	case tConstant:
		if bindsLet != yNULL {
			goto ERROR
		}
	case tPair:
		binds := pair(bindsLet)
	VARCOLLECT:
		for {
			bind := binds.car
			if typeOf(bind) != tPair {
				goto ERROR
			}
			carBind := pair(bind).car
			if typeOf(carBind) != tSymbol {
				goto ERROR
			}
			vars[carBind] = &varInfo{index: len(vars)}
			cdrBind := pair(bind).cdr
			if typeOf(cdrBind) != tPair {
				goto ERROR
			}
			vals = append(vals, pair(cdrBind).car)
			if pair(cdrBind).cdr != yNULL {
				goto ERROR
			}
			cdrBinds := binds.cdr
			typ = typeOf(cdrBinds)
			switch typ {
			case tConstant:
				if cdrBinds == yNULL {
					break VARCOLLECT
				} else {
					goto ERROR
				}
			case tPair:
				binds = pair(cdrBinds)
			default:
				goto ERROR
			}
		}
	default:
		goto ERROR
	}

	//making body
	rest = pair(rest).cdr
	typ = typeOf(rest)
	switch typ {
	case tConstant:
		if rest == yNULL {
			body = yNULL
		} else {
			goto ERROR
		}
	case tPair:
		body = maybeBegin(rest)
	default:
		goto ERROR
	}

	return vars, vals, body, tag, nil

ERROR:
	return nil, nil, nil, nil, mkYerror(errComp, errSyntax, "malformed let")
}

/*----------------------------------------------------
 * namedLetExpand
 */
func namedLetExpand(vars locTable, vals []yObj, body yObj, tag yObj) yObj {
	indexes := make([]int, len(vars))
	i := 0
	argRoot, argLast := yNULL, yNULL
	for v, info := range vars {
		argRoot, argLast = scmAppend(argRoot, argLast, v)
		indexes[i] = info.index
		i++
	}

	letrec := mkPair(mkSymbol("letrec"), mkPair(mkPair(mkPair(tag, mkPair(mkPair(mkSymbol("lambda"),
		mkPair(argRoot, mkPair(body, yNULL))), yNULL)), yNULL), mkPair(tag, yNULL)))

	valRoot, valLast := yNULL, yNULL
	i = 0
	for i < len(vars) {
		valRoot, valLast = scmAppend(valRoot, valLast, vals[indexes[i]])
		i++
	}

	return yObj(&yCell{car: letrec, cdr: valRoot})
}

/*----------------------------------------------------
 * makeLetrecBody
 */
func makeLetrecBody(vars locTable, vals []yObj, body yObj) yObj {
	lRoot := yObj(&yCell{car: mkSymbol("let")})
	lLast := lRoot
	var bRoot, bLast yObj = yNULL, yNULL
	tmpVars := make([]yObj, len(vals))
	for i, v := range vals {
		t := mkUninternedSym("t" + strconv.FormatInt(int64(i), 10))
		tmpVars[i] = t
		bRoot, bLast = scmAppend(bRoot, bLast,
			yObj(&yCell{car: t, cdr: yObj(&yCell{car: v, cdr: yNULL})}))
	}
	lRoot, lLast = scmAppend(lRoot, lLast, bRoot)

	for v, info := range vars {
		lRoot, lLast = scmAppend(lRoot, lLast,
			yObj(&yCell{car: mkSymbol("set!"),
				cdr: yObj(&yCell{car: v,
					cdr: yObj(&yCell{car: tmpVars[info.index],
						cdr: yNULL})})}))
	}
	lRoot, _ = scmAppend(lRoot, lLast, body)
	return lRoot
}

/*----------------------------------------------------
 * recordIf
 */
func recordIf(ifexp yObj) (tst yObj, thn yObj, els yObj, err error) {
	var (
		typ  int64
		rest yObj
	)
	p := pair(ifexp).cdr
	if typeOf(p) != tPair {
		goto ERROR
	}
	tst = pair(p).car
	p = pair(p).cdr
	if typeOf(p) != tPair {
		goto ERROR
	}
	thn = pair(p).car
	rest = pair(p).cdr
	typ = typeOf(rest)
	switch typ {
	case tConstant:
		if rest == yNULL {
			els = yUNDEFINED
		} else {
			goto ERROR
		}
	case tPair:
		if length(rest) > 1 {
			goto ERROR
		}
		els = pair(rest).car
	default:
		goto ERROR
	}
	return tst, thn, els, nil

ERROR:
	return nil, nil, nil, mkYerror(errComp, errSyntax, "malformed if")
}

/*----------------------------------------------------
 * recordSet
 */
func recordSet(setexp yObj) (svar yObj, exp yObj, err error) {
	var rest yObj
	asgn := pair(setexp).cdr
	if typeOf(asgn) != tPair {
		goto ERROR
	}
	svar = pair(asgn).car
	if typeOf(svar) != tSymbol {
		goto ERROR
	}
	rest = pair(asgn).cdr
	if typeOf(rest) != tPair || (length(rest) > 1) {
		goto ERROR
	}
	exp = pair(rest).car

	return svar, exp, nil

ERROR:
	return nil, nil, mkYerror(errComp, errSyntax, "malformed set!")
}

/*----------------------------------------------------
 * recordDefine
 */
func recordDefine(defexp yObj) (svar yObj, exp yObj, err error) {
	var (
		typ            int64
		cadrDef, parms yObj
	)

	rest := pair(defexp).cdr
	if typeOf(rest) != tPair {
		goto ERROR
	}
	cadrDef = pair(rest).car
	typ = typeOf(cadrDef)

	switch typ {
	case tSymbol:
		svar = cadrDef
	case tPair:
		svar = pair(cadrDef).car
		if typeOf(svar) != tSymbol {
			goto ERROR
		}
		parms = pair(cadrDef).cdr
	default:
		goto ERROR
	}
	rest = pair(rest).cdr
	if !(typeOf(rest) == tPair || rest == yNULL) {
		goto ERROR
	}
	if parms == nil {
		if typeOf(rest) != tPair || (length(rest) > 1) {
			goto ERROR
		}
		exp = pair(rest).car
		return svar, exp, nil
	}
	exp = mkPair(mkSymbol("lambda"), mkPair(parms, rest))
	return svar, exp, nil

ERROR:
	return nil, nil, mkYerror(errComp, errSyntax, "malformed define")
}

/*----------------------------------------------------
 * recordBegin
 */
func recordBegin(beginexp yObj) (exps []yObj, err error) {
	exps = make([]yObj, 0)
	body := pair(beginexp).cdr
	if typeOf(body) == tConstant {
		if body == yNULL {
			return exps, nil
		} else {
			goto ERROR
		}
	}

	if typeOf(body) != tPair {
		goto ERROR
	}
BEGINLOOP:
	for {
		exps = append(exps, pair(body).car)
		cdrBody := pair(body).cdr
		typ := typeOf(cdrBody)
		switch typ {
		case tConstant:
			if cdrBody == yNULL {
				break BEGINLOOP
			} else {
				goto ERROR
			}
		case tPair:
			body = cdrBody
		default:
			goto ERROR
		}
	}
	return exps, nil

ERROR:
	return nil, mkYerror(errComp, errSyntax, "malformed begin")
}

/*----------------------------------------------------
 * findFree
 */
func findFree(body yObj, vars locTable, env locTable, m *vm) (locTable, error) {
	var err error
	x := body
	v := vars
	for {
		typ := typeOf(x)
		switch typ {
		case tConstant, tIntnum, tFlonum, tRatinum, tCmplnum, tString, tChar, tVector:
			return make(locTable), nil
		case tSymbol:
			if _, exist := v[x]; !exist {
				if val, exist := env[x]; exist {
					return locTable{x: val}, nil
				}
			}
			return make(locTable), nil
		case tPair:
			xCar := pair(x).car
			if typeOf(xCar) == tSymbol {
				op := lookUp(xCar, env, m)
				switch op {
				case tSyntaxQuote:
					return make(locTable), nil
				case tSyntaxLambda:
					var (
						lmbdVars locTable
						lmbdBody yObj
					)
					lmbdVars, lmbdBody, _, _, err = recordLambda(x)
					if err != nil {
						goto ERROR
					}
					x = lmbdBody
					v = locTblUnion(v, lmbdVars)
					continue
				case tSyntaxLet:
					var (
						l1vars locTable
						l1vals []yObj
						l1body yObj
						l1tag  yObj
					)
					l1vars, l1vals, l1body, l1tag, err = recordLet(x, false)
					if err != nil {
						goto ERROR
					}
					extLetVars := locTblUnion(v, l1vars)
					if l1tag != nil {
						err = extLetVars.addToTable(l1tag, false)
						if err != nil {
							goto ERROR
						}
					}
					var frees, frtmp locTable
					frees, err = findFree(l1body, extLetVars, env, m)
					if err != nil {
						goto ERROR
					}
					for _, xx := range l1vals {
						frtmp, err = findFree(xx, v, env, m)
						if err != nil {
							goto ERROR
						}
						frees = locTblUnion(frees, frtmp)
					}
					return frees, nil
				case tSyntaxLetseq:
					var (
						lsvars, frees, frtmp, extLetVars locTable
						lsvals                           []yObj
						lsbody                           yObj
					)
					lsvars, lsvals, lsbody, _, err = recordLet(x, false)
					if err != nil {
						goto ERROR
					}

					length := len(lsvars)
					orderedLocTbl := make([]locTable, length)
					for vr, info := range lsvars {
						orderedLocTbl[info.index] = locTable{vr: info}
					}

					extLetVars = v
					frees = make(locTable)
					for i := 0; i < length; i++ {
						frtmp, err = findFree(lsvals[i], extLetVars, env, m)
						if err != nil {
							goto ERROR
						}
						frees = locTblUnion(frees, frtmp)
						extLetVars = locTblUnion(extLetVars, orderedLocTbl[i])
					}

					frtmp, err = findFree(lsbody, extLetVars, env, m)
					if err != nil {
						goto ERROR
					}

					return locTblUnion(frees, frtmp), nil
				case tSyntaxLetrec:
					var (
						lrvars locTable
						lrvals []yObj
						lrbody yObj
					)
					lrvars, lrvals, lrbody, _, err = recordLet(x, true)
					if err != nil {
						goto ERROR
					}
					extLetVars := locTblUnion(v, lrvars)
					var frees, frtmp locTable
					frees, err = findFree(lrbody, extLetVars, env, m)
					if err != nil {
						goto ERROR
					}
					for _, xx := range lrvals {
						frtmp, err = findFree(xx, extLetVars, env, m)
						if err != nil {
							goto ERROR
						}
						frees = locTblUnion(frees, frtmp)
					}
					return frees, nil
				case tSyntaxIf:
					var tst, thn, els yObj
					tst, thn, els, err = recordIf(x)
					if err != nil {
						goto ERROR
					}
					var frtmp1, frtmp2, frtmp3 locTable
					frtmp1, err = findFree(tst, v, env, m)
					if err != nil {
						goto ERROR
					}
					frtmp2, err = findFree(thn, v, env, m)
					if err != nil {
						goto ERROR
					}
					frtmp3, err = findFree(els, v, env, m)
					if err != nil {
						goto ERROR
					}
					return locTblUnion(locTblUnion(frtmp1, frtmp2), frtmp3), nil
				case tSyntaxDefine:
					var defvar, defval yObj
					defvar, defval, err = recordDefine(x)
					if err != nil {
						goto ERROR
					}
					defv := locTable{defvar: &varInfo{}}
					return findFree(defval, locTblUnion(v, defv), env, m)
				case tSyntaxSet:
					var svar, sval yObj
					svar, sval, err = recordSet(x)
					if err != nil {
						goto ERROR
					}
					svarFrees := make(locTable)
					if _, exist := v[svar]; !exist {
						if val, exist := env[svar]; exist {
							svarFrees = locTable{svar: val}
						}
					}
					var frtmp locTable
					frtmp, err = findFree(sval, v, env, m)
					if err != nil {
						goto ERROR
					}
					return locTblUnion(svarFrees, frtmp), nil
				case tSyntaxBegin, tSyntaxAnd, tSyntaxOr:
					var exps []yObj
					exps, err = recordBegin(x)
					if err != nil {
						goto ERROR
					}
					frees := make(locTable)
					var frtmp locTable
					for _, e := range exps {
						frtmp, err = findFree(e, v, env, m)
						if err != nil {
							goto ERROR
						}
						frees = locTblUnion(frees, frtmp)
					}
					return frees, nil
				case tSyntaxCond:
					elsSym := mkSymbol("else")
					arrSym := mkSymbol("=>")
					frees := make(locTable)
					var frtmp locTable
					if typeOf(pair(x).cdr) != tPair {
						err = mkYerror(errComp, errSyntax, "malformed cond")
						goto ERROR
					}
					var clauses []yObj
					clauses, err = recordBegin(x)
					if err != nil {
						goto ERROR
					}
					for _, clause := range clauses {
						if typeOf(clause) != tPair {
							err = mkYerror(errComp, errSyntax, "malformed cond")
							goto ERROR
						}
						carClause := pair(clause).car
						if carClause == elsSym {
							frtmp, err = findFree(pair(clause).cdr, v, env, m)
							if err != nil {
								goto ERROR
							}
							frees = locTblUnion(frees, frtmp)
							continue
						}
						frtmp, err = findFree(carClause, v, env, m)
						if err != nil {
							goto ERROR
						}
						frees = locTblUnion(frees, frtmp)
						cdrClause := pair(clause).cdr
						if typeOf(cdrClause) != tPair {
							continue
						}
						if pair(cdrClause).car == arrSym {
							frtmp, err = findFree(pair(cdrClause).cdr, v, env, m)
							if err != nil {
								goto ERROR
							}
							frees = locTblUnion(frees, frtmp)
							continue
						}
						frtmp, err = findFree(cdrClause, v, env, m)
						if err != nil {
							goto ERROR
						}
						frees = locTblUnion(frees, frtmp)
						continue
					}
					return frees, nil
				case tSyntaxCase:
					var keyAndClauses []yObj
					var frtmp locTable
					frees := make(locTable)
					if typeOf(pair(x).cdr) != tPair {
						err = mkYerror(errComp, errSyntax, "malformed case")
						goto ERROR
					}
					keyAndClauses, err = recordBegin(x)
					if err != nil {
						goto ERROR
					}
					frees, err = findFree(keyAndClauses[0], v, env, m)
					if err != nil {
						goto ERROR
					}
					for _, clause := range keyAndClauses {
						if typeOf(clause) == tPair {
							frtmp, err = findFree(pair(clause).cdr, v, env, m)
							if err != nil {
								goto ERROR
							}
							frees = locTblUnion(frees, frtmp)
							continue
						}
					}
					return frees, nil
				case tSyntaxDo:
					var (
						vars                       locTable
						inits, steps, posts, bodys []yObj
						test                       yObj
						extV, frtmp                locTable
					)
					frees := make(locTable)
					xCdr := pair(x).cdr
					if length(xCdr) < 2 {
						err = mkYerror(errComp, errSyntax, "malformed do")
						goto ERROR
					}

					vars, inits, steps, test, posts, bodys, err = recordDo(xCdr)
					if err != nil {
						goto ERROR
					}
					extV = locTblUnion(v, vars)
					for _, init := range inits {
						frtmp, err = findFree(init, v, env, m)
						if err != nil {
							goto ERROR
						}
						frees = locTblUnion(frees, frtmp)
					}
					for _, step := range steps {
						frtmp, err = findFree(step, extV, env, m)
						if err != nil {
							goto ERROR
						}
						frees = locTblUnion(frees, frtmp)
					}
					frtmp, err = findFree(test, extV, env, m)
					if err != nil {
						goto ERROR
					}
					frees = locTblUnion(frees, frtmp)
					for _, post := range posts {
						frtmp, err = findFree(post, extV, env, m)
						if err != nil {
							goto ERROR
						}
						frees = locTblUnion(frees, frtmp)
					}
					for _, body := range bodys {
						frtmp, err = findFree(body, extV, env, m)
						if err != nil {
							goto ERROR
						}
						frees = locTblUnion(frees, frtmp)
					}
					return frees, nil
				case tSyntaxQuasiquote:
					return findFreeInQq(x, 0, v, env, m)
				}
			}

			ret := make(locTable)
			p := pair(x)
			var frtmp locTable
		VARCOLLECT:
			for {
				frtmp, err = findFree(p.car, v, env, m)
				if err != nil {
					goto ERROR
				}
				ret = locTblUnion(ret, frtmp)
				nxt := p.cdr
				typ = typeOf(nxt)
				switch typ {
				case tConstant:
					if nxt == yNULL {
						break VARCOLLECT
					}
				case tPair:
					p = pair(nxt)
				default:
					frtmp, err = findFree(nxt, v, env, m)
					if err != nil {
						goto ERROR
					}
					ret = locTblUnion(ret, frtmp)
					break VARCOLLECT
				}
			}
			return ret, nil
		}
	}

ERROR:
	return nil, err
}

/*----------------------------------------------------
 * findSets
 *  todo: env isnot used, ommit it.
 */
func findSets(body yObj, vars locTable, env locTable, m *vm) error {
	var err error
	x := body
	v := vars
FINDSETSLOOP:
	for {
		typ := typeOf(x)
		switch typ {
		case tSymbol, tConstant, tIntnum, tFlonum, tRatinum, tCmplnum, tChar, tString, tVector:
			break FINDSETSLOOP
		case tPair:
			xCar := pair(x).car
			if typeOf(xCar) == tSymbol {
				//symName := symbolVal(xCar)
				op := lookUp(xCar, env, m)
				switch op {
				case tSyntaxQuote:
					break FINDSETSLOOP
				case tSyntaxLambda:
					var (
						lmbdVars locTable
						lmbdBody yObj
					)
					lmbdVars, lmbdBody, _, _, err = recordLambda(x)
					if err != nil {
						goto ERROR
					}
					x = lmbdBody
					v = locTblMinus(v, lmbdVars)
					continue
				case tSyntaxLet:
					var (
						l1vars locTable
						l1vals []yObj
						l1body yObj
						l1tag  yObj
					)
					l1vars, l1vals, l1body, l1tag, err = recordLet(x, false)
					if err != nil {
						goto ERROR
					}
					extLetVars := locTblMinus(v, l1vars)
					delete(extLetVars, l1tag)
					err = findSets(l1body, extLetVars, env, m)
					if err != nil {
						goto ERROR
					}
					for _, xx := range l1vals {
						err = findSets(xx, v, env, m)
						if err != nil {
							goto ERROR
						}
					}
					break FINDSETSLOOP
				case tSyntaxLetseq:
					var (
						lsvars, extLetVars locTable
						lsvals             []yObj
						lsbody             yObj
					)
					lsvars, lsvals, lsbody, _, err = recordLet(x, false)
					if err != nil {
						goto ERROR
					}

					length := len(lsvars)
					orderedLocTbl := make([]locTable, length)
					for vr, info := range lsvars {
						orderedLocTbl[info.index] = locTable{vr: info}
					}

					extLetVars = v
					for i := 0; i < length; i++ {
						err = findSets(lsvals[i], extLetVars, env, m)
						if err != nil {
							goto ERROR
						}
						extLetVars = locTblMinus(extLetVars, orderedLocTbl[i])
					}

					err = findSets(lsbody, extLetVars, env, m)
					if err != nil {
						goto ERROR
					}
					break FINDSETSLOOP
				case tSyntaxLetrec:
					var (
						lrvars locTable
						lrvals []yObj
						lrbody yObj
					)
					lrvars, lrvals, lrbody, _, err = recordLet(x, true)
					if err != nil {
						goto ERROR
					}
					extLetVars := locTblMinus(v, lrvars)
					err = findSets(lrbody, extLetVars, env, m)
					if err != nil {
						goto ERROR
					}
					for _, xx := range lrvals {
						err = findSets(xx, extLetVars, env, m)
						if err != nil {
							goto ERROR
						}
					}
					break FINDSETSLOOP
				case tSyntaxIf:
					var tst, thn, els yObj
					tst, thn, els, err = recordIf(x)
					if err != nil {
						goto ERROR
					}
					err = findSets(tst, v, env, m)
					if err != nil {
						goto ERROR
					}
					err = findSets(thn, v, env, m)
					if err != nil {
						goto ERROR
					}
					err = findSets(els, v, env, m)
					if err != nil {
						goto ERROR
					}
					break FINDSETSLOOP
				case tSyntaxDefine:
					var defvar, defval yObj
					defvar, defval, err = recordDefine(x)
					if err != nil {
						goto ERROR
					}
					v = locTblMinus(v, locTable{defvar: nil})
					x = defval
					continue
				case tSyntaxSet:
					var svar, sval yObj
					svar, sval, err = recordSet(x)
					if err != nil {
						goto ERROR
					}
					if sv, exist := v[svar]; exist {
						sv.indirect = true
					}
					err = findSets(sval, v, env, m)
					if err != nil {
						goto ERROR
					}
					break FINDSETSLOOP
				case tSyntaxBegin, tSyntaxAnd, tSyntaxOr:
					var exps []yObj
					exps, err = recordBegin(x)
					if err != nil {
						goto ERROR
					}
					for _, e := range exps {
						err = findSets(e, v, env, m)
						if err != nil {
							goto ERROR
						}
					}
					break FINDSETSLOOP
				case tSyntaxCond:
					elsSym := mkSymbol("else")
					arrSym := mkSymbol("=>")
					if typeOf(pair(x).cdr) != tPair {
						err = mkYerror(errComp, errSyntax, "malformed cond")
						goto ERROR
					}
					var clauses []yObj
					clauses, err = recordBegin(x)
					if err != nil {
						goto ERROR
					}
					for _, clause := range clauses {
						if typeOf(clause) != tPair {
							err = mkYerror(errComp, errSyntax, "malformed cond")
							goto ERROR
						}
						carClause := pair(clause).car
						if carClause == elsSym {
							err = findSets(pair(clause).cdr, v, env, m)
							if err != nil {
								goto ERROR
							}
							continue
						}
						err = findSets(carClause, v, env, m)
						if err != nil {
							goto ERROR
						}
						cdrClause := pair(clause).cdr
						if typeOf(cdrClause) != tPair {
							continue
						}
						if pair(cdrClause).car == arrSym {
							err = findSets(pair(cdrClause).cdr, v, env, m)
							if err != nil {
								goto ERROR
							}
							continue
						}
						err = findSets(cdrClause, v, env, m)
						if err != nil {
							goto ERROR
						}
						continue
					}
					break FINDSETSLOOP
				case tSyntaxCase:
					var keyAndClauses []yObj
					if typeOf(pair(x).cdr) != tPair {
						err = mkYerror(errComp, errSyntax, "malformed case")
						goto ERROR
					}
					keyAndClauses, err = recordBegin(x)
					if err != nil {
						goto ERROR
					}
					err = findSets(keyAndClauses[0], v, env, m)
					if err != nil {
						goto ERROR
					}
					for _, clause := range keyAndClauses[1:] {
						if typeOf(clause) == tPair {
							err = findSets(pair(clause).cdr, v, env, m)
							if err != nil {
								goto ERROR
							}
						}
					}
					break FINDSETSLOOP
				case tSyntaxDo:
					doExp := pair(x).cdr
					var (
						doVars                             locTable
						doInits, doSteps, doPosts, doBodys []yObj
						doTest                             yObj
					)
					doVars, doInits, doSteps, doTest, doPosts, doBodys, err = recordDo(doExp)
					if err != nil {
						goto ERROR
					}
					for _, e := range doInits {
						err = findSets(e, v, env, m)
						if err != nil {
							goto ERROR
						}
					}
					extDoVars := locTblMinus(v, doVars)
					err = findSets(doTest, extDoVars, env, m)
					if err != nil {
						goto ERROR
					}
					for _, e := range doSteps {
						if e == nil {
							continue
						}
						err = findSets(e, extDoVars, env, m)
						if err != nil {
							goto ERROR
						}
					}

					for _, e := range doPosts {
						err = findSets(e, extDoVars, env, m)
						if err != nil {
							goto ERROR
						}
					}

					for _, e := range doBodys {
						err = findSets(e, extDoVars, env, m)
						if err != nil {
							goto ERROR
						}
					}
					break FINDSETSLOOP
				case tSyntaxQuasiquote:
					err = findSetsInQq(x, 0, v, env, m)
					if err != nil {
						goto ERROR
					}
					break FINDSETSLOOP
				}
			}

			p := pair(x)
		VARCOLLECT:
			for {
				err = findSets(p.car, v, env, m)
				if err != nil {
					goto ERROR
				}
				nxt := p.cdr
				typ = typeOf(nxt)
				switch typ {
				case tConstant:
					if nxt == yNULL {
						break VARCOLLECT
					}
				case tPair:
					p = pair(nxt)
				default:
					err = findSets(nxt, v, env, m)
					if err != nil {
						goto ERROR
					}
					break VARCOLLECT
				}
			}
			break FINDSETSLOOP
		}
	}
	return nil

ERROR:
	return err
}

/*----------------------------------------------------
 * findLocal
 */
func findLocal(exp yObj, env locTable, m *vm, ctx int) (int, error) {
	var err error
	x := exp
	for {
		typ := typeOf(x)
		switch typ {
		case tConstant, tIntnum, tFlonum, tRatinum, tCmplnum, tChar, tString, tSymbol, tVector:
			return 0, nil
		case tPair:
			xCar := pair(x).car
			if typeOf(xCar) == tSymbol {
				op := lookUp(xCar, env, m)
				switch op {
				case tSyntaxQuote, tSyntaxLambda:
					return 0, nil
				case tSyntaxLet, tSyntaxLetseq:
					var (
						l1vars locTable
						l1vals []yObj
						l1body yObj
					)
					l1vars, l1vals, l1body, _, err = recordLet(x, false)
					if err != nil {
						goto ERROR
					}
					var locs int
					locs, err = findLocal(l1body, env, m, ctxNotopInner)
					if err != nil {
						goto ERROR
					}
					var valLocs int
					for _, val := range l1vals {
						valLocs, err = findLocal(val, env, m, ctx|ctxINNER)
						if err != nil {
							goto ERROR
						}
						if valLocs > locs {
							locs = valLocs
						}
					}
					return locs + len(l1vars), nil
				case tSyntaxLetrec:
					var (
						lrvars locTable
						lrvals []yObj
						lrbody yObj
					)
					lrvars, lrvals, lrbody, _, err = recordLet(x, true)
					if err != nil {
						goto ERROR
					}
					var locs int
					locs, err = findLocal(lrbody, env, m, ctxNotopInner)
					if err != nil {
						goto ERROR
					}
					var valLocs int
					for _, val := range lrvals {
						valLocs, err = findLocal(val, env, m, ctx|ctxINNER)
						if err != nil {
							goto ERROR
						}
						if valLocs > locs {
							locs = valLocs
						}
					}
					return locs + len(lrvars)*2, nil
				case tSyntaxIf:
					var tst, thn, els yObj
					tst, thn, els, err = recordIf(x)
					if err != nil {
						goto ERROR
					}
					var locs, l int
					locs, err = findLocal(tst, env, m, ctx|ctxINNER)
					if err != nil {
						goto ERROR
					}
					l, err = findLocal(thn, env, m, ctx|ctxINNER)
					if err != nil {
						goto ERROR
					}
					if l > locs {
						locs = l
					}
					l, err = findLocal(els, env, m, ctx|ctxINNER)
					if err != nil {
						goto ERROR
					}
					if l > locs {
						locs = l
					}
					return locs, nil
				case tSyntaxDefine:
					var defval yObj
					_, defval, err = recordDefine(x)
					if err != nil {
						goto ERROR
					}
					var locs int
					locs, err = findLocal(defval, env, m, ctx|ctxINNER)
					if err != nil {
						goto ERROR
					}
					if (ctx & ctxNOTOP) != 0 { //no top-level
						return locs + 2, nil
					}
					//top-level
					return locs, nil
				case tSyntaxSet:
					var sval yObj
					_, sval, err = recordSet(x)
					if err != nil {
						goto ERROR
					}
					x = sval
					ctx |= ctxINNER
					continue
				case tSyntaxBegin:
					var exps []yObj
					exps, err = recordBegin(x)
					if err != nil {
						goto ERROR
					}
					locs := 0
					var l int
					if (ctx & ctxNOTOP) != 0 { //no top-level
						index := 0
						for _, e := range exps {
							if isDefine(e) {
								l, err = findLocal(e, env, m, ctx|ctxINNER)
								if err != nil {
									goto ERROR
								}
								if l > locs {
									locs = l
								}
								index++
								continue
							}
							break
						}
						locs -= 2
						for _, e := range exps[index:] {
							l, err = findLocal(e, env, m, ctx|ctxINNER)
							if err != nil {
								goto ERROR
							}
							if l > locs {
								locs = l
							}
						}
						return index*2 + locs, nil
					}
					//top-level
					for _, e := range exps {
						l, err = findLocal(e, env, m, ctx|ctxINNER)
						if err != nil {
							goto ERROR
						}
						if l > locs {
							locs = l
						}
					}
					return locs, nil
				case tSyntaxCond:
					var clauses, exps []yObj
					locs, l := 0, 0
					elsSym := mkSymbol("else")
					arrSym := mkSymbol("=>")
					if typeOf(pair(x).cdr) != tPair {
						err = mkYerror(errComp, errSyntax, "malformed cond")
						goto ERROR
					}
					clauses, err = recordBegin(x)
					if err != nil {
						goto ERROR
					}
					for _, clause := range clauses {
						if typeOf(clause) != tPair {
							err = mkYerror(errComp, errSyntax, "malformed cond")
							goto ERROR
						}
						carClause := pair(clause).car
						if carClause == elsSym {
							exps, err = recordBegin(clause)
							if err != nil {
								goto ERROR
							}
							for _, e := range exps {
								l, err = findLocal(e, env, m, ctx|ctxINNER)
								if err != nil {
									goto ERROR
								}
								if l > locs {
									locs = l
								}
							}
						}
						l, err = findLocal(carClause, env, m, ctx|ctxINNER)
						if err != nil {
							goto ERROR
						}
						if l > locs {
							locs = l
						}
						cdrClause := pair(clause).cdr
						if typeOf(cdrClause) != tPair {
							continue
						}
						if pair(cdrClause).car == arrSym {
							cddrClause := pair(cdrClause).cdr
							if typeOf(cddrClause) != tPair {
								err = mkYerror(errComp, errSyntax, "malformed cond")
								goto ERROR
							}
							l, err = findLocal(pair(cddrClause).car, env, m, ctx|ctxINNER)
							if err != nil {
								goto ERROR
							}
							if l > locs {
								locs = l
							}
						}
						exps, err = recordBegin(cdrClause)
						if err != nil {
							goto ERROR
						}
						for _, e := range exps {
							l, err = findLocal(e, env, m, ctx|ctxINNER)
							if err != nil {
								goto ERROR
							}
							if l > locs {
								locs = l
							}
						}
					}
					return locs, nil
				case tSyntaxCase:
					var keyAndClauses, exps []yObj
					locs, l := 0, 0
					if typeOf(pair(x).cdr) != tPair {
						err = mkYerror(errComp, errSyntax, "malformed case")
						goto ERROR
					}
					keyAndClauses, err = recordBegin(x)
					if err != nil {
						goto ERROR
					}
					l, err = findLocal(keyAndClauses[0], env, m, ctx|ctxINNER)
					if err != nil {
						goto ERROR
					}
					if l > locs {
						locs = l
					}
					for _, clause := range keyAndClauses[1:] {
						if typeOf(clause) == tPair {
							exps, err = recordBegin(clause)
							if err != nil {
								goto ERROR
							}
							for _, e := range exps {
								l, err = findLocal(e, env, m, ctx|ctxINNER)
								if err != nil {
									goto ERROR
								}
								if l > locs {
									locs = l
								}
							}
						}
					}
					return locs, nil
				case tSyntaxDo:
					doExp := pair(x).cdr
					var (
						vars                       locTable
						inits, steps, posts, bodys []yObj
						test                       yObj
					)
					vars, inits, steps, test, posts, bodys, err = recordDo(doExp)
					if err != nil {
						goto ERROR
					}
					var locs, l int
					locs, err = findLocal(test, env, m, ctx|ctxINNER)
					if err != nil {
						goto ERROR
					}
					for _, e := range inits {
						l, err = findLocal(e, env, m, ctx|ctxINNER)
						if err != nil {
							goto ERROR
						}
						if l > locs {
							locs = l
						}
					}
					for _, e := range steps {
						if e == nil {
							continue
						}
						l, err = findLocal(e, env, m, ctx|ctxINNER)
						if err != nil {
							goto ERROR
						}
						if l > locs {
							locs = l
						}
					}
					for _, e := range posts {
						l, err = findLocal(e, env, m, ctx|ctxINNER)
						if err != nil {
							goto ERROR
						}
						if l > locs {
							locs = l
						}
					}
					for _, e := range bodys {
						l, err = findLocal(e, env, m, ctx|ctxINNER)
						if err != nil {
							goto ERROR
						}
						if l > locs {
							locs = l
						}
					}
					return locs + len(vars), nil
				case tSyntaxAnd, tSyntaxOr:
					var exps []yObj
					locs, l := 0, 0
					exps, err = recordBegin(x)
					if err != nil {
						goto ERROR
					}
					for _, e := range exps {
						l, err = findLocal(e, env, m, ctx|ctxINNER)
						if err != nil {
							goto ERROR
						}
						if l > locs {
							locs = l
						}
					}
					return locs, nil
				case tSyntaxQuasiquote:
					return findLocalInQq(x, 0, env, m, ctx)
				}
			}

			locs := 0
			p := pair(x)
			var l int
		VARCOLLECT:
			for {
				l, err = findLocal(p.car, env, m, ctx|ctxINNER)
				if err != nil {
					goto ERROR
				}
				if l > locs {
					locs = l
				}
				nxt := p.cdr
				typ = typeOf(nxt)
				switch typ {
				case tConstant:
					if nxt == yNULL {
						break VARCOLLECT
					}
					err = mkYerror(errComp, errSyntax, errListReq)
					goto ERROR
				case tPair:
					p = pair(nxt)
				default:
					err = mkYerror(errComp, errSyntax, errListReq)
					goto ERROR
				}
			}
			return locs, nil
		}
	}
ERROR:
	return 0, err
}

/*----------------------------------------------------
 * isDefine
 */
func isDefine(exp yObj) bool {
	if typeOf(exp) == tPair {
		carExp := pair(exp).car
		return carExp == mkSymbol("define")
	}
	return false
}

/*----------------------------------------------------
 * compileRefer
 */
func compileRefer(exp yObj, env locTable, m *vm, next *insn) *insn {
	nxt := next
	if bind, exist := env[exp]; exist {
		if bind.indirect {
			nxt = &insn{op: opINDIRECT, i0: nxt}
		}
		if bind.lorf == sLOCAL {
			return &insn{op: opREFERLOCAL, n0: bind.index, i0: nxt}
		}
		return &insn{op: opREFERFREE, n0: bind.index, i0: nxt}
	}

	if gRec, exist := m.env[exp]; exist {
		return &insn{op: opREFERGLOBAL, o0: yObj(gRec.gloc), i0: nxt}
	}

	return &insn{op: opLOOKUPGLOBAL, o0: exp, i0: nxt}
}

/*----------------------------------------------------
 * compileAssign
 */
func compileAssign(sym yObj, env locTable, top map[yObj]*glocRec, next *insn) (*insn, error) {
	nxt := next
	if bind, exist := env[sym]; exist {
		if bind.lorf == sLOCAL {
			return &insn{op: opASSIGNLOCAL, n0: bind.index, i0: nxt}, nil
		}
		return &insn{op: opASSIGNFREE, n0: bind.index, i0: nxt}, nil
	}

	if gRec, exist := top[sym]; exist {
		if gRec.immutable {
			return nil, mkYerror(errComp, errSyntax, "immutable variable")
		}
		return &insn{op: opASSIGNGLOBAL, o0: yObj(gRec.gloc), i0: nxt}, nil
	}

	return &insn{op: opLOOKUPASSIGNGLOBAL, o0: sym, i0: nxt}, nil
}

/*----------------------------------------------------
 * compileDefine
 */
func compileDefine(sym yObj, env locTable, m *vm, next *insn) (*insn, error) {
	nxt := next
	if gRec, exist := m.env[sym]; exist {
		if gRec.immutable {
			return nil, mkYerror(errComp, errSyntax, "immutable variable")
		}
		return &insn{op: opASSIGNGLOBAL, o0: yObj(gRec.gloc), i0: nxt}, nil
	}

	di := defInfo{sym: sym, gloc: new(yObj)}
	return &insn{op: opDEFINE, o0: yObj(&di), i0: nxt}, nil
}

/*----------------------------------------------------
 * compileFuncall
 */
func compileFuncall(x yObj, env locTable, m *vm, ctx int, nx *insn) (*insn, error) {
	var (
		args             []yObj
		argc             int
		nxt, funcall     *insn
		needLocExt, tail bool
		locc             int
		err              error
	)

	args, err = recordBegin(x)
	if err != nil {
		goto ERROR
	}
	argc = len(args)
	nxt = nx
	locc, err = findLocal(x, env, m, ctx)
	if err != nil {
		goto ERROR
	}
	needLocExt = (ctx == ctxTopOuter && locc > 0)
	if needLocExt {
		nxt = &insn{op: opSHRINK, n0: locc, i0: nxt}
	}
	funcall = &insn{op: opAPPLY}
	tail = (nx.op == opRETURN)
	if tail {
		funcall = &insn{op: opSHIFT, n0: argc, n1: nx.n0, i0: funcall}
	}
	funcall, err = compile(pair(x).car, env, m, ctx|ctxINNER, funcall)
	if err != nil {
		goto ERROR
	}
	for i := argc - 1; i > -1; i-- {
		funcall, err = compile(args[i], env, m, ctx|ctxINNER, &insn{op: opARGUMENT, i0: funcall})
		if err != nil {
			goto ERROR
		}
	}
	if tail {
		nxt = funcall
	} else {
		nxt = &insn{op: opFRAME, i0: nxt, i1: funcall}
	}
	if needLocExt {
		nxt = &insn{op: opEXTEND, n0: locc, i0: nxt}
	}
	return nxt, nil

ERROR:
	return nil, err
}

/*----------------------------------------------------
 * compileProc
 */
func compileProc(x yObj, env locTable, m *vm, ctx int, next *insn) (*insn, error) {
	var (
		args []yObj
		argc int
		nxt  *insn
		err  error
	)
	errNumOfArgMis := mkYerror(errComp, errSyntax, "num of argument mismatch")
	typ := (*procedure)((*yCell)(*(*yObj)(m.env[pair(x).car].gloc)).cdr).typ
	if typ == 1000 {
		goto FUNCALL
	}
	args, err = recordBegin(x)
	if err != nil {
		goto ERROR
	}
	nxt = next
	argc = len(args)
	switch typ {
	case tADD:
		if argc == 0 {
			return &insn{op: opCONSTANT, o0: yZERO, i0: nxt}, nil
		}
		if argc == 1 {
			return compile(args[0], env, m, ctx, nxt)
		}
		for i := argc - 1; i > 1; i-- {
			nxt, err = compile(args[i], env, m, ctx, &insn{op: opADD2, i0: nxt})
			if err != nil {
				goto ERROR
			}
			nxt = &insn{op: opARGUMENT, i0: nxt}
		}
		nxt, err = compile(args[1], env, m, ctx, &insn{op: opADD2, i0: nxt})
		if err != nil {
			goto ERROR
		}
		return compile(args[0], env, m, ctx, &insn{op: opARGUMENT, i0: nxt})
	case tSUB:
		if argc == 0 {
			err = errNumOfArgMis
			goto ERROR
		}
		if argc == 1 {
			args = append(args, args[0])
			args[0] = yZERO
		}
		for i := argc - 1; i > 1; i-- {
			nxt, err = compile(args[i], env, m, ctx, &insn{op: opSUB2, i0: nxt})
			if err != nil {
				goto ERROR
			}
			nxt = &insn{op: opARGUMENT, i0: nxt}
		}
		nxt, err = compile(args[1], env, m, ctx, &insn{op: opSUB2, i0: nxt})
		if err != nil {
			goto ERROR
		}
		return compile(args[0], env, m, ctx, &insn{op: opARGUMENT, i0: nxt})
	case tNUMEQ: //if not binary operation, should compile all arguments and apply LSS proc.
		if argc < 2 {
			err = errNumOfArgMis
			goto ERROR
		}
		if argc == 2 {
			nxt, err = compile(args[1], env, m, ctx, &insn{op: opEQN, i0: nxt})
			if err != nil {
				goto ERROR
			}
			return compile(args[0], env, m, ctx, &insn{op: opARGUMENT, i0: nxt})
		}
		goto FUNCALL
	case tLSS: //if not binary operation, should compile all arguments and apply LSS proc.
		if argc < 2 {
			err = errNumOfArgMis
			goto ERROR
		}
		if argc == 2 {
			nxt, err = compile(args[1], env, m, ctx, &insn{op: opLSS, i0: nxt})
			if err != nil {
				goto ERROR
			}
			return compile(args[0], env, m, ctx, &insn{op: opARGUMENT, i0: nxt})
		}
		n := nxt
		for i := argc - 1; i > 0; i-- {
			nxt = &insn{op: opTEST, i0: nxt, i1: n}
			nxt, err = compile(args[i], env, m, ctx, &insn{op: opLSS, i0: nxt})
			if err != nil {
				goto ERROR
			}
			nxt, err = compile(args[i-1], env, m, ctx, &insn{op: opARGUMENT, i0: nxt})
			if err != nil {
				goto ERROR
			}
		}
		return nxt, nil
	case tMEMV:
		if argc != 2 {
			err = errNumOfArgMis
			goto ERROR
		}
		nxt, err = compile(args[1], env, m, ctx, &insn{op: opMEMV, i0: nxt})
		if err != nil {
			goto ERROR
		}
		return compile(args[0], env, m, ctx, &insn{op: opARGUMENT, i0: nxt})
	case tLST2VEC:
		if argc != 1 {
			err = errNumOfArgMis
			goto ERROR
		}
		return compile(args[0], env, m, ctx, &insn{op: opLST2VEC, i0: nxt})
	case tPAIRP:
		if argc != 1 {
			err = errNumOfArgMis
			goto ERROR
		}
		return compile(args[0], env, m, ctx, &insn{op: opPAIRP, i0: nxt})
	case tNULLP:
		if argc != 1 {
			err = errNumOfArgMis
			goto ERROR
		}
		return compile(args[0], env, m, ctx, &insn{op: opNULLP, i0: nxt})
	case tCONS:
		if argc != 2 {
			err = errNumOfArgMis
			goto ERROR
		}
		nxt, err = compile(args[1], env, m, ctx, &insn{op: opCONS, i0: nxt})
		if err != nil {
			goto ERROR
		}
		return compile(args[0], env, m, ctx, &insn{op: opARGUMENT, i0: nxt})
	case tCAR:
		if argc != 1 {
			err = errNumOfArgMis
			goto ERROR
		}
		return compile(args[0], env, m, ctx, &insn{op: opCAR, i0: nxt})
	case tCDR:
		if argc != 1 {
			err = errNumOfArgMis
			goto ERROR
		}
		return compile(args[0], env, m, ctx, &insn{op: opCDR, i0: nxt})
	case tLIST:
		if argc == 0 {
			return &insn{op: opCONSTANT, o0: yNULL, i0: nxt}, nil
		}
		if argc == 1 {
			return compile(args[0], env, m, ctx, &insn{op: opLIST, n0: 1, i0: nxt})
		}
		nxt, err = compile(args[argc-1], env, m, ctx, &insn{op: opLIST, n0: argc, i0: nxt})
		if err != nil {
			goto ERROR
		}
		for i := argc - 2; i > 0; i-- {
			nxt, err = compile(args[i], env, m, ctx, &insn{op: opARGUMENT, i0: nxt})
			if err != nil {
				goto ERROR
			}
		}
		return compile(args[0], env, m, ctx, &insn{op: opARGUMENT, i0: nxt})
	case tLENGTH:
		if argc != 1 {
			err = errNumOfArgMis
			goto ERROR
		}
		return compile(args[0], env, m, ctx, &insn{op: opLENGTH, i0: nxt})
	case tAPPEND:
		if argc == 0 {
			return &insn{op: opCONSTANT, o0: yNULL, i0: nxt}, nil
		}
		nxt, err = compile(args[argc-1], env, m, ctx, &insn{op: opAPPEND, n0: argc, i0: nxt})
		if err != nil {
			goto ERROR
		}
		for i := 0; i < argc-1; i++ {
			nxt, err = compile(args[i], env, m, ctx, &insn{op: opARGUMENT, i0: nxt})
			if err != nil {
				goto ERROR
			}
		}
		return nxt, nil
	case tEQV:
		if argc != 2 {
			err = errNumOfArgMis
			goto ERROR
		}
		nxt, err = compile(args[1], env, m, ctx, &insn{op: opEQV, i0: nxt})
		if err != nil {
			goto ERROR
		}
		return compile(args[0], env, m, ctx, &insn{op: opARGUMENT, i0: nxt})
	}

FUNCALL:
	return compileFuncall(x, env, m, ctx, next)
ERROR:
	return nil, err
}

/*----------------------------------------------------
 * makeClose
 */
func makeClose(frees locTable, vars locTable, body yObj, reqc int, optc int, locc int, m *vm, next *insn) (*insn, error) {
	var (
		err      error
		nxt, bdy *insn
		tail     **insn
		newEnv   locTable = make(locTable)
		i        int
	)

	for key, val := range frees {
		if val.lorf == sLOCAL {
			nxt = &insn{op: opREFERLOCAL, n0: val.index, i0: &insn{op: opARGUMENT, i0: nxt}}
			if tail == nil {
				tail = &nxt.i0.i0
			}
		} else {
			nxt = &insn{op: opREFERFREE, n0: val.index, i0: &insn{op: opARGUMENT, i0: nxt}}
			if tail == nil {
				tail = &nxt.i0.i0
			}
		}

		newEnv[key] = &varInfo{index: i, lorf: sFREE, indirect: val.indirect}
		i++
	}
	for key, val := range vars {
		newEnv[key] = val
	}
	argc := [2]int{reqc, optc}
	bdy, err = compile(body, newEnv, m, ctxNotopInner, &insn{op: opRETURN, n0: reqc + optc + locc})
	if err != nil {
		return nil, err
	}
	closeInst := &insn{op: opCLOSE,
		n0: len(frees),
		n1: locc,
		i0: makeBoxes(vars, bdy),
		o0: yObj(&argc),
		i1: next}
	if i > 0 {
		*tail = closeInst
	} else {
		nxt = closeInst
	}

	return nxt, nil
}

/*----------------------------------------------------
 * compileLambda
 */
func compileLambda(vars locTable, body yObj, reqc int, optc int, env locTable, m *vm, ctx int, nx *insn) (*insn, error) {
	var (
		err        error
		ext        int
		carBody    yObj
		letrecBody yObj
		letBody    yObj
		extVars    locTable
		free       locTable
		closePart  *insn  = nil
		newEnv            = make(locTable)
		tail       **insn = nil
		numFree    int    = 0
		nxt        *insn
		argc       [2]int
		closeInst  *insn
	)

	symDef := mkSymbol("define")
	bdy := body
	defVars := make(locTable)
	defVals := make([]yObj, 0)

	var locc int
	locc, err = findLocal(bdy, env, m, ctxNotopInner)
	if err != nil {
		goto ERROR
	}
	ext = reqc + optc

	if typeOf(bdy) != tPair {
		goto NODEFINE
	}
	carBody = pair(bdy).car
	if typeOf(carBody) != tSymbol {
		goto NODEFINE
	}

	//case of body = (define XX) , since letrec body is null, lambda body = nil (do nothing)
	if carBody == symDef {
		var vr, vl yObj
		vr, vl, err = recordDefine(bdy)
		if err != nil {
			goto ERROR
		}
		defVars[vr] = &varInfo{index: 0}
		defVals = append(defVals, vl)
		bdy = nil
	}

	//case of body = (begin (XX ...) ...)
	if carBody == mkSymbol("begin") {
		for i := 0; ; i++ {
			bdy = pair(bdy).cdr
			begElt := pair(bdy).car
			if typeOf(begElt) != tPair {
				break
			}
			carBegElt := pair(begElt).car
			if typeOf(carBegElt) != tSymbol {
				break
			}
			if carBegElt != symDef {
				break
			}
			var vr, vl yObj
			vr, vl, err = recordDefine(begElt)
			if err != nil {
				goto ERROR
			}
			defVars[vr] = &varInfo{index: i}
			defVals = append(defVals, vl)
		}
	}

	if len(defVars) == 0 { //if no (define), return nil
		goto NODEFINE
	}

	if bdy != nil {
		letrecBody = maybeBegin(bdy)
	}
	letBody = makeLetrecBody(defVars, defVals, letrecBody)

	extVars, _ = locTblExtend(vars, defVars)
	free, err = findFree(letBody, extVars, env, m)
	if err != nil {
		goto ERROR
	}
	err = findSets(letBody, extVars, env, m)
	if err != nil {
		goto ERROR
	}

	for key, val := range free {
		if val.lorf == sLOCAL {
			closePart = &insn{op: opREFERLOCAL, n0: val.index, i0: &insn{op: opARGUMENT, i0: closePart}}
			if tail == nil {
				tail = &closePart.i0.i0
			}
		} else {
			closePart = &insn{op: opREFERFREE, n0: val.index, i0: &insn{op: opARGUMENT, i0: closePart}}
			if tail == nil {
				tail = &closePart.i0.i0
			}
		}

		newEnv[key] = &varInfo{index: numFree, lorf: sFREE, indirect: val.indirect}
		numFree++
	}

	for key, val := range extVars {
		newEnv[key] = val
	}
	nxt, err = compile(letBody, newEnv, m, ctxNotopInner, &insn{op: opRETURN, n0: ext + locc})
	if err != nil {
		return nil, err
	}
	nxt = makeBoxes(extVars, nxt)
	for i, _ := range defVals { //for outer let (body of lambda)
		nxt = &insn{op: opCONSTANT, o0: yUNDEFINED, i0: &insn{op: opPUSHLOCAL, n0: i + ext, i0: nxt}}
	}

	argc = [2]int{reqc, optc}

	closeInst = &insn{op: opCLOSE,
		n0: len(free),
		n1: locc,
		i0: nxt,
		o0: yObj(&argc),
		i1: nx}

	if numFree > 0 {
		*tail = closeInst
	} else {
		closePart = closeInst
	}

	return closePart, nil

NODEFINE:
	free, err = findFree(body, vars, env, m)
	if err != nil {
		goto ERROR
	}
	err = findSets(body, vars, env, m)
	if err != nil {
		goto ERROR
	}
	return makeClose(free, vars, body, reqc, optc, locc, m, nx)

ERROR:
	return nil, err
}

/*----------------------------------------------------
 * compileLetBody
 */
func compileLetBody(body yObj, env locTable, m *vm, nx *insn) (*insn, error) {
	var (
		err                 error
		letrecBody, letBody yObj
		extEnv, modDefVars  locTable
		nxt                 *insn
		ext                 int
	)
	symDef := mkSymbol("define")
	bdy := body
	defVars := make(locTable)
	defVals := make([]yObj, 0)

	if typeOf(bdy) != tPair {
		return compile(body, env, m, ctxNotopInner, nx)
	}
	carBody := pair(bdy).car
	if typeOf(carBody) != tSymbol {
		return compile(body, env, m, ctxNotopInner, nx)
	}

	//case of body = (define X Y) , since letrec body is null, just only eval Y and next.
	if carBody == symDef {
		var vr, vl yObj
		vr, vl, err = recordDefine(bdy)
		if err != nil {
			goto ERROR
		}
		defVars[vr] = &varInfo{index: 0}
		defVals = append(defVals, vl)
		bdy = nil
	}

	//case of body = (begin (XX ...) ...)
	if carBody == mkSymbol("begin") {
		for i := 0; ; i++ {
			bdy = pair(bdy).cdr
			begElt := pair(bdy).car
			if typeOf(begElt) != tPair {
				break
			}
			carBegElt := pair(begElt).car
			if typeOf(carBegElt) != tSymbol {
				break
			}
			if carBegElt != symDef {
				break
			}
			var vr, vl yObj
			vr, vl, err = recordDefine(begElt)
			if err != nil {
				goto ERROR
			}
			defVars[vr] = &varInfo{index: i}
			defVals = append(defVals, vl)
		}
	}

	if len(defVars) == 0 { //if no (define), return nil
		return compile(body, env, m, ctxNotopInner, nx)
	}

	if bdy != nil {
		letrecBody = maybeBegin(bdy)
	}
	letBody = makeLetrecBody(defVars, defVals, letrecBody)
	err = findSets(letBody, defVars, env, m)
	if err != nil {
		goto ERROR
	}
	extEnv, modDefVars = locTblExtend(env, defVars)
	nxt, err = compile(letBody, extEnv, m, ctxNotopInner, nx)
	if err != nil {
		return nil, err
	}
	nxt = makeBoxes(modDefVars, nxt)

	ext = len(env)
	for i, _ := range defVals {
		nxt = &insn{op: opCONSTANT, o0: yUNDEFINED, i0: &insn{op: opPUSHLOCAL, n0: ext + i, i0: nxt}}
	}

	return nxt, nil

ERROR:
	return nil, err
}

/*----------------------------------------------------
 * makeBoxes
 */
func makeBoxes(vars locTable, next *insn) *insn {
	nxt := next
	for _, val := range vars {
		if val.indirect {
			nxt = &insn{op: opBOX, n0: val.index, i0: nxt}
		}
	}
	return nxt
}

/*----------------------------------------------------
 * compileApply
 */
func compileApply(vals []yObj, env locTable, m *vm, lv int, next *insn) (*insn, error) {
	var err error
	nxt := next
	for _, val := range vals {
		nxt, err = compile(val, env, m, lv, &insn{op: opARGUMENT, i0: nxt})
		if err != nil {
			return nil, err
		}
	}
	return nxt, nil
}

/*----------------------------------------------------
 * compileCond
 */
func compileCond(clauses []yObj, env locTable, m *vm, lv int, nx *insn) (*insn, error) {
	var (
		err       error
		carClause yObj
	)
	condErr := mkYerror(errComp, errSyntax, "malformed cond")
	clause := clauses[0]

	if typeOf(clause) != tPair {
		err = condErr
		goto ERROR
	}

	carClause = pair(clause).car
	// else clause
	if carClause == mkSymbol("else") {
		if len(clauses) > 1 {
			err = condErr
			goto ERROR
		}
		cdrClause := pair(clause).cdr
		if typeOf(cdrClause) != tPair || length(cdrClause) < 1 {
			err = condErr
			goto ERROR
		}
		var exps []yObj
		exps, err = recordBegin(clause)
		if err != nil {
			goto ERROR
		}
		nxt := nx
		for i := len(exps) - 1; i >= 0; i-- {
			nxt, err = compile(exps[i], env, m, lv, nxt)
			if err != nil {
				goto ERROR
			}
		}
		return nxt, nil
	}

	// normal clause
	if len(clauses) > 1 { //none last clause
		var thnc, elsc *insn
		if length(clause) > 1 { // (test exp1 exp2 ...) or (test => exp)
			cdrClause := pair(clause).cdr
			cadrClause := pair(cdrClause).car
			if cadrClause == mkSymbol("=>") { // (test => exp)
				if length(clause) != 3 {
					err = condErr
					goto ERROR
				}
				thnc = &insn{op: opAPPLY}
				tail := (nx.op == opRETURN)
				if tail {
					thnc = &insn{op: opSHIFT, n0: 1, n1: nx.n0, i0: thnc}
				}
				thnc, err = compile(pair(pair(cdrClause).cdr).car, env, m, lv, thnc)
				if err != nil {
					goto ERROR
				}
				thnc = &insn{op: opARGUMENT, i0: thnc} // carClause is still in acum
				if !tail {
					thnc = &insn{op: opFRAME, i0: nx, i1: thnc}
				}
			} else { // (test exp1 exp2 ...)
				var exps []yObj
				exps, err = recordBegin(clause)
				if err != nil {
					goto ERROR
				}
				thnc = nx
				for i := len(exps) - 1; i >= 0; i-- {
					thnc, err = compile(exps[i], env, m, lv, thnc)
					if err != nil {
						goto ERROR
					}
				}
			}

			elsc, err = compileCond(clauses[1:], env, m, lv, nx)
			if err != nil {
				goto ERROR
			}
			nxt := &insn{op: opTEST, i0: thnc, i1: elsc}
			return compile(carClause, env, m, lv, nxt)
		}
		//(test)
		elsc, err = compileCond(clauses[1:], env, m, lv, nx)
		if err != nil {
			goto ERROR
		}
		thnc = nx // carClause is still in acum
		nxt := &insn{op: opTEST, i0: thnc, i1: elsc}
		return compile(carClause, env, m, lv, nxt)
	}

	//last clause
	if length(clause) > 1 { // (test exp1 exp2 ...) or (test => exp)
		cdrClause := pair(clause).cdr
		cadrClause := pair(cdrClause).car
		var thnc, elsc *insn
		if cadrClause == mkSymbol("=>") { // (test => exp)
			if length(clause) != 3 {
				err = condErr
				goto ERROR
			}
			elsc = &insn{op: opCONSTANT, o0: yUNDEFINED, i0: nx}
			thnc = &insn{op: opAPPLY}
			tail := (nx.op == opRETURN)
			if tail {
				thnc = &insn{op: opSHIFT, n0: 1, n1: nx.n0, i0: thnc}
			}
			thnc, err = compile(pair(pair(cdrClause).cdr).car, env, m, lv, thnc)
			if err != nil {
				goto ERROR
			}
			thnc = &insn{op: opARGUMENT, i0: thnc} // carClause is still in acum
			if !tail {
				thnc = &insn{op: opFRAME, i0: nx, i1: thnc}
			}
		} else { // (test exp1 exp2 ...)
			elsc = &insn{op: opCONSTANT, o0: yUNDEFINED, i0: nx}
			var exps []yObj
			exps, err = recordBegin(clause)
			if err != nil {
				goto ERROR
			}
			thnc = nx
			for i := len(exps) - 1; i >= 0; i-- {
				thnc, err = compile(exps[i], env, m, lv, thnc)
				if err != nil {
					goto ERROR
				}
			}
		}

		nxt := &insn{op: opTEST, i0: thnc, i1: elsc}
		return compile(carClause, env, m, lv, nxt)
	}
	//(test)
	return compile(carClause, env, m, lv, nx)

ERROR:
	return nil, err
}

/*----------------------------------------------------
 * compileCase
 */
func compileCase(keyAndClauses []yObj, env locTable, m *vm, ctx int, first bool, nx *insn) (*insn, error) {
	var (
		err                     error
		fstClause, carFstClause yObj
		nxt                     *insn
		typ                     int64
	)

	caseErr := mkYerror(errComp, errSyntax, "malformed case")
	key := keyAndClauses[0]
	numOfElm := len(keyAndClauses)

	if numOfElm < 2 {
		err = caseErr
		goto ERROR
	}
	fstClause = keyAndClauses[1]
	if typeOf(fstClause) != tPair {
		err = caseErr
		goto ERROR
	}

	nxt = nx
	carFstClause = pair(fstClause).car
	typ = typeOf(carFstClause)

	// else clause
	if typ == tSymbol {
		if carFstClause == mkSymbol("else") {
			if numOfElm > 2 {
				err = caseErr
				goto ERROR
			}
			cdrFstClause := pair(fstClause).cdr
			if length(cdrFstClause) < 1 {
				err = caseErr
				goto ERROR
			}
			if first {
				nxt, err = compile(yObj(&yCell{car: mkSymbol("begin"), cdr: cdrFstClause}),
					env, m, ctx, nxt)
				if err != nil {
					goto ERROR
				}
				return compile(key, env, m, ctx, nxt)
			}
			return compile(yObj(&yCell{car: mkSymbol("begin"), cdr: cdrFstClause}),
				env, m, ctx, nxt)
		}
		err = caseErr
		goto ERROR
	}

	//normal clause
	if typ == tPair || carFstClause == yNULL {
		if numOfElm == 2 { //last clause
			cdrFstClause := pair(fstClause).cdr
			if length(cdrFstClause) < 1 {
				err = caseErr
				goto ERROR
			}
			var thn *insn
			thn, err = compile(yObj(&yCell{car: mkSymbol("begin"), cdr: cdrFstClause}),
				env, m, ctx|ctxINNER, nxt)
			if err != nil {
				goto ERROR
			}
			nxt = &insn{op: opTEST, i0: thn, i1: &insn{op: opCONSTANT, o0: yUNDEFINED, i0: nxt}}
			nxt = &insn{op: opMEMV, i0: nxt}
			if first {
				nxt = &insn{op: opARGUMENT, i0: &insn{op: opCONSTANT, o0: carFstClause, i0: nxt}}
				return compile(key, env, m, ctx, nxt)
			}
			return &insn{op: opRECOVER, n0: 1, i0: &insn{op: opCONSTANT, o0: carFstClause, i0: nxt}}, nil
		}
		if numOfElm > 2 { //none last clause
			cdrFstClause := pair(fstClause).cdr
			if length(cdrFstClause) < 1 {
				err = caseErr
				goto ERROR
			}
			newKeyAndClauses := make([]yObj, numOfElm-1)
			newKeyAndClauses[0] = key
			for i, obj := range keyAndClauses[2:] {
				newKeyAndClauses[i+1] = obj
			}
			var els *insn
			els, err = compileCase(newKeyAndClauses, env, m, ctx, false, nxt)
			if err != nil {
				goto ERROR
			}
			var thn *insn
			thn, err = compile(yObj(&yCell{car: mkSymbol("begin"), cdr: cdrFstClause}),
				env, m, ctx, nxt)
			if err != nil {
				goto ERROR
			}
			nxt = &insn{op: opTEST, i0: thn, i1: els}
			nxt = &insn{op: opMEMV, i0: nxt}
			if first {
				nxt = &insn{op: opARGUMENT, i0: &insn{op: opCONSTANT, o0: carFstClause, i0: nxt}}
				return compile(key, env, m, ctx, nxt)
			}
			return &insn{op: opRECOVER, n0: 1, i0: &insn{op: opCONSTANT, o0: carFstClause, i0: nxt}}, nil
		}
		err = caseErr
		goto ERROR
	}
ERROR:
	return nil, err
}

/*----------------------------------------------------
 * recordDo
 */
func recordDo(exp yObj) (vars locTable, inits []yObj, steps []yObj, test yObj, posts []yObj, bodys []yObj, err error) {
	var (
		expRest, testPart, postList yObj
	)
	doErr := mkYerror(errComp, errSyntax, "malformed do")
	vars = make(locTable)
	inits = make([]yObj, 0)
	steps = make([]yObj, 0)
	posts = make([]yObj, 0)
	bodys = make([]yObj, 0)

	varList := pair(exp).car

	i := 0
	for { //making vars,inits,steps
		if varList == yNULL {
			break
		}
		if typeOf(varList) == tPair {
			clause := pair(varList).car
			typ := typeOf(clause)
			if typ == tPair {
				vr := pair(clause).car
				if typeOf(vr) != tSymbol {
					err = doErr
					goto ERROR
				}
				rest := pair(clause).cdr
				if typeOf(rest) != tPair {
					err = doErr
					goto ERROR
				}
				init := pair(rest).car
				rest = pair(rest).cdr
				var step yObj
				if rest == yNULL {
					step = nil
					goto MAKE
				} else if typeOf(rest) == tPair {
					step = pair(rest).car
				} else {
					err = doErr
					goto ERROR
				}
				if pair(rest).cdr != yNULL {
					err = doErr
					goto ERROR
				}
			MAKE:
				vars[vr] = &varInfo{index: i}
				i++
				inits = append(inits, init)
				steps = append(steps, step)

				varList = pair(varList).cdr
				continue
			}
			err = doErr
			goto ERROR
		}
		err = doErr
		goto ERROR
	}

	expRest = pair(exp).cdr
	if typeOf(expRest) != tPair {
		err = doErr
		goto ERROR
	}

	testPart = pair(expRest).car
	if typeOf(testPart) != tPair {
		err = doErr
		goto ERROR
	}

	test = pair(testPart).car //making test

	postList = pair(testPart).cdr

	for { //making posts
		if postList == yNULL {
			break
		}
		if typeOf(postList) == tPair {
			posts = append(posts, pair(postList).car)
			postList = pair(postList).cdr
			continue
		}
		err = doErr
		goto ERROR
	}

	expRest = pair(expRest).cdr

	for { //making bodys
		if expRest == yNULL {
			break
		}
		if typeOf(expRest) == tPair {
			bodys = append(bodys, pair(expRest).car)
			expRest = pair(expRest).cdr
			continue
		}
		err = doErr
		goto ERROR
	}

	return vars, inits, steps, test, posts, bodys, nil

ERROR:
	return nil, nil, nil, nil, nil, nil, err
}

/*----------------------------------------------------
 * compileDo
 */
func compileDo(exp yObj, env locTable, m *vm, ctx int, nx *insn) (*insn, error) {
	var (
		err                        error
		vars, extEnv, modVars      locTable
		inits, steps, posts, bodys []yObj
		test                       yObj
		ext                        int
		branch, tst, els, thn, nxt *insn
	)
	vars, inits, steps, test, posts, bodys, err = recordDo(exp)
	if err != nil {
		goto ERROR
	}

	extEnv, modVars = locTblExtend(env, vars)
	ext = env.numOfLocal()

	err = findSets(test, modVars, env, m)
	if err != nil {
		goto ERROR
	}

	for _, e := range steps {
		if e == nil {
			continue
		}
		err = findSets(e, modVars, env, m)
		if err != nil {
			goto ERROR
		}
	}

	for _, e := range posts {
		err = findSets(e, modVars, env, m)
		if err != nil {
			goto ERROR
		}
	}

	for _, e := range bodys {
		err = findSets(e, modVars, env, m)
		if err != nil {
			goto ERROR
		}
	}

	for _, info := range modVars {
		if steps[info.index-ext] == nil {
			continue
		}
		info.indirect = true
	}

	branch = &insn{op: opTEST, i0: nil, i1: nil}

	tst, err = compile(test, extEnv, m, ctx, branch)
	if err != nil {
		goto ERROR
	}

	els = tst
	for _, info := range modVars {
		idx := info.index - ext
		if steps[idx] == nil {
			continue
		}
		els = &insn{op: opASSIGNLOCAL, n0: info.index, i0: els}
		els, err = compile(steps[info.index-ext], extEnv, m, ctx, els)
		if err != nil {
			goto ERROR
		}
	}

	for i := len(bodys) - 1; i > -1; i-- {
		els, err = compile(bodys[i], extEnv, m, ctx, els)
		if err != nil {
			goto ERROR
		}
	}

	thn = nx
	if len(posts) == 0 {
		thn = &insn{op: opCONSTANT, o0: yUNDEFINED, i0: thn}
	}
	for i := len(posts) - 1; i > -1; i-- {
		thn, err = compile(posts[i], extEnv, m, ctx, thn)
		if err != nil {
			goto ERROR
		}
	}

	branch.i0 = thn
	branch.i1 = els

	nxt = makeBoxes(modVars, tst)

	for _, info := range modVars {
		nxt = &insn{op: opPUSHLOCAL, n0: info.index, i0: nxt}
		nxt, err = compile(inits[info.index-ext], env, m, ctx, nxt)
		if err != nil {
			goto ERROR
		}
	}

	return nxt, nil

ERROR:
	return nil, err
}

/*----------------------------------------------------
 * compileQq
 */
func compileQq(x yObj, qLv int, env locTable, m *vm, ctx int, nx *insn) (*insn, error) {
	var (
		err error
		exp yObj
		nxt *insn
		typ int64
	)
	qqErr := mkYerror(errComp, errSyntax, "malformed quasi-quote")
	uqErr := mkYerror(errComp, errSyntax, "malformed unquote")

	if qLv < 0 {
		err = uqErr
		goto ERROR
	}
	exp = x
	nxt = nx

	typ = typeOf(exp)
	switch typ {
	case tIntnum, tFlonum, tRatinum, tCmplnum, tChar, tString, tConstant:
		return &insn{op: opCONSTANT, o0: exp, i0: nxt}, nil
	case tSymbol:
		if qLv == 0 {
			return compile(exp, env, m, ctx, nxt)
		}
		return &insn{op: opCONSTANT, o0: exp, i0: nxt}, nil
	case tPair:
		xCar := pair(exp).car
		lenX := length(exp)

		if qLv == 0 {
			if typeOf(xCar) == tSymbol {
				if xCar == yQq {
					if lenX != 2 {
						err = qqErr
						goto ERROR
					}
					return compileQq(pair(pair(exp).cdr).car, qLv+1, env, m, ctx, nxt)
				}
				if xCar == yUq || xCar == yUqSplicing {
					err = uqErr
					goto ERROR
				}
			}
			return compile(exp, env, m, ctx, nxt)
		}
		// qLv > 0
		if xCar == yUq || xCar == yUqSplicing {
			if lenX != 2 {
				err = uqErr
				goto ERROR
			}
			if qLv > 1 {
				qLv--
				goto LISTPROC
			}
			if qLv == 1 {
				return compile(pair(pair(exp).cdr).car, env, m, ctx, nxt)
			}
		}

		if xCar == yQq {
			if lenX != 2 {
				err = qqErr
				goto ERROR
			}
			nxt = &insn{op: opLIST, n0: 2, i0: nxt}
			nxt, err = compileQq(pair(pair(exp).cdr).car, qLv+1, env, m, ctx, nxt)
			if err != nil {
				goto ERROR
			}
			nxt = &insn{op: opCONSTANT, o0: yQq, i0: &insn{op: opARGUMENT, i0: nxt}}
			return nxt, nil
		}

	LISTPROC:
		lstElm := make([]yObj, lenX)
		for i := 0; i < lenX; i++ {
			lstElm[i] = pair(exp).car
			exp = pair(exp).cdr
		}
		for i := 0; i < lenX; i++ {
			e := lstElm[i]
			if typeOf(e) == tPair && pair(e).car == yUqSplicing && qLv == 1 {
				if length(e) != 2 {
					err = uqErr
					goto ERROR
				}
				nxt = &insn{op: opAPPEND, n0: 2, i0: nxt}
			} else {
				nxt = &insn{op: opCONS, i0: nxt}
			}
		}
		nxt = &insn{op: opCONSTANT, o0: yNULL, i0: nxt}
		for i := lenX - 1; i > -1; i-- {
			nxt, err = compileQq(lstElm[i], qLv, env, m, ctx, &insn{op: opARGUMENT, i0: nxt})
			if err != nil {
				goto ERROR
			}
		}
		return nxt, nil
	default:
		err = qqErr
		goto ERROR
	}

ERROR:
	return nil, err
}

/*----------------------------------------------------
 * findFreeInQq
 */
func findFreeInQq(x yObj, qLv int, vars locTable, env locTable, m *vm) (locTable, error) {
	var (
		err       error
		exp       yObj
		typ       int64
		fr, frees locTable
	)
	qqErr := mkYerror(errComp, errSyntax, "malformed quasi-quote")
	uqErr := mkYerror(errComp, errSyntax, "malformed unquote")

	if qLv < 0 {
		err = uqErr
		goto ERROR
	}
	exp = x

	typ = typeOf(exp)
	switch typ {
	case tIntnum, tFlonum, tChar, tString, tConstant:
		return make(locTable), nil
	case tSymbol:
		if qLv == 0 {
			return findFree(exp, vars, env, m)
		}
		return make(locTable), nil
	case tPair:
		xCar := pair(exp).car
		lenX := length(exp)

		if qLv == 0 {
			if typeOf(xCar) == tSymbol {
				if xCar == yQq {
					if lenX != 2 {
						err = qqErr
						goto ERROR
					}
					return findFreeInQq(pair(pair(exp).cdr).car, qLv+1, vars, env, m)
				}
				if xCar == yUq || xCar == yUqSplicing {
					err = uqErr
					goto ERROR
				}
			}
			return findFree(exp, vars, env, m)
		}
		// qLv > 0
		if xCar == yUq || xCar == yUqSplicing {
			if lenX != 2 {
				err = uqErr
				goto ERROR
			}
			if qLv > 1 {
				qLv--
				goto LISTPROC
			}
			if qLv == 1 {
				return findFree(pair(pair(exp).cdr).car, vars, env, m)
			}
		}

		if xCar == yQq {
			if lenX != 2 {
				err = qqErr
				goto ERROR
			}
			return findFreeInQq(pair(pair(exp).cdr).car, qLv+1, vars, env, m)
		}

	LISTPROC:
		for i := 0; i < lenX; i++ {
			fr, err = findFreeInQq(pair(exp).car, qLv, vars, env, m)
			if err != nil {
				goto ERROR
			}
			frees = locTblUnion(frees, fr)
			exp = pair(exp).cdr
		}
		return frees, nil
	default:
		err = qqErr
		goto ERROR
	}

ERROR:
	return nil, err
}

/*----------------------------------------------------
 * findSetsInQq
 */
func findSetsInQq(x yObj, qLv int, vars locTable, env locTable, m *vm) error {
	var (
		err error
		exp yObj
		typ int64
	)
	qqErr := mkYerror(errComp, errSyntax, "malformed quasi-quote")
	uqErr := mkYerror(errComp, errSyntax, "malformed unquote")

	if qLv < 0 {
		err = uqErr
		goto ERROR
	}
	exp = x

	typ = typeOf(exp)
	switch typ {
	case tIntnum, tFlonum, tRatinum, tCmplnum, tChar, tString, tConstant, tSymbol:
		return nil
	case tPair:
		xCar := pair(exp).car
		lenX := length(exp)

		if qLv == 0 {
			if typeOf(xCar) == tSymbol {
				if xCar == yQq {
					if lenX != 2 {
						err = qqErr
						goto ERROR
					}
					return findSetsInQq(pair(pair(exp).cdr).car, qLv+1, vars, env, m)
				}
				if xCar == yUq || xCar == yUqSplicing {
					err = uqErr
					goto ERROR
				}
			}
			return findSets(exp, vars, env, m)
		}
		// qLv > 0
		if xCar == yUq || xCar == yUqSplicing {
			if lenX != 2 {
				err = uqErr
				goto ERROR
			}
			if qLv > 1 {
				qLv--
				goto LISTPROC
			}
			if qLv == 1 {
				return findSets(pair(pair(exp).cdr).car, vars, env, m)
			}
		}

		if xCar == yQq {
			if lenX != 2 {
				err = qqErr
				goto ERROR
			}
			return findSetsInQq(pair(pair(exp).cdr).car, qLv+1, vars, env, m)
		}

	LISTPROC:
		for i := 0; i < lenX; i++ {
			err = findSetsInQq(pair(exp).car, qLv, vars, env, m)
			if err != nil {
				goto ERROR
			}
			exp = pair(exp).cdr
		}
		return nil
	default:
		err = qqErr
		goto ERROR
	}

ERROR:
	return err
}

/*----------------------------------------------------
 * findLocalInQq
 */
func findLocalInQq(x yObj, qLv int, env locTable, m *vm, ctx int) (int, error) {
	var (
		err     error
		exp     yObj
		typ     int64
		locs, l = 0, 0
	)
	qqErr := mkYerror(errComp, errSyntax, "malformed quasi-quote")
	uqErr := mkYerror(errComp, errSyntax, "malformed unquote")

	if qLv < 0 {
		err = uqErr
		goto ERROR
	}
	exp = x

	typ = typeOf(exp)
	switch typ {
	case tIntnum, tFlonum, tRatinum, tCmplnum, tChar, tString, tConstant, tSymbol:
		return 0, nil
	case tPair:
		xCar := pair(exp).car
		lenX := length(exp)

		if qLv == 0 {
			if typeOf(xCar) == tSymbol {
				if xCar == yQq {
					if lenX != 2 {
						err = qqErr
						goto ERROR
					}
					return findLocalInQq(pair(pair(exp).cdr).car, qLv+1, env, m, ctx|ctxINNER)
				}
				if xCar == yUq || xCar == yUqSplicing {
					err = uqErr
					goto ERROR
				}
			}
			return findLocal(exp, env, m, ctx|ctxINNER)
		}
		// qLv > 0
		if xCar == yUq || xCar == yUqSplicing {
			if lenX != 2 {
				err = uqErr
				goto ERROR
			}
			if qLv > 1 {
				qLv--
				goto LISTPROC
			}
			if qLv == 1 {
				return findLocal(pair(pair(exp).cdr).car, env, m, ctx|ctxINNER)
			}
		}

		if xCar == yQq {
			if lenX != 2 {
				err = qqErr
				goto ERROR
			}
			return findLocalInQq(pair(pair(exp).cdr).car, qLv+1, env, m, ctx|ctxINNER)
		}

	LISTPROC:
		for i := 0; i < lenX; i++ {
			l, err = findLocalInQq(pair(exp).car, qLv, env, m, ctx|ctxINNER)
			if err != nil {
				goto ERROR
			}
			if l > locs {
				locs = l
			}
			exp = pair(exp).cdr
		}
		return locs, nil
	default:
		err = qqErr
		goto ERROR
	}

ERROR:
	return 0, err
}
