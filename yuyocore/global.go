package yuyocore

type glocRec struct {
	immutable bool
	gloc      *yObj
}

var (
	syntaxQuote      = yObj(&yCell{car: yObj(uintptr(tSyntaxQuote))})
	syntaxLambda     = yObj(&yCell{car: yObj(uintptr(tSyntaxLambda))})
	syntaxLet        = yObj(&yCell{car: yObj(uintptr(tSyntaxLet))})
	syntaxLetseq     = yObj(&yCell{car: yObj(uintptr(tSyntaxLetseq))})
	syntaxLetrec     = yObj(&yCell{car: yObj(uintptr(tSyntaxLetrec))})
	syntaxIf         = yObj(&yCell{car: yObj(uintptr(tSyntaxIf))})
	syntaxSet        = yObj(&yCell{car: yObj(uintptr(tSyntaxSet))})
	syntaxDefine     = yObj(&yCell{car: yObj(uintptr(tSyntaxDefine))})
	syntaxBegin      = yObj(&yCell{car: yObj(uintptr(tSyntaxBegin))})
	syntaxCond       = yObj(&yCell{car: yObj(uintptr(tSyntaxCond))})
	syntaxCase       = yObj(&yCell{car: yObj(uintptr(tSyntaxCase))})
	syntaxDo         = yObj(&yCell{car: yObj(uintptr(tSyntaxDo))})
	syntaxAnd        = yObj(&yCell{car: yObj(uintptr(tSyntaxAnd))})
	syntaxOr         = yObj(&yCell{car: yObj(uintptr(tSyntaxOr))})
	syntaxQq         = yObj(&yCell{car: yObj(uintptr(tSyntaxQuasiquote))})
	syntaxUq         = yObj(&yCell{car: yObj(uintptr(tSyntaxUnquote))})
	syntaxUqSplicing = yObj(&yCell{car: yObj(uintptr(tSyntaxUnquoteSplicing))})

	yQuote      = mkSymbol("quote")
	yLambda     = mkSymbol("lambda")
	yLet        = mkSymbol("let")
	yLetseq     = mkSymbol("let*")
	yLetrec     = mkSymbol("letrec")
	yIf         = mkSymbol("if")
	ySet        = mkSymbol("set!")
	yDefine     = mkSymbol("define")
	yBegin      = mkSymbol("begin")
	yCond       = mkSymbol("cond")
	yCase       = mkSymbol("case")
	yDo         = mkSymbol("do")
	yAnd        = mkSymbol("and")
	yOr         = mkSymbol("or")
	yQq         = mkSymbol("quasiquote")
	yUq         = mkSymbol("unquote")
	yUqSplicing = mkSymbol("unquote-splicing")
	yCallWithCc = mkSymbol("call/cc")
	yLss        = mkSymbol("<")
	yAdd        = mkSymbol("+")
	ySub        = mkSymbol("-")
	yMemv       = mkSymbol("memv")
	yLst2Vec    = mkSymbol("list->vector")
	yPairp      = mkSymbol("pair?")
	yNullp      = mkSymbol("null?")
	yCons       = mkSymbol("cons")
	yCar        = mkSymbol("car")
	yCdr        = mkSymbol("cdr")
	ySetCar     = mkSymbol("set-car!")
	ySetCdr     = mkSymbol("set-cdr!")
	yList       = mkSymbol("list")
	yListp      = mkSymbol("list?")
	yLength     = mkSymbol("length")
	yAppend     = mkSymbol("append")
	yReverse    = mkSymbol("reverse")
	yNumeq      = mkSymbol("=")
	yEqv        = mkSymbol("eqv?")
	yChareq     = mkSymbol("char=?")
	yExactp     = mkSymbol("exact?")
	yInexactp   = mkSymbol("inexact?")
	yNumberp    = mkSymbol("number?")
	yIntegerp   = mkSymbol("integer?")
	yRationalp  = mkSymbol("rational?")
	yRealp      = mkSymbol("real?")
	yComplexp   = mkSymbol("complex?")
	yDisplay    = mkSymbol("display")
)

var topEnv = map[yObj]*glocRec{
	yQuote:      &glocRec{false, &syntaxQuote},
	yLambda:     &glocRec{false, &syntaxLambda},
	yLet:        &glocRec{false, &syntaxLet},
	yLetseq:     &glocRec{false, &syntaxLetseq},
	yLetrec:     &glocRec{false, &syntaxLetrec},
	yIf:         &glocRec{false, &syntaxIf},
	ySet:        &glocRec{false, &syntaxSet},
	yDefine:     &glocRec{false, &syntaxDefine},
	yBegin:      &glocRec{false, &syntaxBegin},
	yCond:       &glocRec{false, &syntaxCond},
	yCase:       &glocRec{false, &syntaxCase},
	yDo:         &glocRec{false, &syntaxDo},
	yAnd:        &glocRec{false, &syntaxAnd},
	yOr:         &glocRec{false, &syntaxOr},
	yQq:         &glocRec{false, &syntaxQq},
	yUq:         &glocRec{false, &syntaxUq},
	yUqSplicing: &glocRec{false, &syntaxUqSplicing},
	yCallWithCc: &glocRec{false, &procCALLWITHCC},
	yLss:        &glocRec{false, &procLSS},
	yAdd:        &glocRec{false, &procADD},
	ySub:        &glocRec{false, &procSUB},
	yMemv:       &glocRec{false, &procMEMV},
	yLst2Vec:    &glocRec{false, &procLST2VEC},
	yPairp:      &glocRec{false, &procPAIRP},
	yNullp:      &glocRec{false, &procNULLP},
	yCons:       &glocRec{false, &procCONS},
	yCar:        &glocRec{false, &procCAR},
	yCdr:        &glocRec{false, &procCDR},
	ySetCar:     &glocRec{false, &procSETCAR},
	ySetCdr:     &glocRec{false, &procSETCDR},
	yList:       &glocRec{false, &procLIST},
	yListp:      &glocRec{false, &procLISTP},
	yLength:     &glocRec{false, &procLENGTH},
	yAppend:     &glocRec{false, &procAPPEND},
	yReverse:    &glocRec{false, &procREVERSE},
	yNumeq:      &glocRec{false, &procNUMEQ},
	yEqv:        &glocRec{false, &procEQV},
	yChareq:     &glocRec{false, &procCHAREQ},
	yExactp:     &glocRec{false, &procEXACTP},
	yInexactp:   &glocRec{false, &procINEXACTP},
	yNumberp:    &glocRec{false, &procNUMBERP},
	yIntegerp:   &glocRec{false, &procINTEGERP},
	yRationalp:  &glocRec{false, &procRATIONALP},
	yRealp:      &glocRec{false, &procREALP},
	yComplexp:   &glocRec{false, &procCOMPLEXP},
	yDisplay:    &glocRec{false, &procDISPLAY},
}
