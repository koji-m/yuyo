package yuyocore

type yError struct {
	desc []string
}

func mkYerror(msg string, det ...string) yError {
	errMsgs := []string{msg}
	for _, d := range det {
		errMsgs = append(errMsgs, d)
	}
	return yError{errMsgs}
}

func (err yError) Error() string {
	s := ""
	d := err.desc
	for i := 0; i < len(d); i++ {
		s += (d[i] + " ")
	}
	return errHeader + s
}

func (err *yError) detail(msg string) {
	err.desc = append(err.desc, msg)
}

const (
	errHeader        = "[ERROR] "
	errRead          = "read-error:"
	errIncompExp     = "incomplete expression:"
	errSyntax        = "syntax-error:"
	errIntern        = "internal-error:"
	errOutOfRange    = "type value out of range:"
	errBadIdentifier = "bad identifier:"
	errBadCharacter  = "bad character:"
	errBadNumber     = "bad number:"
	errComp          = "compile-error:"
	errUnboundVar    = "unbound variable:"
	errImmutableVar  = "immutable varibale:"
	errNumArg        = "wrong number of arguments:"
	errVmPanic       = "vm runtime panic:"
	errArgTypeMis    = "argument type mismatch:"
	errListReq       = "proper list required:"
	errApply         = "non-function application:"
	errUnknownEscSeq = "unknown escape sequence in string:"
)

var (
	yErrReadError = yError{[]string{errRead}}

	yErrListReq      = yObj(&yCell{typeErr, yObj(&yError{[]string{errListReq}})})
	yErrUnboundVar   = yObj(&yCell{typeErr, yObj(&yError{[]string{errUnboundVar}})})
	yErrImmutableVar = yObj(&yCell{typeErr, yObj(&yError{[]string{errImmutableVar}})})
	yErrNumArg       = yObj(&yCell{typeErr, yObj(&yError{[]string{errNumArg}})})
	yErrArgTypeMis   = yObj(&yCell{typeErr, yObj(&yError{[]string{errArgTypeMis}})})
	yErrVmPanic      = yObj(&yCell{typeErr, yObj(&yError{[]string{errVmPanic}})})
	yErrApply        = yObj(&yCell{typeErr, yObj(&yError{[]string{errApply}})})
)
