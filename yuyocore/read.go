package yuyocore

import (
	"io"
	"regexp"
	"strconv"
	"strings"
	"unicode"
)

/*** for symbol identifier ***/
const (
	sLETTER byte = 1 << iota
	sSPECIALINITIAL
	sDIGIT
	sSPECIALSUBSEQ
	sDELIMITER

	sINITIAL = sLETTER + sSPECIALINITIAL
	sSUBSEQ  = sINITIAL + sDIGIT + sSPECIALSUBSEQ
)

// 00  01  02  03  04  05  06  07  08  09  0A  0B  0C  0D  0E  0F
var chTbl = []byte{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 16, 0, 0,
	// 10  11  12  13  14  15  16  17  18  19  1A  1B  1C  1D  1E  1F
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	// 20  21  22  23  24  25  26  27  28  29  2A  2B  2C  2D  2E  2F
	16, 2, 16, 0, 2, 2, 2, 0, 16, 16, 2, 8, 0, 8, 8, 2,
	// 30  31  32  33  34  35  36  37  38  39  3A  3B  3C  3D  3E  3F
	4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 16, 2, 2, 2, 2,
	// 40  41  42  43  44  45  46  47  48  49  4A  4B  4C  4D  4E  4F
	8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	// 50  51  52  53  54  55  56  57  58  59  5A  5B  5C  5D  5E  5F
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 2, 2,
	// 60  61  62  63  64  65  66  67  68  69  6A  6B  6C  6D  6E  6F
	0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	// 70  71  72  73  74  75  76  77  78  79  7A  7B  7C  7D  7E  7F
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 2, 0,
}

/*** for symbol identifier END ***/

func isDigit(c rune, rdx int64) (int64, bool) {
	if rdx == 16 {
		if c >= 'a' && c <= 'f' {
			return int64(c - 'a' + 10), true
		}
		n := c - '0'
		if n > -1 && n < 10 {
			return int64(n), true
		}
		return 0, false
	}
	n := int64(c - '0')
	if n > -1 && n < rdx {
		return int64(n), true
	}
	return 0, false
}

func isExpoMarker(c rune) bool {
	switch c {
	case 'e', 's', 'f', 'd', 'l':
		return true
	default:
		return false
	}
}

func power(e int) int64 {
	res := 1
	for i := 0; i < e; i++ {
		res *= 10
	}
	return int64(res)
}

func gcd(x, y int64) int64 {
	if x < 0 {
		return gcd(-x, y)
	}
	if x == 0 {
		return y
	}
	return gcd(y%x, x)
}

func makeRational(rat rational, numAftPnt int, expo int) rational {
	if expo > numAftPnt {
		rat.nume *= power(expo - numAftPnt)
	} else if numAftPnt > expo {
		rat.denom *= power(numAftPnt - expo)
	}
	cd := gcd(rat.nume, rat.denom)
	rat.nume /= cd
	rat.denom /= cd
	return rat
}

var nullFlgSyms = map[string]yObj{}

type port struct {
	rdr *strings.Reader
}

// for testing
func NewPort(str string) *port {
	return newPort(str)
}

func newPort(str string) *port {
	return &port{strings.NewReader(str)}
}

func (self *port) nextRune() (rune, error) {
	var (
		chr rune
		err error
	)

	for chr, _, err = self.rdr.ReadRune(); unicode.IsSpace(chr); chr, _, err = self.rdr.ReadRune() {
	}

	if err != nil {
		return -1, err
	}

	return chr, nil
}

func isDelim(c rune) bool {
	switch c {
	case ' ':
		return true
	case '\n':
		return true
	case '(':
		return true
	case ')':
		return true
	case '"':
		return true
	case ';':
		return true
	default:
		return false
	}
}

func (self *port) backRune() error {
	err := self.rdr.UnreadRune()
	if err != nil {
		return err
	}
	return nil
}

func (self *port) peek1() (rune, error) {
	chr, _, err := self.rdr.ReadRune()
	if err != nil {
		return -1, err
	}
	err = self.rdr.UnreadRune()
	if err != nil {
		return -1, err
	}
	return chr, err
}

/*----------------------------------------------------
 * readExp
 *  if read EOF before read object head, return EOF.
 *  if read EOF after read object head and not finish
 *  reading (can't parse), retrun ERROR.
 */
func readExp(prt *port, flgSyms map[string]yObj) (yObj, error) {
	var x yObj
	c, err := prt.nextRune()
	//Deb("read_exp():c:"+string(c))
	if err != nil {
		if err == io.EOF {
			return yEOF, nil
		}
		goto ERROR
	}

	switch c {
	case '(':
		return readList(prt, flgSyms)
	case '"':
		return readString(prt)
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		return readNumber(prt, c)
	case '.':
		return readSymbolOrNumber(prt, c)
	case '+', '-':
		return readSymbolOrNumber(prt, c)
	case ')':
		return nil, mkYerror(errRead, errSyntax, "extra close parenthesis")
	case '\'':
		x, err = readExp(prt, flgSyms)
		if err != nil {
			goto ERROR
		}
		return mkPair(yQuote, mkPair(x, yNULL)), nil
	case '`':
		c, err := prt.nextRune()
		if err != nil {
			goto ERROR
		}
		if c == '#' {
			c, _, err = prt.rdr.ReadRune()
			if err != nil {
				goto ERROR
			}
			if c == '(' {
				x, err = readList(prt, flgSyms)
				if err != nil {
					goto ERROR
				}
				return mkPair(yLst2Vec, mkPair(mkPair(yQq, mkPair(x, yNULL)), yNULL)), nil
			}
			prt.rdr.UnreadRune()
			x, err = readSpecial(prt, flgSyms)
			if err != nil {
				goto ERROR
			}
			return mkPair(yQq, mkPair(x, yNULL)), nil
		}
		prt.rdr.UnreadRune()
		x, err = readExp(prt, flgSyms)
		if err != nil {
			goto ERROR
		}
		return mkPair(yQq, mkPair(x, yNULL)), nil
	case ',':
		c, _, err = prt.rdr.ReadRune()
		if err != nil {
			err = mkYerror(errRead, errIntern)
			goto ERROR
		}
		if c == '@' {
			x, err = readExp(prt, flgSyms)
			if err != nil {
				goto ERROR
			}
			return mkPair(yUqSplicing, mkPair(x, yNULL)), nil
		}
		err = prt.rdr.UnreadRune()
		if err != nil {
			err = mkYerror(errRead, errIntern)
			goto ERROR
		}
		x, err = readExp(prt, flgSyms)
		if err != nil {
			goto ERROR
		}
		return mkPair(yUq, mkPair(x, yNULL)), nil
	case '#':
		return readSpecial(prt, flgSyms)
	}
	prt.rdr.UnreadRune()
	return readSymbol(prt, -1)

ERROR:
	return nil, err
}

var ReadExp = readExp

func scmAppend(root yObj, last yObj, obj yObj) (r_root yObj, r_last yObj) {
	if obj == nil {
		return root, last
	}
	if root == yNULL {
		r_root = mkPair(obj, yNULL)
		r_last = r_root
		return r_root, r_last
	} else {
		pair(last).cdr = mkPair(obj, yNULL)
		r_last = pair(last).cdr
		return root, r_last
	}
}

func readList(prt *port, flgSyms map[string]yObj) (yObj, error) {
	var (
		c1, c2    rune
		root      yObj = yNULL
		last      yObj = yNULL
		exp       yObj
		dot_state bool = false
		err       error
	)

	for {
		c1, err = prt.nextRune()
		//Deb("read_list():c1:"+string(c1))
		if err != nil { //if not encolose paren and no more input, syntax error.
			if err == io.EOF {
				return nil, mkYerror(errRead, errIncompExp, "list is not closed")
			}
			goto ERROR
		}

		if c1 == ')' { //if "()", must return yNULL
			if typeOf(root) == tPair {
				if pair(root).car == yQq {
					if length(root) != 2 {
						goto ERROR
					}
					cadrRoot := pair(pair(root).cdr).car
					if typeOf(cadrRoot) == tVector {
						return mkPair(yLst2Vec, mkPair(mkPair(yQq, mkPair(vector2list(cadrRoot), yNULL)), yNULL)), nil
					}
				}
			}
			return root, nil
		}

		if dot_state { //if anything after dot&cdr read, error
			return nil, mkYerror(errRead, errSyntax, "malformed dot syntax")
		}

		if c1 == '.' {
			c2, err = prt.peek1()
			if err != nil {
				goto ERROR
			}
			if !unicode.IsSpace(c2) {
				exp, err = readSymbolOrNumber(prt, '.') //expecting symbol "..." or number
				if err != nil {
					return nil, err
				}
				goto APPEND
			}

			if root == yNULL { //if initial state, error
				return nil, mkYerror(errRead, errSyntax, "malformed dot syntax")
			}

			pair(last).cdr, err = readExp(prt, flgSyms)
			if err != nil {
				return nil, err
			}
			dot_state = true
			// here go back to first nextRune()
			continue
		}

		err = prt.backRune()
		if err != nil {
			goto ERROR
		}
		exp, err = readExp(prt, flgSyms)
		if err != nil {
			return nil, err
		}
	APPEND:
		root, last = scmAppend(root, last, exp)
	}

ERROR:
	return nil, mkYerror(errRead, errIntern)
}

func readString(prt *port) (yObj, error) {
	var (
		str string
		c   rune
		err error
	)
STATE0:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			err = mkYerror(errRead, errIncompExp, "EOF in string")
			goto REFUSED
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if c == '\\' {
		str += string(c)
		goto STATE1
	}
	if c == '"' {
		goto ACCEPT
	}
	str += string(c)
	goto STATE0

STATE1:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			err = mkYerror(errRead, errIncompExp, "EOF in string")
			goto REFUSED
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if c == '\\' || c == '"' || c == 'n' || c == 't' {
		str += string(c)
		goto STATE0
	}
	err = mkYerror(errRead, errUnknownEscSeq)
	goto REFUSED

ACCEPT:
	return mkString(str), nil

REFUSED:
	return nil, err
}

/*
func readNumber(prt *port) (yObj, error) {
  var (
  		num int64
		c rune
		err error
  )
  for c,_,err = prt.rdr.ReadRune(); unicode.IsDigit(c) && err == nil;
      c,_,err = prt.rdr.ReadRune() {
	num *= 10
    num += int64(c - '0')
  }

  _ = prt.backRune()

  if err != nil {
    if err == io.EOF {
	  err = nil
	}else{
      return nil, mkYerror(errRead, errIntern)
	}
  }

  if num > 1152921504606846975 || num < -1152921504606846976 {
    err = mkYerror(errRead, errOutOfRange)
  }

  return mkIntnum(num), err
}
*/

func readNumber(prt *port, head rune) (yObj, error) {
	var (
		c               rune
		rdx             int64 = 10
		exct, prec, ok  bool
		sign1, sign2    int = 1, 1
		obj, obj1, obj2 yObj
		err             error
	)

	if head != -1 {
		c, err = head, nil
	} else {
		c, _, err = prt.rdr.ReadRune()
	}
	if err != nil {
		return nil, mkYerror(errRead, errIntern)
	}

	//STATE0
	switch c {
	case '#':
		goto STATE1
	case '+':
		goto STATE5
	case '-':
		sign1 = -1
		goto STATE5
	case '.':
		goto STATE10
	}
	if unicode.IsDigit(c) {
		obj1, err = readUreal(prt, 10, c)
		if err != nil {
			goto REFUSED
		}
		goto UR1
	}

STATE1:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	switch c {
	case 'b':
		rdx = 2
		goto STATE2
	case 'o':
		rdx = 8
		goto STATE2
	case 'd':
		rdx = 10
		goto STATE2
	case 'x':
		rdx = 16
		goto STATE2
	case 'i':
		prec = true
		exct = false
		goto STATE7
	case 'e':
		prec = true
		exct = true
		goto STATE7
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE2:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	switch c {
	case '#':
		goto STATE3
	case '+':
		goto STATE5
	case '-':
		sign1 = -1
		goto STATE5
	}
	if _, ok = isDigit(c, rdx); ok || c == '.' {
		obj1, err = readUreal(prt, rdx, c)
		if err != nil {
			goto REFUSED
		}
		goto UR1
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE3:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	switch c {
	case 'i':
		prec = true
		exct = false
		goto STATE4
	case 'e':
		prec = true
		exct = true
		goto STATE4
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE4:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	switch c {
	case '+':
		sign1 = 1
		goto STATE5
	case '-':
		sign1 = -1
		goto STATE5
	}
	if _, ok = isDigit(c, rdx); ok || c == '.' {
		obj1, err = readUrealPrec(prt, rdx, exct, c)
		if err != nil {
			goto REFUSED
		}
		goto UR1
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE5:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if c == 'i' {
		goto STATE6
	}
	if _, ok = isDigit(c, rdx); ok || c == '.' {
		if prec {
			obj1, err = readUrealPrec(prt, rdx, exct, c)
		} else {
			obj1, err = readUreal(prt, rdx, c)
		}
		if err != nil {
			goto REFUSED
		}
		goto UR2
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE6:
	if exct {
		err = mkYerror(errRead, errBadNumber, "exact complex number is not supported")
		goto REFUSED
	}
	obj = mkCmplnum(complex(0, float64(sign1)))
	goto ACCEPT

STATE7:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	switch c {
	case '+':
		sign1 = 1
		goto STATE5
	case '-':
		sign1 = -1
		goto STATE5
	case '#':
		goto STATE8
	}
	if _, ok = isDigit(c, rdx); ok || c == '.' {
		obj1, err = readUrealPrec(prt, rdx, exct, c)
		if err != nil {
			goto REFUSED
		}
		goto UR1
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE8:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	switch c {
	case 'b':
		rdx = 2
		goto STATE9
	case 'o':
		rdx = 8
		goto STATE9
	case 'd':
		rdx = 10
		goto STATE9
	case 'x':
		rdx = 16
		goto STATE9
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE9:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	switch c {
	case '+':
		sign1 = 1
		goto STATE5
	case '-':
		sign1 = -1
		goto STATE5
	}
	if _, ok = isDigit(c, rdx); ok || c == '.' {
		obj1, err = readUrealPrec(prt, rdx, exct, c)
		if err != nil {
			goto REFUSED
		}
		goto UR1
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE10:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if unicode.IsDigit(c) {
		prt.rdr.UnreadRune()
		obj1, err = readUreal(prt, 10, '.')
		if err != nil {
			goto REFUSED
		}
		goto UR1
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

UR1:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			obj = obj1
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	switch c {
	case '@':
		goto STATE11
	case '+':
		sign2 = 1
		goto STATE13
	case '-':
		sign2 = -1
		goto STATE13
	}
	if isDelim(c) {
		obj = obj1
		goto ACCEPT
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

UR2:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			if sign1 == -1 {
				obj, _ = yNumNegative(obj1)
			} else {
				obj = obj1
			}
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	switch c {
	case '@':
		goto STATE11
	case '+':
		sign2 = 1
		goto STATE13
	case '-':
		sign2 = -1
		goto STATE13
	case 'i':
		goto STATE16
	}
	if isDelim(c) {
		if sign1 == -1 {
			obj, _ = yNumNegative(obj1)
		} else {
			obj = obj1
		}
		goto ACCEPT
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE11:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	switch c {
	case '+':
		sign2 = 1
		goto STATE12
	case '-':
		sign2 = -1
		goto STATE2
	}
	if _, ok = isDigit(c, rdx); ok || c == '.' {
		if prec {
			obj2, err = readUrealPrec(prt, rdx, exct, c)
		} else {
			obj2, err = readUreal(prt, rdx, c)
		}
		if err != nil {
			goto REFUSED
		}
		goto UR3
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE12:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if _, ok = isDigit(c, rdx); ok || c == '.' {
		if prec {
			obj2, err = readUrealPrec(prt, rdx, exct, c)
		} else {
			obj2, err = readUreal(prt, rdx, c)
		}
		if err != nil {
			goto REFUSED
		}
		goto UR3
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE13:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if _, ok = isDigit(c, rdx); ok || c == '.' {
		if prec {
			obj2, err = readUrealPrec(prt, rdx, exct, c)
		} else {
			obj2, err = readUreal(prt, rdx, c)
		}
		if err != nil {
			goto REFUSED
		}
		goto UR4
	}
	if c == 'i' {
		goto STATE15
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

UR3:
	if exct {
		err = mkYerror(errRead, errBadNumber, "exact complex number is not supported")
		goto REFUSED
	}
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			err = mkYerror(errRead, errBadNumber, "<real num>@<real num> format not supported")
			goto REFUSED
			//obj, _ = constCmplnum(obj1, obj2, sign1, sign2)
			//goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if isDelim(c) {
		err = mkYerror(errRead, errBadNumber, "<real num>@<real num> format not supported")
		goto REFUSED
		//obj, _ = constCmplnum(obj1, obj2, sign1, sign2)
		//goto ACCEPT
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

UR4:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if c == 'i' {
		goto STATE14
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE14:
	if exct {
		err = mkYerror(errRead, errBadNumber, "exact complex number is not supported")
		goto REFUSED
	}
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			obj, _ = constCmplnum(obj1, obj2, sign1, sign2)
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if isDelim(c) {
		obj, _ = constCmplnum(obj1, obj2, sign1, sign2)
		goto ACCEPT
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE15:
	if exct {
		err = mkYerror(errRead, errBadNumber, "exact complex number is not supported")
		goto REFUSED
	}
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			obj, _ = constCmplnum(obj1, yIntNum1, sign1, sign2)
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if isDelim(c) {
		obj, _ = constCmplnum(obj1, yIntNum1, sign1, sign2)
		goto ACCEPT
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE16:
	if exct {
		err = mkYerror(errRead, errBadNumber, "exact complex number is not supported")
		goto REFUSED
	}
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			obj, _ = constCmplnum(yIntNum0, obj1, 0, sign1)
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if isDelim(c) {
		obj, _ = constCmplnum(yIntNum0, obj1, 0, sign1)
		goto ACCEPT
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

ACCEPT:
	prt.rdr.UnreadRune()
	return obj, nil

REFUSED:
	return nil, err
}

func readUrealPrec(prt *port, rdx int64, exct bool, head rune) (yObj, error) {
	var (
		inum                  int64
		expo, sign, numAftPnt int = 0, 1, 0
		fl                    float64
		rnum                  rational
		decStr                = "0."
		obj                   yObj
		c                     rune
		ok                    bool
		err                   error
	)

	//STATE0:
	if head != -1 {
		c, err = head, nil
	} else {
		c, _, err = prt.rdr.ReadRune()
	}
	if err != nil {
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if inum, ok = isDigit(c, rdx); ok {
		goto STATE1
	}
	if c == '.' {
		if rdx != 10 {
			err = mkYerror(errRead, errBadNumber, "not support "+strconv.FormatInt(rdx, 10)+"-based fraction")
			goto REFUSED
		}
		goto STATE14
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE1:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			if exct {
				obj = mkIntnum(inum)
			} else {
				obj = mkFlonum(float64(inum))
			}
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if i, ok := isDigit(c, rdx); ok {
		inum = inum*rdx + i
		goto STATE1
	}
	if c == '#' {
		inum *= rdx
		goto STATE5
	}
	if isExpoMarker(c) { //not distinguish precision for now
		if rdx != 10 {
			err = mkYerror(errRead, errBadNumber)
			goto REFUSED
		}
		decStr = strconv.FormatInt(inum, 10) + "."
		goto STATE9
	}
	if c == '.' {
		if rdx != 10 {
			err = mkYerror(errRead, errBadNumber, "not support "+strconv.FormatInt(rdx, 10)+"-based fraction")
			goto REFUSED
		}
		decStr = strconv.FormatInt(inum, 10) + "."
		goto STATE10
	}
	if c == '/' {
		rnum = rational{nume: inum}
		goto STATE2
	}
	if exct {
		obj = mkIntnum(inum)
	} else {
		obj = mkFlonum(float64(inum))
	}
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE2:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errBadNumber)
		goto REFUSED
	}
	if i, ok := isDigit(c, rdx); ok {
		rnum.denom = i
		goto STATE3
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE3:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			if exct {
				rnum = makeRational(rnum, 0, 0)
				if rnum.denom == 1 {
					obj = mkIntnum(rnum.nume)
				} else {
					obj = mkRatinum(rnum)
				}
			} else {
				obj = mkFlonum(float64(rnum.nume) / float64(rnum.denom))
			}
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if i, ok := isDigit(c, rdx); ok {
		rnum.denom = rnum.denom*rdx + i
		goto STATE3
	}
	if c == '#' {
		rnum.denom *= rdx
		goto STATE4
	}
	if exct {
		rnum = makeRational(rnum, 0, 0)
		if rnum.denom == 1 {
			obj = mkIntnum(rnum.nume)
		} else {
			obj = mkRatinum(rnum)
		}
	} else {
		obj = mkFlonum(float64(rnum.nume) / float64(rnum.denom))
	}
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE4:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			if exct {
				rnum = makeRational(rnum, 0, 0)
				if rnum.denom == 1 {
					obj = mkIntnum(rnum.nume)
				} else {
					obj = mkRatinum(rnum)
				}
			} else {
				obj = mkFlonum(float64(rnum.nume) / float64(rnum.denom))
			}
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if c == '#' {
		rnum.denom *= rdx
		goto STATE4
	}
	if exct {
		rnum = makeRational(rnum, 0, 0)
		if rnum.denom == 1 {
			obj = mkIntnum(rnum.nume)
		} else {
			obj = mkRatinum(rnum)
		}
	} else {
		obj = mkFlonum(float64(rnum.nume) / float64(rnum.denom))
	}
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE5:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			if exct {
				obj = mkIntnum(inum)
			} else {
				obj = mkFlonum(float64(inum))
			}
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if c == '#' {
		inum *= rdx
		goto STATE5
	}
	if c == '/' {
		rnum = rational{nume: inum}
		goto STATE6
	}
	if isExpoMarker(c) {
		if rdx != 10 {
			err = mkYerror(errRead, errBadNumber)
			goto REFUSED
		}
		decStr = strconv.FormatInt(inum, 10) + "."
		goto STATE9
	}
	if exct {
		obj = mkIntnum(inum)
	} else {
		obj = mkFlonum(float64(inum))
	}
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE6:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errBadNumber)
		goto REFUSED
	}
	if i, ok := isDigit(c, rdx); ok {
		rnum.denom = i
		goto STATE7
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE7:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			if exct {
				rnum = makeRational(rnum, 0, 0)
				if rnum.denom == 1 {
					obj = mkIntnum(rnum.nume)
				} else {
					obj = mkRatinum(rnum)
				}
			} else {
				obj = mkFlonum(float64(rnum.nume) / float64(rnum.denom))
			}
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if i, ok := isDigit(c, rdx); ok {
		rnum.denom = rnum.denom*10 + i
		goto STATE7
	}
	if c == '#' {
		rnum.denom *= rdx
		goto STATE8
	}
	if exct {
		rnum = makeRational(rnum, 0, 0)
		if rnum.denom == 1 {
			obj = mkIntnum(rnum.nume)
		} else {
			obj = mkRatinum(rnum)
		}
	} else {
		obj = mkFlonum(float64(rnum.nume) / float64(rnum.denom))
	}
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE8:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			if exct {
				rnum = makeRational(rnum, 0, 0)
				if rnum.denom == 1 {
					obj = mkIntnum(rnum.nume)
				} else {
					obj = mkRatinum(rnum)
				}
			} else {
				obj = mkFlonum(float64(rnum.nume) / float64(rnum.denom))
			}
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if c == '#' {
		rnum.denom *= rdx
		goto STATE8
	}
	if exct {
		rnum = makeRational(rnum, 0, 0)
		if rnum.denom == 1 {
			obj = mkIntnum(rnum.nume)
		} else {
			obj = mkRatinum(rnum)
		}
	} else {
		obj = mkFlonum(float64(rnum.nume) / float64(rnum.denom))
	}
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE9:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errBadNumber)
		goto REFUSED
	}
	if i, ok := isDigit(c, 10); ok {
		expo = int(i)
		goto STATE13
	}
	if c == '+' {
		sign = 1
		goto STATE12
	}
	if c == '-' {
		sign = -1
		goto STATE12
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE10:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			if exct {
				if numAftPnt == 0 {
					obj = mkIntnum(inum)
				} else {
					rnum = makeRational(rational{nume: inum, denom: 1}, numAftPnt, 0)
					if rnum.denom == 1 {
						obj = mkIntnum(rnum.nume)
					} else {
						obj = mkRatinum(rnum)
					}
				}
			} else {
				fl, _ = strconv.ParseFloat(decStr, 64)
				obj = mkFlonum(fl)
			}
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if i, ok := isDigit(c, 10); ok {
		if exct {
			inum = inum*10 + i
			numAftPnt++
		} else {
			decStr += string(c)
		}
		goto STATE10
	}
	if isExpoMarker(c) {
		if rdx != 10 {
			err = mkYerror(errRead, errBadNumber)
			goto REFUSED
		}
		goto STATE9
	}
	if c == '#' {
		goto STATE11
	}
	if exct {
		if numAftPnt == 0 {
			obj = mkIntnum(inum)
		} else {
			rnum = makeRational(rational{nume: inum, denom: 1}, numAftPnt, 0)
			if rnum.denom == 1 {
				obj = mkIntnum(rnum.nume)
			} else {
				obj = mkRatinum(rnum)
			}
		}
	} else {
		fl, _ = strconv.ParseFloat(decStr, 64)
		obj = mkFlonum(fl)
	}
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE11:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			if exct {
				if numAftPnt == 0 {
					obj = mkIntnum(inum)
				} else {
					rnum = makeRational(rational{nume: inum, denom: 1}, numAftPnt, 0)
					if rnum.denom == 1 {
						obj = mkIntnum(rnum.nume)
					} else {
						obj = mkRatinum(rnum)
					}
				}
			} else {
				fl, _ = strconv.ParseFloat(decStr, 64)
				obj = mkFlonum(fl)
			}
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if c == '#' {
		goto STATE11
	}
	if isExpoMarker(c) {
		if rdx != 10 {
			err = mkYerror(errRead, errBadNumber)
			goto REFUSED
		}
		goto STATE9
	}
	if exct {
		if numAftPnt == 0 {
			obj = mkIntnum(inum)
		} else {
			rnum = makeRational(rational{nume: inum, denom: 1}, numAftPnt, 0)
			if rnum.denom == 1 {
				obj = mkIntnum(rnum.nume)
			} else {
				obj = mkRatinum(rnum)
			}
		}
	} else {
		fl, _ = strconv.ParseFloat(decStr, 64)
		obj = mkFlonum(fl)
	}
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE12:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errBadNumber)
		goto REFUSED
	}
	if i, ok := isDigit(c, 10); ok {
		expo = int(i)
		goto STATE13
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE13:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			if exct {
				rnum = makeRational(rational{nume: inum, denom: 1},
					numAftPnt, expo*sign)
				if rnum.denom == 1 {
					obj = mkIntnum(rnum.nume)
				} else {
					obj = mkRatinum(rnum)
				}
			} else {
				fl, _ = strconv.ParseFloat(decStr+"e"+strconv.FormatInt(int64(expo*sign), 10), 64)
				obj = mkFlonum(fl)
			}
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if i, ok := isDigit(c, 10); ok {
		expo = expo*10 + int(i)
		goto STATE13
	}
	if exct {
		rnum = makeRational(rational{nume: inum, denom: 1},
			numAftPnt, expo*sign)
		if rnum.denom == 1 {
			obj = mkIntnum(rnum.nume)
		} else {
			obj = mkRatinum(rnum)
		}
	} else {
		fl, _ = strconv.ParseFloat(decStr+"e"+strconv.FormatInt(int64(expo*sign), 10), 64)
		obj = mkFlonum(fl)
	}
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE14:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errBadNumber)
		goto REFUSED
	}
	if i, ok := isDigit(c, 10); ok {
		if exct {
			inum = i
			numAftPnt++
		} else {
			decStr += string(c)
		}
		goto STATE15
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE15:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			if exct {
				rnum = makeRational(rational{nume: inum, denom: 1}, numAftPnt, 0)
				if rnum.denom == 1 {
					obj = mkIntnum(rnum.nume)
				} else {
					obj = mkRatinum(rnum)
				}
			} else {
				fl, _ = strconv.ParseFloat(decStr, 64)
				obj = mkFlonum(fl)
			}
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if i, ok := isDigit(c, 10); ok {
		if exct {
			inum = inum*10 + i
			numAftPnt++
		} else {
			decStr += string(c)
		}
		goto STATE15
	}
	if isExpoMarker(c) {
		if rdx != 10 {
			err = mkYerror(errRead, errBadNumber)
			goto REFUSED
		}
		goto STATE9
	}
	if c == '#' {
		goto STATE16
	}
	if exct {
		rnum = makeRational(rational{nume: inum, denom: 1}, numAftPnt, 0)
		if rnum.denom == 1 {
			obj = mkIntnum(rnum.nume)
		} else {
			obj = mkRatinum(rnum)
		}
	} else {
		fl, _ = strconv.ParseFloat(decStr, 64)
		obj = mkFlonum(fl)
	}
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE16:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			if exct {
				rnum = makeRational(rational{nume: inum, denom: 1}, numAftPnt, 0)
				if rnum.denom == 1 {
					obj = mkIntnum(rnum.nume)
				} else {
					obj = mkRatinum(rnum)
				}
			} else {
				fl, _ = strconv.ParseFloat(decStr, 64)
				obj = mkFlonum(fl)
			}
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if c == '#' {
		goto STATE16
	}
	if isExpoMarker(c) {
		if rdx != 10 {
			err = mkYerror(errRead, errBadNumber)
			goto REFUSED
		}
		goto STATE9
	}
	if exct {
		rnum = makeRational(rational{nume: inum, denom: 1}, numAftPnt, 0)
		if rnum.denom == 1 {
			obj = mkIntnum(rnum.nume)
		} else {
			obj = mkRatinum(rnum)
		}
	} else {
		fl, _ = strconv.ParseFloat(decStr, 64)
		obj = mkFlonum(fl)
	}
	prt.rdr.UnreadRune()
	goto ACCEPT

ACCEPT:
	return obj, nil

REFUSED:
	return nil, err
}

func readUreal(prt *port, rdx int64, head rune) (yObj, error) {
	var (
		inum, tmpInum int64
		expo, sign    int = 0, 1
		fl            float64
		rnum          rational
		decStr        = "0."
		obj           yObj
		c             rune
		ok            bool
		err           error
	)

	//STATE0:
	if head != -1 {
		c, err = head, nil
	} else {
		c, _, err = prt.rdr.ReadRune()
	}
	if err != nil {
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if inum, ok = isDigit(c, rdx); ok {
		goto STATE1
	}
	if c == '.' {
		if rdx != 10 {
			err = mkYerror(errRead, errBadNumber, "not support "+strconv.FormatInt(rdx, 10)+"-based fraction")
			goto REFUSED
		}
		goto STATE14
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE1:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			obj = mkIntnum(inum)
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if i, ok := isDigit(c, rdx); ok {
		inum = inum*rdx + i
		goto STATE1
	}
	if c == '#' {
		inum = inum * rdx
		goto STATE5
	}
	if isExpoMarker(c) { //not distinguish precision for now
		if rdx != 10 {
			err = mkYerror(errRead, errBadNumber)
			goto REFUSED
		}
		decStr = strconv.FormatInt(inum, 10) + "."
		goto STATE9
	}
	if c == '.' {
		if rdx != 10 {
			err = mkYerror(errRead, errBadNumber, "not support "+strconv.FormatInt(rdx, 10)+"-based fraction")
			goto REFUSED
		}
		decStr = strconv.FormatInt(inum, 10) + "."
		goto STATE10
	}
	if c == '/' {
		rnum = rational{nume: inum}
		goto STATE2
	}
	obj = mkIntnum(inum)
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE2:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errBadNumber)
		goto REFUSED
	}
	if i, ok := isDigit(c, rdx); ok {
		rnum.denom = i
		goto STATE3
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE3:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			rnum = makeRational(rnum, 0, 0)
			if rnum.denom == 1 {
				obj = mkIntnum(rnum.nume)
			} else {
				obj = mkRatinum(rnum)
			}
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if i, ok := isDigit(c, rdx); ok {
		rnum.denom = rnum.denom*rdx + i
		goto STATE3
	}
	if c == '#' {
		rnum.denom *= rdx
		goto STATE4
	}
	rnum = makeRational(rnum, 0, 0)
	if rnum.denom == 1 {
		obj = mkIntnum(rnum.nume)
	} else {
		obj = mkRatinum(rnum)
	}
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE4:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			obj = mkFlonum(float64(rnum.nume) / float64(rnum.denom))
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if c == '#' {
		rnum.denom *= rdx
		goto STATE4
	}
	obj = mkFlonum(float64(rnum.nume) / float64(rnum.denom))
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE5:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			obj = mkFlonum(float64(inum))
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if c == '#' {
		inum *= rdx
		goto STATE5
	}
	if c == '/' {
		goto STATE6
	}
	if isExpoMarker(c) {
		if rdx != 10 {
			err = mkYerror(errRead, errBadNumber)
			goto REFUSED
		}
		decStr = strconv.FormatInt(inum, 10) + "."
		goto STATE9
	}
	obj = mkFlonum(float64(inum))
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE6:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errBadNumber)
		goto REFUSED
	}
	if i, ok := isDigit(c, rdx); ok {
		tmpInum = i
		goto STATE7
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE7:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			obj = mkFlonum(float64(inum) / float64(tmpInum))
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if i, ok := isDigit(c, rdx); ok {
		tmpInum = tmpInum*rdx + i
		goto STATE7
	}
	if c == '#' {
		tmpInum *= rdx
		goto STATE8
	}
	obj = mkFlonum(float64(inum) / float64(tmpInum))
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE8:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			obj = mkFlonum(float64(inum) / float64(tmpInum))
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if c == '#' {
		tmpInum *= rdx
		goto STATE8
	}
	obj = mkFlonum(float64(inum) / float64(tmpInum))
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE9:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errBadNumber)
		goto REFUSED
	}
	if i, ok := isDigit(c, 10); ok {
		expo = int(i)
		goto STATE13
	}
	if c == '+' {
		sign = 1
		goto STATE12
	}
	if c == '-' {
		sign = -1
		goto STATE12
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE10:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			fl, _ = strconv.ParseFloat(decStr, 64)
			obj = mkFlonum(fl)
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if _, ok = isDigit(c, 10); ok {
		decStr += string(c)
		goto STATE10
	}
	if isExpoMarker(c) {
		if rdx != 10 {
			err = mkYerror(errRead, errBadNumber)
			goto REFUSED
		}
		goto STATE9
	}
	if c == '#' {
		goto STATE11
	}
	fl, _ = strconv.ParseFloat(decStr, 64)
	obj = mkFlonum(fl)
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE11:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			fl, _ = strconv.ParseFloat(decStr, 64)
			obj = mkFlonum(fl)
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if c == '#' {
		goto STATE11
	}
	if isExpoMarker(c) {
		if rdx != 10 {
			err = mkYerror(errRead, errBadNumber)
			goto REFUSED
		}
		goto STATE9
	}
	fl, _ = strconv.ParseFloat(decStr, 64)
	obj = mkFlonum(fl)
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE12:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errBadNumber)
		goto REFUSED
	}
	if i, ok := isDigit(c, 10); ok {
		expo = int(i)
		goto STATE13
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE13:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			fl, _ = strconv.ParseFloat(decStr+"e"+strconv.FormatInt(int64(expo*sign), 10), 64)
			obj = mkFlonum(fl)
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if i, ok := isDigit(c, 10); ok {
		expo = expo*10 + int(i)
		goto STATE13
	}
	fl, _ = strconv.ParseFloat(decStr+"e"+strconv.FormatInt(int64(expo*sign), 10), 64)
	obj = mkFlonum(fl)
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE14:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		err = mkYerror(errRead, errBadNumber)
		goto REFUSED
	}
	if _, ok = isDigit(c, 10); ok {
		decStr += string(c)
		goto STATE15
	}
	err = mkYerror(errRead, errBadNumber)
	goto REFUSED

STATE15:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			fl, _ = strconv.ParseFloat(decStr, 64)
			obj = mkFlonum(fl)
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if _, ok = isDigit(c, 10); ok {
		decStr += string(c)
		goto STATE15
	}
	if isExpoMarker(c) {
		if rdx != 10 {
			err = mkYerror(errRead, errBadNumber)
			goto REFUSED
		}
		goto STATE9
	}
	if c == '#' {
		goto STATE16
	}
	fl, _ = strconv.ParseFloat(decStr, 64)
	obj = mkFlonum(fl)
	prt.rdr.UnreadRune()
	goto ACCEPT

STATE16:
	c, _, err = prt.rdr.ReadRune()
	if err != nil {
		if err == io.EOF {
			fl, _ = strconv.ParseFloat(decStr, 64)
			obj = mkFlonum(fl)
			goto ACCEPT
		}
		err = mkYerror(errRead, errIntern)
		goto REFUSED
	}
	if c == '#' {
		goto STATE16
	}
	if isExpoMarker(c) {
		if rdx != 10 {
			err = mkYerror(errRead, errBadNumber)
			goto REFUSED
		}
		goto STATE9
	}
	fl, _ = strconv.ParseFloat(decStr, 64)
	obj = mkFlonum(fl)
	prt.rdr.UnreadRune()
	goto ACCEPT

ACCEPT:
	return obj, nil

REFUSED:
	return nil, err
}

func readSymbolOrNumber(prt *port, head rune) (yObj, error) {
	var (
		c rune
		e error
	)

	if head != -1 {
		c, e = head, nil
	} else {
		c, _, e = prt.rdr.ReadRune()
	}
	if e != nil {
		return nil, mkYerror(errRead, errIntern)
	}

	switch c {
	case '.':
		next, _, e := prt.rdr.ReadRune()
		if e != nil {
			return nil, mkYerror(errRead, errIntern)
		}
		if next == '.' {
			e = prt.rdr.UnreadRune()
			if e != nil {
				return nil, mkYerror(errRead, errIntern)
			}
			return readSymbol(prt, '.')
		}
		if unicode.IsDigit(next) {
			e = prt.rdr.UnreadRune()
			if e != nil {
				return nil, mkYerror(errRead, errIntern)
			}
			return readNumber(prt, c)
		}
		return nil, mkYerror(errRead, errSyntax, "malformed dot syntax")
	case '+', '-':
		next, _, e := prt.rdr.ReadRune()
		if e != nil {
			return nil, mkYerror(errRead, errIntern)
		}
		if unicode.IsDigit(next) || (next >= 'a' && next <= 'f') || next == 'i' || next == '.' {
			e = prt.rdr.UnreadRune()
			if e != nil {
				return nil, mkYerror(errRead, errIntern)
			}
			return readNumber(prt, c)
		}
		e = prt.rdr.UnreadRune()
		if e != nil {
			return nil, mkYerror(errRead, errIntern)
		}
		return readSymbol(prt, c)
	default:
		return nil, mkYerror(errRead, errSyntax, "unrecognized syntax")
	}
}

func readSymbol(prt *port, head rune) (yObj, error) {
	var (
		c   rune
		e   error
		str string
	)

	//STATE0:
	if head != -1 {
		c, e = head, nil
	} else {
		c, _, e = prt.rdr.ReadRune()
	}
	if e != nil {
		goto REFUSED
	}
	if (chTbl[c] & sINITIAL) != 0 {
		str += string(c)
		goto STATE1
	} else if c == '+' || c == '-' {
		next, _, e := prt.rdr.ReadRune()
		if e != nil {
			if e == io.EOF {
				str = string(c)
				goto STATE2
			}
			return nil, mkYerror(errRead, errIntern)
		}
		if isDelim(next) {
			str = string(c)
			goto STATE2
		}
		goto REFUSED
	} else if c == '.' { //1st '.'
		next, _, e := prt.rdr.ReadRune()
		if e != nil {
			goto REFUSED
		}
		if next == '.' { //2nd '.'
			next, _, e = prt.rdr.ReadRune()
			if e != nil {
				goto REFUSED
			}
			if next == ('.') { //3rd '.'
				next, _, e = prt.rdr.ReadRune()
				if e != nil {
					if e == io.EOF {
						str = "..."
						goto STATE2
					}
					return nil, mkYerror(errRead, errIntern)
				}
				if unicode.IsSpace(next) {
					str = "..."
					goto STATE2
				}
			}
		}
	}
	goto REFUSED

STATE1:
	c, _, e = prt.rdr.ReadRune()
	if e != nil {
		goto STATE2
	}
	if (chTbl[c] & sSUBSEQ) != 0 {
		str += string(c)
		goto STATE1
	} else {
		goto STATE2
	}

STATE2:
	prt.rdr.UnreadRune()
	return mkSymbol(str), nil

REFUSED:
	prt.rdr.UnreadRune()
	return nil, mkYerror(errRead, errBadIdentifier)
}

func readSpecial(prt *port, flgSyms map[string]yObj) (yObj, error) {
	c, _, e := prt.rdr.ReadRune()
	if e != nil || (c != '(' && isDelim(c)) {
		return nil, mkYerror(errRead, errSyntax, "malformed # syntax")
	}

	switch c {
	case 't':
		c, _, e = prt.rdr.ReadRune()
		if e != nil {
			if e == io.EOF {
				return yTRUE, nil
			}
			return nil, mkYerror(errRead, errIntern)
		}
		if isDelim(c) {
			prt.rdr.UnreadRune()
			return yTRUE, nil
		}
		return nil, mkYerror(errRead, errSyntax, "malformed # syntax")
	case 'f':
		c, _, e = prt.rdr.ReadRune()
		if e != nil {
			if e == io.EOF {
				return yFALSE, nil
			}
			return nil, mkYerror(errRead, errIntern)
		}
		if isDelim(c) {
			prt.rdr.UnreadRune()
			return yFALSE, nil
		}
		return nil, mkYerror(errRead, errSyntax, "malformed # syntax")
	case '(':
		return readVector(prt, flgSyms)
	case ':':
		if len(flgSyms) == 0 {
			return nil, mkYerror(errRead, errSyntax, "malformed # syntax")
		}
		c, _, e = prt.rdr.ReadRune()
		if e != nil || !unicode.IsLower(c) {
			return nil, mkYerror(errRead, errSyntax, "malformed # syntax")
		}
		spStr := "#:" + string(c)
		c, _, e = prt.rdr.ReadRune()
		if e != nil || !unicode.IsDigit(c) {
			return nil, mkYerror(errRead, errSyntax, "malformed # syntax")
		}
		spStr += string(c)
		for {
			c, _, e = prt.rdr.ReadRune()
			if e != nil {
				if e == io.EOF {
					break
				}
				return nil, mkYerror(errRead, errIntern)
			}
			if !unicode.IsDigit(c) {
				break
			}
			spStr += string(c)
		}
		prt.rdr.UnreadRune()
		if obj, ok := flgSyms[spStr]; ok {
			return obj, nil
		}
		return nil, mkYerror(errRead, errSyntax, "malformed # syntax")
	case '\\':
		return readChar(prt)
	case 'b', 'o', 'd', 'x', 'i', 'e':
		prt.rdr.UnreadRune()
		return readNumber(prt, '#')

	}

	return nil, mkYerror(errRead, errSyntax, "malformed # syntax")
}

func readChar(prt *port) (yObj, error) {
	var (
		c, ch    rune
		e        error
		isChName bool
	)

	//STATE0:
	c, _, e = prt.rdr.ReadRune()
	if e != nil {
		goto REFUSED
	}
	if isDelim(c) {
		if c == '\n' {
			goto REFUSED
		}
		ch = c
		_, _, _ = prt.rdr.ReadRune()
		goto ACCEPT
	}
	ch = c
	switch c {
	case 's':
		goto STATE1
	case 'n':
		goto STATE2
	default:
		goto STATE3
	}

STATE1:
	isChName, e = readChSPACE(prt)
	if e != nil {
		goto REFUSED
	}
	if isChName {
		ch = ' '
	}
	goto ACCEPT

STATE2:
	isChName, e = readChNEWLINE(prt)
	if e != nil {
		goto REFUSED
	}
	if isChName {
		ch = '\n'
	}
	goto ACCEPT

STATE3:
	c, _, e = prt.rdr.ReadRune()
	if e != nil {
		if e == io.EOF {
			goto ACCEPT
		}
		return nil, mkYerror(errRead, errIntern)
	}
	if isDelim(c) {
		goto ACCEPT
	}
	goto REFUSED

ACCEPT:
	prt.rdr.UnreadRune()
	return mkChar(ch), nil

REFUSED:
	prt.rdr.UnreadRune()
	return nil, mkYerror(errRead, errBadCharacter)
}

func readChSPACE(prt *port) (bool, error) {
	var (
		e        error
		c        rune
		isChName bool
	)

	c, _, e = prt.rdr.ReadRune()
	if e != nil {
		if e == io.EOF {
			goto ACCEPT
		}
		goto REFUSED
	}
	if isDelim(c) {
		goto ACCEPT
	}
	if c != 'p' {
		e = yErrReadError
		goto REFUSED
	}

	c, _, e = prt.rdr.ReadRune()
	if e != nil {
		goto REFUSED
	}
	if c != 'a' {
		e = yErrReadError
		goto REFUSED
	}

	c, _, e = prt.rdr.ReadRune()
	if e != nil {
		goto REFUSED
	}
	if c != 'c' {
		e = yErrReadError
		goto REFUSED
	}

	c, _, e = prt.rdr.ReadRune()
	if e != nil {
		goto REFUSED
	}
	if c != 'e' {
		e = yErrReadError
		goto REFUSED
	}

	c, _, e = prt.rdr.ReadRune()
	if e != nil {
		if e == io.EOF {
			goto ACCEPT
		}
		goto REFUSED
	}
	if !isDelim(c) {
		e = yErrReadError
		goto REFUSED
	}

	isChName = true

ACCEPT:
	prt.rdr.UnreadRune()
	return isChName, nil

REFUSED:
	prt.rdr.UnreadRune()
	return false, e
}

func readChNEWLINE(prt *port) (bool, error) {
	var (
		e        error
		c        rune
		isChName bool
	)

	c, _, e = prt.rdr.ReadRune()
	if e != nil {
		if e == io.EOF {
			goto ACCEPT
		}
		goto REFUSED
	}
	if isDelim(c) {
		goto ACCEPT
	}
	if c != 'e' {
		e = yErrReadError
		goto REFUSED
	}

	c, _, e = prt.rdr.ReadRune()
	if e != nil {
		goto REFUSED
	}
	if c != 'w' {
		e = yErrReadError
		goto REFUSED
	}

	c, _, e = prt.rdr.ReadRune()
	if e != nil {
		goto REFUSED
	}
	if c != 'l' {
		e = yErrReadError
		goto REFUSED
	}

	c, _, e = prt.rdr.ReadRune()
	if e != nil {
		goto REFUSED
	}
	if c != 'i' {
		e = yErrReadError
		goto REFUSED
	}

	c, _, e = prt.rdr.ReadRune()
	if e != nil {
		goto REFUSED
	}
	if c != 'n' {
		e = yErrReadError
		goto REFUSED
	}

	c, _, e = prt.rdr.ReadRune()
	if e != nil {
		goto REFUSED
	}
	if c != 'e' {
		e = yErrReadError
		goto REFUSED
	}

	c, _, e = prt.rdr.ReadRune()
	if e != nil {
		if e == io.EOF {
			goto ACCEPT
		}
		goto REFUSED
	}
	if !isDelim(c) {
		e = yErrReadError
		goto REFUSED
	}

	isChName = true

ACCEPT:
	prt.rdr.UnreadRune()
	return isChName, nil

REFUSED:
	prt.rdr.UnreadRune()
	return false, e
}

func readVector(prt *port, flgSyms map[string]yObj) (yObj, error) {
	vec := make([]yObj, 0)

	for {
		c, err := prt.nextRune()
		if err != nil {
			if err == io.EOF {
				return nil, mkYerror(errRead, errIncompExp, "EOF in vector")
			}
			return nil, mkYerror(errRead, errIntern)
		}
		if c == ')' {
			break
		}
		prt.rdr.UnreadRune()
		elm, err := readExp(prt, flgSyms)
		if err != nil {
			return nil, err
		}
		vec = append(vec, elm)
	}

	return mkVector(vec), nil
}

func readExpWithFlags(src string, flgSyms map[string]yObj) (yObj, error) {
	re := regexp.MustCompile("#:.")
	locs := re.FindAllStringIndex(src, len(flgSyms))

	numLocs := len(locs)
	var tSrc string
	if numLocs == 0 {
		tSrc = src
	} else {
		tSrc = src[0:locs[0][0]] + src[locs[0][0]:locs[0][1]] + strconv.Itoa(0)
		for idx := 1; idx < numLocs; idx++ {
			tSrc += src[locs[idx-1][1]:locs[idx][0]]
			tSrc += (src[locs[idx][0]:locs[idx][1]] + strconv.Itoa(idx))
		}
		tSrc += src[locs[numLocs-1][1]:]
	}

	prt := NewPort(tSrc)

	return readExp(prt, flgSyms)
}
