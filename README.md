yuyo
===============

Yuyo is a implementation of Programing Language Scheme aiming at
being incorporated into and used in Go.

Also it is easy to use and works well as stand-alone.

(but experimental implementation for now)

Installation
===============

To install yuyo interpreter, just use the following:

```
	$ go get github.com/koji-m/yuyo
```

Or it's possible to install as following:

```
	$ git clone https://github.com/koji-m/yuyo.git
	$ cd yuyo
	$ go install
```

Then yuyo command file is installed to $GOPATH/bin .  
(Environment variable $GOPATH is required to be set in advance.)

Now you can test it using:

```
	$ $GOPATH/bin/yuyo version
```

Usage
===============
Yuyo can be used as stand-alone Scheme interpreter as well as
built-in language for Go.  
(In this section, I suppose that the $PATH is set appropriately.)
###Using stand-alone interpreter

To invoke the interative session of yuyo (invoke REPL), just enter:

```
$ yuyo
```

This brings up a prompt `yuyo>`. You can enter Scheme expressions and
test it interactively.
To exit the interactive session, enter `Ctrl-D` to terminate yuyo.

You can use the interpreter to run Scheme script file as following:

```
$ yuyo foo.scm
```

(running foo.scm Scheme script file)

For more information about the usage, use:

```
$ yuyo help
```

###Using in Go
Simplest way to use yuyo to evaluate the Scheme expression in Go
program is that, firstly import the "yuyocore" package into, then
invoke the "Run" method in your program. For the following example
evaluates the Scheme expression `(let ((x 2) (y 3.14)) (+ x y))` and
prints the result.

```go
package main
```

Other than the method described above (one shot evaluation), you can
evaluate expressions continually like following. In this case, the
status, environment etc., is preserved until the VM object (the value of m in the following case) is abondoned.
 
```go
package main
```

Development
===============
Yuyo is a experimental application of Scheme interpreter.
Its design is based on the stack-based model. My implementation is
naive and supporting syntaxes and procedures are very insufficicent
at this stage.   
I intend to gradually implement the features in future.

#####supporting syntaxes
quote quasiquote( ` ) lambda if set! define cond case and or  
let let* letrec begin do

#####supporting procedures
eqv? number? complex? real? rational? integer? exact? inexact?  
char=? pair? null? list? = < + - cons car cdr set-car! set-cdr!  
list length append reverse memv list->vector call/cc display  

#####implementation plan candidate
* Multiple return values
* Hygienic macro
* Procedures specified in R7RS

Lincense
===============

Yuyo is released under the MIT License, see LICENSE.txt.