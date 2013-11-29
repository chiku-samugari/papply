# PAPPLY - macros for partial application

PAPPLY offers a shorthand form to write a partial
application of a function. The semantics are simple, exact and visual.
There are three macros PAPPLY, APAPPLY and P and also a custom reader
macro extension and its enabler function.

In PAPPLY, a special symbol `_` can be used to denote an unspecified,
or not-yet-fixed, value. You can specify the actual value in the
later call.

    (papply (list (1+ _) (parse-integer _)))
    
    ;=> #'(LAMBDA (#:P0 #:P1 &REST #:REST0)
             (APPLY #'LIST (1+ #:P0) (PARSE-INTEGER #:P1) #:REST0))
             
    (funcall * 3 "10")
    
    ;=> (4 10)
             

APAPPLY is similar but only accepts anaphoric variables instead of
 `_`.
Those values are evaluated once only
and the values can be reused many times in a template.
A symbol `A<n>` (n is a number) is going to be the nth parameter of generated function object.

    (apapply (list (length (string a0)) (format nil "~a-index" a0)))
    ;=> #'(LAMBDA (A0 &REST #:REST0)
            (APPLY #'LIST (LENGTH (STRING A0)) (FORMAT NIL "~a-index" A0) #:REST0))

P macro is a shorthand convenience macro built on PAPPLY and APAPPLY.
It automatically detects a symbol `_` in its body and become a PAPPLY.
Otherwise it is converted into APAPPLY form.

    (p (list (length (string a0)) (format nil "~a-index" a0)))
    ;=> (APAPPLY (LIST (LENGTH (STRING A0)) (FORMAT NIL "~a-index" A0)))

Additionally, EXTEND-SHARP-QUOTE function is provided. It activates the
extension for #' reader macro that enables to write a function name after
the left parenthesis following to #' reader macro by convert such form into P macro
call.

    #'(list (length (string a0)) (format nil "~a-index" a0))
    ;=> (P (LIST (LENGTH (STRING A0)) (FORMAT NIL "~a-index" A0)))

## Macro PAPPLY

Here is an BNF-ish definition of PAPPLY:

    (papply (<op> <arg>*)) => function
    (papply <op> <arg>*) => function
    <op>     := <symbol> | <fobj>
    <symbol> := <dynamic-bound> | <lex-fbound>
    <dynamic-bound> := a symbol dynamically bound to a function.
    <lex-fbound>    := a symbol lexically fbound to a function.
    <fobj>   := (function <lex-fbound>) | #'<lex-fbound>
    <arg>    := _ | <arg> | <form>
    <form>   := an arbitrary lisp form.
    function : a function object.

### Basic usage

PAPPLY macro generates a function object by applying the first m arguments
of `op` to `args` where m is the number of elements in `args` except a form
that includes symbol `_`.  Special symbol `_` works as the place holder for
not-yet-fixed arguments. The nth `_` is replaced by the nth argument of the
result function object.  The order is from left to right -- depth-first-order
in other terminology. For example,

    (papply (list _ (1+ _) 'a))

is converted into a lambda expression identical to the following:

    #'(lambda (param0 param1 &rest restparams)
        (apply #'list param0 (1+ param1) 'a restparams))

### Variation of arguments

This macro accepts 2 different format. One is called *form format*
because its argument seems like one form:

    (papply (list 0 (1+ _)))

The another format is called *apply format* because it seems much like
the ordinary `cl:apply`.

    (papply #'list 0 (1+ _))

Both of them accepts some more variations. As described later,
`op` can be a symbol `fbound` to a function or a symbol that is used as a
lexical variable bound to a function object.
Thus, all of the followings:

    (papply (list 0 (1+ _)))
    (papply (#'list 0 (1+ _)))
    (let ((- #'list)) ; SBCL Only
     (papply (- 0 (1+ _))))
    (papply #'list 0 (1+ _))
    (papply list 0 (1+ _))
    (let ((- #'list)) ; SBCL Only
     (papply - 0 (1+ _)))

... result in an identical function:

    #'(lambda (param0 &rest restparam)
        (apply #'list 0 (1+ param0) restparam))

### OP parameter detail


Symbols are valid to be specified as `op`. If a symbol is given to `op`,
then normally it is treated as a symbol that names a function. But if the
symbol is a variable which is lexically bound, then it is treated as a
variable. If one have to pass a function whose name is identical
to such lexically bound symbol, use function object instead of symbol
(just put #' in front of the symbol).

The special handling of lexical variable is available only in SBCL. A symbol
given to `op` is always treated as a name of function in other
implementations.

### ARGS parameter detail

Not only atoms but also forms are accepted as elements of `args`.
A form that includes the special symbol `_` is naturally accepted.

e.g.) remove the pathnames whose type is equal to "lisp".
(assume `files` is the list of pathnames.)

    (remove-if-not (papply (string= (pathname-type _) "lisp")) files)

Macro call forms and special operator forms are invalid arguments for
`args`. Although IDENTITY function can be used as a wrapper in some cases,
LAMBDA expression instead of PAPPLY macro can be better in such case.

## Macro APAPPLY

Other definitions being equal to PAPPLY, the BNF-ish definition of
APAPPLY semantics follow:

    (apapply (<op> <a-arg>*)) => function
    (apapply <op> <a-arg>*) => function
    <a-arg>    := <anaphoric> | <a-arg> | <form>
    <anaphoric> := A[1-9][0-9]* | A0   --- case insensitive

### Detail

APAPPLY is an upper compatible version of PAPPLY. It accepts anaphoric
variables (anaphoras) instead of symbol `_`. 

In PAPPLY, the value of an not-fixed argument (symbol `_`) is not
shared between variables because different appearance of `_` is
recognized as different arguments. Anaphoric variables introduced by
APAPPLY allows such sharings.

An anaphoric variable available in APAPPLY
is a symbol whose name is `A` followed by a non-negative integer,
or whose name matches to the regular expression below:

     A[1-9][0-9]* | A0

The index of the symbols can be desultory.
All anaphoras are sorted by its index in the
ascending order and used as the parameter of returned function.
Therefore, the behaviors of the resulting functions below are identical.

    (apapply (list a0 (mod a0 a1) (/ a0 a1)))
    ;=> #'(LAMBDA (A0 A1 &REST #:REST0)
            (APPLY #'LIST A0 (MOD A0 A1) (/ A0 A1) #:REST0))

    (apapply (list a7 (mod a7 a20) (/ a7 a20)))
    ;=> #'(LAMBDA (A20 A7 &REST #:REST0)
            (APPLY #'LIST A7 (MOD A7 A20) (/ A7 A20) #:REST0))

## Macro P

    (p (<op> <p-args>)) => function
    (p <op> <p-args>) => function
    <p-args>    := <a-arg>* | <arg>*

The formal definitions of `<arg>` and `<a-arg>` are already described
in `PAPPLY` and `APAPPLY` section.

### Detail

P macro is a convenient interface for PAPPLY and APAPPLY macro. It is converted
into PAPPLY form if the special symbol `_` appears in the arguments. In other
cases it is converted into APAPPLY form.

    (p (list (1+ _) (string _)))
    ;=> (PAPPLY (LIST (1+ _) (STRING _)))
    (p (list (length (string a0)) (format nil "~a-index" a0)))
    ;=> (APAPPLY (LIST (LENGTH (STRING A0)) (FORMAT NIL "~a-index" A0)))

If both `_` and anaphora candidates (symbol whose name is `An` where n is a
number) is used, it is converted into PAPPLY and no anaphoras are
introduced -- they will just be treated as ordinary lexical variables.

    (p (list (1+ _) (string a0)))
    ;=> (PAPPLY (LIST (1+ _) (STRING A0)))
    ;=> #'(LAMBDA (#:P0 &REST #:REST0)
            (APPLY #'LIST (1+ #:P0) (STRING A0) #:REST0))

## Function EXTEND-SHARP-QUOTE

It enables an extension of #' reader macro. The built-in #' reader macro
does not allow us to write a function name after the left parenthesis following
to #'. The only thing we can write at that position is the symbol LAMBDA.
The extension activated by this function allows us to write arbitrary function
by converting such form into P macro call form.

    #'(list 1 _ 3)
    ; => (P LIST 1 _ 3)

This extension never harms the behavior of built-in #' reader macro because it
does not convert the LAMBDA expression into P macro call form. It just utilizes
the unused room of the reader macro.

About the detail of P macro, see the section for P macro.

## miscellaneous

One big disappointing aspect of current PAPPLY implementation is the nested use
of PAPPLY forms. Its not supported. For PAPPLY it should be supported because
the scope of `_` is clear.
About APAPPLY macro on the other hand, I have no reasonable criteria to decide
the scope of anaphoras. The inner APAPPLY could refer the anaphoras.
And too much use of such APAPPLY harms the readability. It seems interesting to
study how to implement it, but practically, it will not used I guess.

The second point is various formats. As it is explained in the previous
chapters, it accepts 6 different formats. In my own programming activity, I
only use form format.

The third point is the operators other than functions. In Common Lisp, there
are macros, special operators and lambda expressions. There is no worth to
handle lambda expressions in PAPPLY. Macros and special operator can be worth
to support.

## Author and License

Author : chiku (Takehiko Nawata, samugari.penguin@gmail.com)

License : MIT License
