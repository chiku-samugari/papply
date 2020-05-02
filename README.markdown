# PAPPLY - macros for partial application

PAPPLY is a macro family that offers a shorthand form to write the
partial application of fucntions.

`PAPPLY` macro generates a function object by partially applying the
fucntion specified as the first argument to the rest arguments. A
special symbol `_` is utilized as a place holder that denotes
not-yet-fixed arguments.

    (papply (list (1+ _) (parse-integer _)))
    ;=> #'(LAMBDA (#:P0 #:P1 &REST #:REST0)
             (APPLY #'LIST (1+ #:P0) (PARSE-INTEGER #:P1) #:REST0))

`APAPPLY` macro introduces anaphoras and made possible to utilize one
parameter multiple times. A symbol `An` (n is a natural number) appears
in `APAPPLY` form is recognized as the nth parameter of generated
function object.

    (apapply (list (length (string a0)) (format nil "~a-index" a0)))
    ;=> #'(LAMBDA (A0 &REST #:REST0)
            (APPLY #'LIST (LENGTH (STRING A0)) (FORMAT NIL "~a-index" A0) #:REST0))

`P` macro is a convenient interface for `PAPPLY` and `APAPPLY` macro. It
is converted into `PAPPLY` form if the special symbol `_` appears in the
arguments. In other cases it is converted into `APAPPLY` form.

    (p (list (length (string a0)) (format nil "~a-index" a0)))
    ;=> (APAPPLY (LIST (LENGTH (STRING A0)) (FORMAT NIL "~a-index" A0)))

Additionally, `EXTEND-SHARP-QUOTE` function is provided. It activates
the extension for `#'` reader macro that enables to write a function
name after the left parenthesis following to `#'` reader macro by
convert such form into `P` macro call.

    #'(list (length (string a0)) (format nil "~a-index" a0))
    ;=> (P (LIST (LENGTH (STRING A0)) (FORMAT NIL "~a-index" A0)))

## Macro PAPPLY

    papply fn &rest args => function
    papply form => function

    fn: a function.
    args: objects.
    form: a form.
    function : a partially applied function.

### Basic usage

PAPPLY macro generates a partially applied function. In the first
format, `papply fn &rest args`, `fn` must be a variable that is bound to
a function or a CL:FUNCTION form and PAPPLY generates a partially
applied function of the function whose first m arguments are partially
applied. Here, m is the number of elemetns in `args` except forms that
includes the symbol `_`. The special symbol `_` works as the place
holder for arguments that are not yet fixed. The kth `_` is replaced by
the kth parameter of the result partially applied function. The order is
from left to right; depth-first-order. This format is called *function
format*.

In the second format, `papply form`, the first argument `form` should
be a form and it is interpreted as follows:

     (op &rest forms)

`op` must be a symbol that names a function, a macro, or a special
operator and PAPPLY generates an n-ary function where n is the number of
the appearance of `_` in `forms`. The body of the generated function is
a list whose CAR is `op` and CDR is `forms` but all the place holders
`_` are replaced by parameters of the function. Similraly to the first
format, the kth appearance of `_` is replaced by the kth parameter of
the generated function. The rest parameter `args` is not used in this
format. This format is called *form format*.

### Examples

    (papply #'list _ (1+ _) 'a)     ; function format
    (let ((vector #'list))
      (papply vector _ (1+ _) 'a)   ; function format
    (papply (list _ (1+ _) 'a))     ; form format

These are all converted into a lambda expression behaves same as the
following lambda expression.

    (lambda (x y &rest restparams)
      (apply #'list x (1+ y) 'a restparams))

### ARGS parameter detail

Not only atoms but also forms are accepted as elements of `args` (in
function format) and `forms` (in form format). A form that includes the
special symbol `_` is naturally accepted.

e.g.) remove the pathnames whose type is equal to "lisp".
(assume `files` is the list of pathnames.)

    (remove-if-not (papply (string= (pathname-type _) "lisp")) files)

## Macro APAPPLY

    apapply op &rest args => function
    apapply form => function

    fn: a function.
    args: objects.
    form: a form.
    function: a function.

### Detail

`APAPPLY` is an anaphoric variation of `PAPPLY`. It allows to use
anaphoric variables (anaphoras) in the `args` argument instead of `_`
symbol. Since anaphoras have name, one anaphora can be used multiple
times. In `PAPPLY`, one not-fixed argument (expressed by `_` symbol)
cannot be used multiple times because different appearance of `_` is
recognized as different arguments.

An anaphora is a symbol whose name is `A` followed by a non-negative
integer. More unambiguously, a symbol whose name mathces to the regular
expression below.

     A[1-9][0-9]* | A0

All the symbols that fulfills this condition is recognized as anaphoras
even if they are desultory indexed. All anaphoras are sorted by its
index in the ascending order and used as the parameter of returned
function. Therefore, the returned functions from next 2 forms behave
identical.

    (apapply (list a0 (mod a0 a1) (/ a0 a1)))
    ;=> #'(LAMBDA (A0 A1 &REST #:REST0)
            (APPLY #'LIST A0 (MOD A0 A1) (/ A0 A1) #:REST0))

    (apapply (list a7 (mod a7 a20) (/ a7 a20)))
    ;=> #'(LAMBDA (A20 A7 &REST #:REST0)
            (APPLY #'LIST A7 (MOD A7 A20) (/ A7 A20) #:REST0))

The format of its arguments (`fn` and `args`) is identical to that of
`PAPPLY`. Please see the `PAPPLY` chapter for detail.

## Macro P

    p op &rest args => function
    p form => function

    fn: a function.
    args: objects.
    form: a form.
    function: a function.

### Detail

P macro is a convenient interface for `PAPPLY` and `APAPPLY` macro. It
is converted into `PAPPLY` form if the special symbol `_` appears in the
arguments. In other cases it is converted into `APAPPLY` form.

    (p (list (1+ _) (string _)))
    ;=> (PAPPLY (LIST (1+ _) (STRING _)))
    (p (list (length (string a0)) (format nil "~a-index" a0)))
    ;=> (APAPPLY (LIST (LENGTH (STRING A0)) (FORMAT NIL "~a-index" A0)))

If both `_` and anaphora candidates (symbol whose name is `An` where n
is a number) is used, it is converted into `PAPPLY` and no anaphoras are
introduced -- they will just be treated as ordinary lexical variables.

    (p (list (1+ _) (string a0)))
    ;=> (PAPPLY (LIST (1+ _) (STRING A0)))
    ;=> #'(LAMBDA (#:P0 &REST #:REST0)
            (APPLY #'LIST (1+ #:P0) (STRING A0) #:REST0))

## Function EXTEND-SHARP-QUOTE

It enables an extension of `#'` reader macro. The built-in `#'` reader
macro does not allow us to write a function name after the left
parenthesis follows to `#'`.  The only thing we can write at that
position is the symbol `LAMBDA` and `SETF`. The extension activated by
this function allows us to write arbitrary function by converting such
form into `P` macro call form.

    #'(list 1 _ 3)
    ; => (P LIST 1 _ 3)

This extension never harms the behavior of built-in `#'` reader macro
because it does not convert the `LAMBDA` forms and `SETF` forms into `P`
macro call form. It just utilizes the unused room of the reader macro.
About the detail of `P` macro, see the section for `P` macro.

## miscellaneous

The CL:FUNCTION form use case of the function format, `#'list` for
example, can be seen as the form format.

    (papply #'list 0 _ 1)

is identical to

    (papply (cl:function list) 0 _ 1)

and CL:FUNCTION is a special operator. PAPPLY treats CL:FUNCTION
specially and no work around is offered by PAPPLY to use CL:FUNCTION
with the form format.

One big disappointing aspect of current `PAPPLY` implementation is the
nested use of `PAPPLY` forms. It's not supported. For `PAPPLY` it should
be supported because the scope of `_` is clear. About `APAPPLY` macro on
the other hand, I have no reasonable criteria to decide the scope of
anaphoras. The inner `APAPPLY` could refer the anaphoras. And too much
use of such `APAPPLY` harms the readability. It seems interesting to
study how to implement it, but practically, it will not be used I guess.

## Author and License

Author : chiku (Takehiko Nawata, samugari.penguin@gmail.com)

License : MIT License
