;;; PAPPLY
;;; papply.lisp
;;; main implementation.
;;; Author: chiku (Takehiko Nawata samugari.penguin@gmail.com)
;;; License: MIT License
(in-package :papply)


(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun underscore-included-p (tree)
    (cond ((symbolp tree) (eq '_ tree))
          ((atom tree) nil)
          (t (some #'underscore-included-p tree))))

  (defun embody-template (template)
    (let* (concrete-symbols
           (embodied (with-tree-leaves template (eq leaf '_)
                       (push (gensym "PARG") concrete-symbols)
                       (car concrete-symbols))))
      (values embodied (nreverse concrete-symbols)))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun construct-body (op not-applied-args body env)
    (if (and (symbolp op) (not (lexically-bound-p op env)))
      (if (or (macro-function op) (special-operator-p op))
        `(,op ,@body)
        `(apply #',op ,@body ,not-applied-args))
      `(apply ,op ,@body ,not-applied-args))))

(defmacro papply-function (fn &rest args)
  (multiple-value-bind (embodied-args params) (embody-template args)
    (with-gensyms (more-args)
      `(lambda (,@params &rest ,more-args)
         (declare (ignorable ,more-args))
         (apply ,fn ,@embodied-args ,more-args)))))

;;; Even though this macro is not exported for now, it might be because
;;; PAPPLY mixes 2 different format and that might be irritating and/or
;;; troublesome. In that situation this macro must accept the actual
;;; form format:
;;;
;;;   (papply-form (list 0 _ 1))
;;;
;;; The parameter construction should not be changed therefore.
(defmacro papply-form ((op &rest forms))
  (multiple-value-bind (embodied-args params) (embody-template forms)
    (if (and (symbolp op)
             (or (macro-function op) (special-operator-p op)))
      `(lambda (,@params)
         (,op ,@embodied-args))
      (with-gensyms (more-args)
        `(lambda (,@params &rest ,more-args)
           (declare (ignorable ,more-args))
           (apply (function ,op) ,@embodied-args ,more-args))))))

(defmacro papply (fn &rest args)
  " papply fn &rest args => function
    papply form => function
    fn: a function.
    args: objects.
    form: a form.
    function : a partially applied function.

   PAPPLY macro generates a partially applied function. In the first
  format, `fn` must be a variable that is bound to a function or a
  CL:FUNCTION form and PAPPLY generates a partially applied function of
  the function whose first m arguments are partially applied. Here, m is
  the number of elemetns in `args` except forms that includes the symbol
  `_`. The special symbol `_` works as the place holder for arguments
  that are not yet fixed. The kth `_` is replaced by the kth parameter
  of the result partially applied function. The order is from left to
  right; depth-first-order.

   In the second format, the first argument `form` should be a form and
  it is interpreted as follows:

     (op &rest forms)

  `op` must be a symbol that names a function, a macro, or a special
  operator and PAPPLY generates an n-ary function where n is the number
  of the appearance of `_` in `forms`. The body of the generated
  function is a list whose CAR is `op` and CDR is `forms` but all the
  place holders `_` are replaced by parameters of the function.
  Similraly to the first format, the kth appearance of `_` is replaced
  by the kth parameter of the generated function. The rest parameter
  `args` is not used in this format.

   For example,

      (papply #'list _ (1+ _) 'a)     ; First format
      (papply (list _ (1+ _) 'a))     ; Second format

  are both converted into a lambda expression behaves same as the
  following lambda expression.

      (lambda (x y &rest restparams)
        (apply #'list x (1+ y) 'a restparams))
  "
  (if (or (symbolp fn)
          (and (consp fn) (eq (car fn) 'cl:function)))
    `(papply-function ,fn ,@args)
    `(papply-form ,fn)))

;;; APAPPLY
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun anaphorap (sym)
    " Retruns T if the name of the given symbol ``SYM'' is a member of
     following regular language:
     A[1-9][0-9]*|A0
     "
    (let ((name (symbol-name sym)))
      (and (<= 2 (length name))
           (string= (take name) "A")
           (every #'digit-char-p (drop name))
           (or (= (length name) 2)
               ;; Disables padding 0
               (not (char= (elt name 1) #\0))))))

    (defun unquotedp (obj)
      #+sbcl
      (if (sb-impl::comma-p obj)
        (values t (sb-impl::comma-expr obj))))

    (defun anaphora-list (tree)
      " Returns a list that is composed of symbols whose ANAPHORAP check
       is T. Each symbol appears only once in the list and the list is
       sorted by the name."
      (labels ((collect (tree)
                 (filter (lambda (obj)
                           (or (and (symbolp obj) (anaphorap obj) obj)
                               #+sbcl
                               (multiple-value-bind (? expr)
                                 (unquotedp obj)
                                 (and ? (collect expr)))))
                         (flatten tree))))
        (sort (remove-duplicates (flatten (collect tree)))
              #'string<= :key #'symbol-name))))

(defmacro apapply-enumerate-format (op &rest args &environment env)
  (with-gensyms (not-applied-args)
    `(lambda (,@(anaphora-list args) &rest ,not-applied-args)
       (declare (ignorable ,not-applied-args))
       ,(construct-body op not-applied-args args env))))

(defmacro apapply-form-format ((op &rest args))
  `(apapply-enumerate-format ,op ,@args))

(defmacro apapply (&rest op-and-args)
  (cond ((or (atom (car op-and-args))
             (eq (caar op-and-args) 'function))
         `(apapply-enumerate-format ,(car op-and-args) ,@(cdr op-and-args)))
        (t `(apapply-form-format ,@op-and-args))))

(defmacro p (&rest op-and-args)
  " P macro is a convinient interface for PAPPLY and APAPPLY macro. It is
   converted into PAPPLY form if the special symbol `_` appears in the
   arguments. In other cases it is converted into APAPPLY form."
  (cond ((underscore-included-p op-and-args)
         `(papply ,@op-and-args))
        ((some (lambda (x) (and (symbolp x) (anaphorap x)))
               (flatten op-and-args))
         `(apapply ,@op-and-args))
        (t `(papply ,@op-and-args))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun |#'-reader| (strm c n)
    (declare (ignore c n))
    (let ((expr (read strm t nil t)))
      (if (and (consp expr)
               (symbolp (car expr))
               (not (inq (car expr)
                         cl:lambda
                         cl:setf
                         #+sbcl sb-int:named-lambda)))
        `(p ,@expr)
        `(function ,expr)))))

(defun extend-sharp-quote ()
  " EXTEND-SHARP-QUOTE activates an extension for #' reader macro. The
   extension allows us to write arbitrary function after the left parenthesis
   that follows to #' by converting such form into PAPPLY:P macro call form.
  "
  (set-dispatch-macro-character #\# #\' (function |#'-reader|)))
