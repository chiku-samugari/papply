;;; PAPPLY
;;; papply.lisp
;;; main implementation.
;;; Author: chiku (Takehiko Nawata samugari.penguin@gmail.com)
;;; License: MIT License
(in-package :papply)

(eval-when (:compile-toplevel :execute)
  #+sbcl (require :sb-cltl2))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun underscore-included-p (tree)
    (cond ((symbolp tree) (eq '_ tree))
          ((atom tree) nil)
          (t (some #'underscore-included-p tree))))

  (defun count-underscore (tree)
    (let ((c 0))
      (with-tree-leaves tree (eq leaf '_) (incf c))
      c))

  (defun lexically-bound-p (name env)
    (or
      #+sbcl (sb-cltl2:variable-information name env)
      nil)))

(defmacro papply-listup-format (fn &rest args &environment env)
  (let* ((gensym-lst)
         (body (with-tree-leaves args (eq leaf '_)
                 (car (push (gensym "PARG") gensym-lst)))))
    (with-gensyms (not-applied-args)
      `(lambda (,@(nreverse gensym-lst) &rest ,not-applied-args)
         (apply
           ,(if (and (symbolp fn) (not (lexically-bound-p fn env)))
              `#',fn
              fn)
           ,@body ,not-applied-args)))))

(defmacro papply-form-format ((op &rest args))
  `(papply-listup-format ,op ,@args))

(defmacro papply (&rest op-and-args)
  " papply (op &rest args) => function ;{{{
    papply op &rest args => function
    op : a symbol or a function object.
    args : objects.
    function : a function object.

    PAPPLY macro generates a function object by applying the first m arguments
    of `op` to `args` where m is the number of elemetns in `args` except a form
    that includes symbol `_`.  Special symbol `_` works as the place holder for
    not-yet-fixed arguments. The nth `_` is replaced by the nth argument of the
    result function object.  The order is from left to right. In other terminology,
    depth-first-order.
    For example,
    
        (papply (list _ (1+ _) 'a))

    is converted into a lambda expression behaves same as following lambda
    expression.

    #'(lambda (param0 param1 &rest restparams)
        (apply #'list param0 (1+ param1) 'a restparams))
  ;}}}"
  (cond ((or (atom (car op-and-args)) (eq (caar op-and-args) 'function))
         `(papply-listup-format ,(car op-and-args) ,@(cdr op-and-args)))
        (t `(papply-form-format ,@op-and-args))))

;;; APAPPLY
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun anaphorap (sym)
    " Retruns T if the name of the given symbol ``SYM'' is
     a member of following regular language:
     A[1-9][0-9]*|A0
     "
    (let ((name (symbol-name sym)))
      (and (<= 2 (length name))
           (string= (take name) "A")
           (every #'digit-char-p (drop name))
           (or (= (length name) 2)
               ;; Disables padding 0
               (not (char= (elt name 1) #\0))))))

    (defun anaphora-list (tree)
      " Returns a list that is composed of symbols whose ANAPHORAP check is T.
       Each symbol appears only once in the list and the list is sorted by the name.
       "
      (sort (remove-duplicates
              (remove-if-not (lambda (sym) (and (symbolp sym) (anaphorap sym)))
                             (flatten tree)))
            #'string<= :key #'symbol-name)))

(defmacro apapply-listup-format (op &rest args &environment env)
  (with-gensyms (not-applied-args)
    `(lambda (,@(anaphora-list args) &rest ,not-applied-args)
       (apply ,(if (and (symbolp op) (not (lexically-bound-p op env)))
                 `#',op
                 op)
              ,@args ,not-applied-args))))

(defmacro apapply-form-format ((op &rest args))
  `(apapply-listup-format ,op ,@args))

(defmacro apapply (&rest op-and-args)
  (cond ((or (atom (car op-and-args))
             (eq (caar op-and-args) 'function))
         `(apapply-listup-format ,(car op-and-args) ,@(cdr op-and-args)))
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
                         #+sbcl sb-int:named-lambda)))
        `(p ,@expr)
        `(function ,expr)))))

(defun extend-sharp-quote ()
  " EXTEND-SHARP-QUOTE activates an extension for #' reader macro. The
   extension allows us to write arbitrary function after the left parenthesis
   that follows to #' by converting such form into PAPPLY:P macro call form.
  "
  (set-dispatch-macro-character #\# #\' (function |#'-reader|)))
