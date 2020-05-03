(in-package :papply)

(defun mylist (&rest args) args)

(and
  ;; PAPPLY
  (null (funcall (papply #'mylist)))
  (null (funcall (papply (mylist))))
  (null (funcall (papply #'list)))
  (null (funcall (papply (list))))
  (equal (funcall (papply #'list 0 1 2)) '(0 1 2)) ; usage of more-arg parameter
  (equal (funcall (papply (list)) 0 1 2) '(0 1 2)) ; usage of more-args parameter
  (equal (funcall (papply #'mylist 0 _ 2) 1) '(0 1 2))
  (equal (funcall (papply #'list 0 _ 2) 1) '(0 1 2))
  (equal (funcall (papply (mylist 0 _ 2)) 1) '(0 1 2))
  (equal (funcall (papply (list 0 _ 2)) 1) '(0 1 2))
  (equal (funcall (papply #'mylist 0 (1+ _) 2) 0) '(0 1 2))
  (equal (funcall (papply #'list 0 (1+ _) 2) 0) '(0 1 2))
  (equal (funcall (papply (mylist 0 (1+ _) 2)) 0) '(0 1 2))
  (equal (funcall (papply (list 0 (1+ _) 2)) 0) '(0 1 2))
  ; Macro use as the operator of form format.
  (equal (funcall (papply (destructuring-bind (a b) _ (+ a b))) '(1 2)) 3)
  ; Special operator use as the operator of form format.
  (equal (funcall (papply (let ((a _) (b _)) (+ a b _))) 1 2 3) 6)
  ;; The variations that the former PAPPLY accepted. It is expected to
  ;; raise an error for the current version.
  (handler-case (funcall (papply mylist))
    (cl:unbound-variable (cnd) (declare (ignore cnd)) t)
    (:no-error (value) (declare (ignore value)) nil))
  (handler-case (funcall (papply list))
    (cl:unbound-variable (cnd) (declare (ignore cnd)) t)
    (:no-error (value) (declare (ignore value)) nil))
  (handler-case (funcall (papply (#'list)))
    (cl:program-error (cnd) (declare (ignore cnd)) t)
    (:no-error (value) (declare (ignore value)) nil))
  (handler-case (funcall (papply mylist (1+ _)) 0)
    (cl:unbound-variable (cnd) (declare (ignore cnd)) t)
    (:no-error (value) (declare (ignore value)) nil))
  (handler-case (funcall (papply list (1+ _)) 0)
    (cl:unbound-variable (cnd) (declare (ignore cnd)) t)
    (:no-error (value) (declare (ignore value)) nil))
  ;; Use cases with local variables.
  (let ((list #'+) (add #'+) (x 1) (y 2))
    (and
      (null (funcall (papply #'list)))
      ; The former PAPPLY returned 0 in this case.
      (null (funcall (papply (list))))
      (zerop (funcall (papply list)))
      (zerop (funcall (papply add)))
      (equal (funcall (papply add 0 _ y) x) 3)
      (equal (funcall (papply list 0 _ y) x) 3)
      (equal (funcall (papply (list 0 _ y)) x) '(0 1 2))
      (equal (funcall (papply #'list 0 _ y) x) '(0 1 2))
      (equal (funcall (papply add 0 _ y) x) 3)
      (equal (funcall (papply list 0 (1+ _) y) 0) 3)
      (equal (funcall (papply (list 0 (1+ _) y)) 0) '(0 1 2))
      (equal (funcall (papply #'list 0 (1+ _) y) 0) '(0 1 2))
      ;; The variations that the former PAPPLY accepted. It is expected to
      ;; raise an error for the current version.
      (handler-case (funcall (papply (add)))
        (cl:undefined-function (cnd) (declare (ignore cnd)) t)
        (:no-error (value) (declare (ignore value)) nil)))))

;; PAPPLY
(and
  (equal (funcall (apapply #'mylist 0 a0 2) 1) '(0 1 2))
  (equal (funcall (apapply #'mylist 0 a7 2) 1) '(0 1 2))
  (equal (funcall (apapply #'list 0 a0 2) 1) '(0 1 2))
  (equal (funcall (apapply #'list a1 a0 a7) 1 0 2) '(0 1 2))
  (equal (funcall (apapply (mylist 0 a0 2)) 1) '(0 1 2))
  (equal (funcall (apapply (list 0 a0 2)) 1) '(0 1 2))
  (equal (funcall (apapply #'mylist 0 (1+ a0) 2) 0) '(0 1 2))
  (equal (funcall (apapply #'mylist 0 (1+ a0) (1+ a3)) 0 1) '(0 1 2))
  (equal (funcall (apapply #'list 0 (1+ a0) 2) 0) '(0 1 2))
  (equal (funcall (apapply #'list 0 (1+ a0) (1+ a3)) 0 1) '(0 1 2))
  (equal (funcall (apapply (mylist a3 (1+ a0) (+ 2 a0))) 0 0) '(0 1 2))
  (equal (funcall (apapply (list 0 (1+ a0) 2)) 0) '(0 1 2))
  ; Macro use as the operator of form format.
  (equal (funcall (apapply (destructuring-bind ((a b) (c d)) (list a0 a0)
                             (+ a b c d))) '(1 2)) 6)
  ; Special operator use as the operator of form format.
  (equal (funcall (apapply (let ((a a0) (b (1+ a1))) (+ a b a2))) 1 2 3) 7)
  ;; The variations that the former PAPPLY accepted. It is expected to
  ;; raise an error for the current version.
  (handler-case (funcall (apapply mylist))
    (cl:unbound-variable (cnd) (declare (ignore cnd)) t)
    (:no-error (value) (declare (ignore value)) nil))
  (handler-case (funcall (apapply list))
    (cl:unbound-variable (cnd) (declare (ignore cnd)) t)
    (:no-error (value) (declare (ignore value)) nil))
  (handler-case (funcall (apapply (#'list)))
    (cl:program-error (cnd) (declare (ignore cnd)) t)
    (:no-error (value) (declare (ignore value)) nil))
  (handler-case (funcall (apapply mylist (1+ _)) 0)
    (cl:unbound-variable (cnd) (declare (ignore cnd)) t)
    (:no-error (value) (declare (ignore value)) nil))
  (handler-case (funcall (apapply list (1+ _)) 0)
    (cl:unbound-variable (cnd) (declare (ignore cnd)) t)
    (:no-error (value) (declare (ignore value)) nil))
  ;; Use cases with local variables.
  (let ((list #'+) (add #'+) (x 1) (y 2))
    (and
      (null (funcall (apapply #'list)))
      ; The former PAPPLY returned 0 in this case.
      (null (funcall (apapply (list))))
      (zerop (funcall (apapply list)))
      (zerop (funcall (apapply add)))
      (equal (funcall (apapply add 0 a0 a0 a1) x y) 4)
      (equal (funcall (apapply list 0 a0 a0 a1) x y) 4)
      (equal (funcall (apapply (list 0 a1 a0)) y x) '(0 1 2))
      (equal (funcall (apapply #'list 0 a1 a0 a1) y x) '(0 1 2 1))
      (equal (funcall (apapply add 0 a0 a0 y) x) 4)
      (equal (funcall (apapply list 0 (1+ a0) y) 0) 3)
      (equal (funcall (apapply (list 0 (1+ a0) y)) 0) '(0 1 2))
      (equal (funcall (apapply #'list 0 (1+ a0) y) 0) '(0 1 2))
      ;; The variations that the former PAPPLY accepted. It is expected to
      ;; raise an error for the current version.
      (handler-case (funcall (apapply (add)))
        (cl:undefined-function (cnd) (declare (ignore cnd)) t)
        (:no-error (value) (declare (ignore value)) nil)))))

(defmacro with-oneish ((&rest templates) &body body)
  "Usage:
     (with-oneish (f g) body)
     (with-oneish ((f x) (g x y)) body)

   `f` and `g` here are variables bound to functions and these symbols
   are made be local macros in BODY. Naturally, these are still
   available as variables too. The second form is merely explanatory and
   both form finally converted into a same form. Second form can raise
   an condition if the number of arguments does not match the number of
   parameters in the second form.

    Nested form is not acceptable as elements of TEMPLATES. Multiple
   times use of one or more symbols in an element of TEMPLATES is also
   not accepted. CL:LABELS or CL:MACROLET should be used for such a
   complicated situation."
  `(macrolet ,(mapcar #'(if (symbolp a0)
                          (with-gensyms (args)
                            `(,a0 (&rest ,args) `(funcall ,',a0 ,@,args)))
                          (destructuring-bind (var &rest args) a0
                            `(,var ,args `(funcall ,',var ,,@args))))
                      templates)
     ,@body))

;; Extended sharp quote
(and
  ; Macro
  (equal (funcall #'(destructuring-bind (a b) _ (+ a b))'(1 2)) 3)
  (equal (funcall #'(destructuring-bind ((a b) (c d)) (list a0 a0)
                      (+ a b c d))'(1 2)) 6)
  ; Special operator
  (equal (funcall  #'(let ((a _) (b _)) (+ a b _))1 2 3) 6)
  (equal (funcall  #'(let ((a a0) (b (1+ a1))) (+ a b a2))1 2 3) 7)
  ;; The variations that the former PAPPLY misunderstood the user
  ;; intention: symbols that are both FBOUNDP and BOUNDP globally and,
  ;; as the global variable, not bound to a function. The former PAPPLY
  ;; understood it as a global variable.
  (equal (funcall #'(* 1 2 3 _) 4 5) 120)
  (equal (funcall #'(+ 1 2 3 _) 4 5) 15)
  (let ((list #'+) (add #'+) (x 1) (y 2))
    (and
      ; The former PAPPLY returned 0 in this case.
      (null (funcall #'(list)))
      (equal (funcall #'(list 0 _ _ _) x y x) '(0 1 2 1))
      (equal (funcall #'(list 0 a0 a1 a0) x y) '(0 1 2 1))
      (equal (funcall #'(list 0 (1+ a0) y) 0) '(0 1 2))
      ;; The variations that the former PAPPLY accepted. It is expected to
      ;; raise an error for the current version. This feature is
      ;; extracted as an independent macro, CHIKU.UTIL:WITH-ONEISH and
      ;; these actual use cases are made possible in combination with
      ;; the macro.
      (handler-case (funcall #'(add))
        (cl:undefined-function (cnd) (declare (ignore cnd)) t)
        (:no-error (value) (declare (ignore value)) nil))
      (handler-case (funcall #'(add 1 _ 10) 0)
        (cl:undefined-function (cnd) (declare (ignore cnd)) t)
        (:no-error (value) (declare (ignore value)) nil))
      (with-oneish (add)
        (zerop (funcall #'(add)))
        (equal (funcall #'(add 1 _ 10) 2) 13)))))

(tree-equal
  (macroexpand
    (read-from-string
      "#'(if (funcall test (funcall key a0) (funcall key a1)) a1 a0)"))
  '(function
     (lambda (a0 a1)
       (IF (FUNCALL TEST (FUNCALL KEY A0) (FUNCALL KEY A1))
           A1
           A0))))

#+sbcl
(sb-cltl2:macroexpand-all
  (read-from-string
    "(with-oneish (order key)
     (reduce (lambda (acc item)
               (let ((projected (key item)))
                 (if (find-if #'(order projected (key _)) acc)
                   acc)))
             seq))"))

#+sbcl
(sb-cltl2:macroexpand-all
  (read-from-string
    "(with-oneish (key)
     (reduce (lambda (acc item)
               (let ((projected (key item)))
                 (if (find-if #'(order projected (key _)) acc)
                   acc)))
             seq))"))

#+sbcl
(sb-cltl2:macroexpand-all
  (read-from-string
    "(macrolet ((order (x y) `(funcall order ,x ,y))
               (key (x) `(funcall key ,x)))
       (reduce (lambda (acc item)
                 (let ((projected (key item)))
                   (if (find-if #'(order projected (key _)) acc)
                     acc)))
               seq))"))

;;; Test the local macro awareness.
(equal
  (let ((seq '((1 . a) (2 . b) (6 . f)))
        (order #'<=) (key #'car))
    (macrolet ((order (x y) `(funcall order ,x ,y))
               (key (x) `(funcall key ,x)))
      (remove-if (lambda (element)
                   (let ((projected (key element)))
                     (find-if #'(and (not (eq element a0))
                                     (order projected (key a0)))
                              seq)))
                 seq)))
  '((6 . f)))
