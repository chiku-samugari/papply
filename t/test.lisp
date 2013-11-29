
(in-package :papply-test)
(in-suite :papply)


(defmacro test-many-forms (criteria expected kind op rest1 &rest rest2)
  `(progn
     (test-form-form ,criteria ,expected ,kind ,op ,rest1 ,@rest2)
     (test-listup-form ,criteria ,expected ,kind ,op ,rest1 ,@rest2)
     (test-form-fn-form ,criteria ,expected ,kind ,op ,rest1 ,@rest2)
     (test-listup-fn-form ,criteria ,expected ,kind ,op ,rest1 ,@rest2)))

(defmacro test-form-form (criteria expected kind op rest1 &rest rest2)
  `(is (,criteria ,expected (funcall (,kind (,op ,@rest1)) ,@rest2))))
(defmacro test-form-fn-form (criteria expected kind op rest1 &rest rest2)
  ;; checks the lexical function binding
  `(is (,criteria ,expected (funcall (,kind ((function ,op) ,@rest1)) ,@rest2))))

(defmacro test-listup-form (criteria expected kind op rest1 &rest rest2)
  `(is (,criteria ,expected (funcall (,kind ,op ,@rest1) ,@rest2))))
(defmacro test-listup-fn-form (criteria expected kind op rest1 &rest rest2)
  ;; checks the lexical function binding
  `(is (,criteria ,expected (funcall (,kind (function ,op) ,@rest1) ,@rest2))))

(test :basic
  (test-many-forms equalp '(5 5)
                   papply list (_ 5)
                   5)
  (test-many-forms equalp '((a b c) (d (e f) g h i) j)
                   papply list ((list _ _ _) (list _ (list _ _) _ _ _) _)
                   'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)
  (test-many-forms equalp '(4 10)
                   papply list ((1+ _) (parse-integer _))
                   3 "10")
  (test-many-forms equalp '(5 "THING")
                   apapply list ((length (string a0)) (symbol-name a0))
                   'thing)
  (let ((i 0))
    (test-many-forms equalp (list 5 (1+ i) "THING" (* (1+ i) 3))
                     apapply list ((length (string a0)) a1 (symbol-name a0) (* a1 3))
                     'thing (incf i))
    ;; checks if (incf i) is evaluated exactly 4 times.
    (is (= i 4))))


(defmacro test-fn-forms (criteria expected kind op rest1 &rest rest2)
  `(progn
     (test-form-fn-form ,criteria ,expected ,kind ,op ,rest1 ,@rest2)
     (test-listup-fn-form ,criteria ,expected ,kind ,op ,rest1 ,@rest2)))


(defmacro test-dynamic-forms (criteria expected kind op rest1 &rest rest2)
  `(progn
     (test-form-form ,criteria ,expected ,kind ,op ,rest1 ,@rest2)
     (test-listup-form ,criteria ,expected ,kind ,op ,rest1 ,@rest2)))

(defvar *list*)

(test :variable
  #+sbcl
  (let ((list #'vector))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; (papply list ...) .
    ;; the variable `list' is bound to #'vector,
    ;; so each result should be a vector.

    (test-dynamic-forms equalp #(5 5)
                        papply list (_ 5)
                        5)
    (test-dynamic-forms equalp #((a b c) (d (e f) g h i) j)
                        papply list ((list _ _ _) (list _ (list _ _) _ _ _) _)
                        'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)
    (test-dynamic-forms equalp #(4 10)
                        papply list ((1+ _) (parse-integer _))
                        3 "10")
    (test-dynamic-forms equalp #(5 "THING")
                        apapply list ((length (string a0)) (symbol-name a0))
                        'thing)
    (let ((i 0))
      (test-dynamic-forms equalp (vector 5 (1+ i) "THING" (* (1+ i) 3))
                          apapply list ((length (string a0)) a1 (symbol-name a0) (* a1 3))
                          'thing (incf i))
      (is (= i 2)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; (papply (function list) ...) === (papply #'list ...)
    ;; or (papply ((function list) ...)) === (papply (#'list ...))
    ;; each result should be a list.

    (test-fn-forms equalp '(5 5)
                   papply list (_ 5)
                   5)
    (test-fn-forms equalp '((a b c) (d (e f) g h i) j)
                   papply list ((list _ _ _) (list _ (list _ _) _ _ _) _)
                   'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)
    (test-fn-forms equalp '(4 10)
                   papply list ((1+ _) (parse-integer _))
                   3 "10")
    (test-fn-forms equalp '(5 "THING")
                   apapply list ((length (string a0)) (symbol-name a0))
                   'thing)
    (let ((i 0))
      (test-fn-forms equalp (list 5 (1+ i) "THING" (* (1+ i) 3))
                     apapply list ((length (string a0)) a1 (symbol-name a0) (* a1 3))
                     'thing (incf i))
      (is (= i 2))))

  (let ((*list* #'vector))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; (papply list ...) .
    ;; the variable `list' is bound to #'vector,
    ;; so each result should be a vector.

    (test-dynamic-forms equalp #(5 5)
                        papply *list* (_ 5)
                        5)
    (test-dynamic-forms equalp #((a b c) (d (e f) g h i) j)
                        papply *list* ((list _ _ _) (list _ (list _ _) _ _ _) _)
                        'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)
    (test-dynamic-forms equalp #(4 10)
                        papply *list* ((1+ _) (parse-integer _))
                        3 "10")
    (test-dynamic-forms equalp #(5 "THING")
                        apapply *list* ((length (string a0)) (symbol-name a0))
                        'thing)
    (let ((i 0))
      (test-dynamic-forms equalp (vector 5 (1+ i) "THING" (* (1+ i) 3))
                          apapply *list* ((length (string a0)) a1 (symbol-name a0) (* a1 3))
                          'thing (incf i))
      (is (= i 2))))

  (setf *list* #'vector)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (papply list ...) .
  ;; the variable `list' is bound to #'vector,
  ;; so each result should be a vector.

  (test-dynamic-forms equalp #(5 5)
                      papply *list* (_ 5)
                      5)
  (test-dynamic-forms equalp #((a b c) (d (e f) g h i) j)
                      papply *list* ((list _ _ _) (list _ (list _ _) _ _ _) _)
                      'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)
  (test-dynamic-forms equalp #(4 10)
                      papply *list* ((1+ _) (parse-integer _))
                      3 "10")
  (test-dynamic-forms equalp #(5 "THING")
                      apapply *list* ((length (string a0)) (symbol-name a0))
                      'thing)
  (let ((i 0))
    (test-dynamic-forms equalp (vector 5 (1+ i) "THING" (* (1+ i) 3))
                        apapply *list* ((length (string a0)) a1 (symbol-name a0) (* a1 3))
                        'thing (incf i))
    (is (= i 2)))
  (setf *list* nil))

(test :fbinding
  (flet ((my-list (&rest args) (apply #'vector args)))
    (test-many-forms equalp (vector 5 5)
                     papply my-list (_ 5)
                     5)
    (test-many-forms equalp #((a b c) (d (e f) g h i) j)
                     papply my-list ((list _ _ _) (list _ (list _ _) _ _ _) _)
                     'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)
    (test-many-forms equalp #(4 10)
                     papply my-list ((1+ _) (parse-integer _))
                     3 "10")
    (test-many-forms equalp #(5 "THING")
                     apapply my-list ((length (string a0)) (symbol-name a0))
                     'thing)
    (let ((i 0))
      (test-many-forms equalp (vector 5 (1+ i) "THING" (* (1+ i) 3))
                       apapply my-list ((length (string a0)) a1 (symbol-name a0) (* a1 3))
                       'thing (incf i))
      (is (= i 4))))

  ;; ensure my-list is not yet defined outside the scope
  (signals UNDEFINED-FUNCTION
    (funcall (papply (my-list _ _)) 5 6)))

