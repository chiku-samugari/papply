;;; PAPPLY
;;; chiku-util-local.lisp
;;; Some utilities I daily use. Mainly imported from chiku-util project.
;;;
;;; Author: chiku (Takehiko Nawata samugari.penguin@gmail.com)
;;; License: MIT License
(in-package :chiku-util-local)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun drop (seq &optional (n 1)) (subseq seq n))

  (defun take (seq &optional (n 1)) (subseq seq 0 n))

  (defun flatten (tree)
    (labels ((rec (tree acc)
               (cond ((null tree) acc)
                     ((atom tree) (cons tree acc))
                     (t (rec (car tree) (rec (cdr tree) acc))))))
      (rec tree nil)))
  
  (defun maptree (func tree &key (pred #'atom))
    (if (funcall pred tree) (funcall func tree)
      (mapcar #'(lambda (x) (maptree func x :pred pred)) tree))))

(defmacro with-tree-leaves (tree test-form result-form)
  `(maptree (lambda (leaf) (if ,test-form ,result-form leaf)) ,tree))
