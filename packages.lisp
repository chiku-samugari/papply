;;; PAPPLY
;;; package.lisp
;;; package definitions
;;;
;;; Author: chiku (Takehiko Nawata, samugari.penguin@gmail.com)
;;; License: MIT License
(in-package :cl-user)

(defpackage :papply.util
  (:use :cl)
  (:export :with-gensyms :drop :take :flatten :with-tree-leaves :leaf :in
           :inq :filter))

(defpackage :papply
  (:use :cl :papply.util)
  (:export :papply :_ :apapply :p :extend-sharp-quote))
