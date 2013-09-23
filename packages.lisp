;;; PAPPLY
;;; package.lisp
;;; package definitions
;;;
;;; Author: chiku (Takehiko Nawata, samugari.penguin@gmail.com)
;;; License: MIT License
(in-package :cl-user)

(defpackage :chiku-util-local
  (:use :cl)
  (:export :with-gensyms :drop :take :flatten :with-tree-leaves :leaf))

(defpackage :papply
  (:use :cl :chiku-util-local)
  (:export :papply :_ :apapply :p :extend-sharp-quote))
