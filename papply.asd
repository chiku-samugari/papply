;;; PAPPLY
;;; papply.asd
;;; system definition for PAPPLY
;;;
;;; Author: chiku (Takehiko Nawata, samugari.penguin@gmail.com)
;;; License: MIT License
(defpackage :papply.asd
  (:use :cl :asdf))

(in-package :papply.asd)

(defsystem papply
  :name "PAPPLY"
  :version "0.8.0"
  :maintainer "Takehiko Nawata"
  :author "Takehiko Nawata"
  :license "MIT License"
  :description "PAPPLY"
  :long-description "Macro family for partial application of fucntions."
  :serial t
  :components ((:file "packages")
               (:file "chiku-util-local")
               (:file "papply"))
  :in-order-to ((test-op (load-op papply-test))))
