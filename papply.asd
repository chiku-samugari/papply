;;; PAPPLY
;;; papply.asd
;;; system definition for PAPPLY
;;;
;;; Author: chiku (Takehiko Nawata, samugari.penguin@gmail.com)
;;; License: MIT License
(defsystem papply
  :name "PAPPLY"
  :version "0.8.0"
  :maintainer "Takehiko Nawata"
  :author "Takehiko Nawata"
  :license "MIT License"
  :description "PAPPLY"
  :long-description "Macro family for partial application of fucntions."
  :depends-on (:azuki)
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "papply")))
