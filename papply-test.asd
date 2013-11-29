(defsystem papply-test
  :version "0.8.0"
  :maintainer "Takehiko Nawata"
  :author "Takehiko Nawata"
  :license "MIT License"
  :description "PAPPLY"
  :long-description "Macro family for partial application of fucntions."
  :serial t
  :depends-on (:papply :fiveam)
  :components ((:module :t
                :components ((:file "packages")
                             (:file "test")))))