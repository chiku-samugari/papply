(asdf:defsystem papply-test
  :version "0.8.0"
  :serial t
  :depends-on (:papply :fiveam)
  :components ((:module :t
                :components ((:file "package")
                             (:file "test"))))
  :perform (load-op :after (op c)
                    (eval (read-from-string "(fiveam:run! :papply)"))
                    (asdf:clear-system c)))
