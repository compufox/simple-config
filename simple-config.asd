;;;; simple-config.asd

(asdf:defsystem #:simple-config
  :description "loads and parses a KEY=VALUE style config file"
  :author "ava fox <dev@computerfox.xyz>"
  :license  "BSD 3-Clause"
  :version "1.2"
  :serial t
  :depends-on (#:str #:uiop)
  :components ((:file "package")
               (:file "simple-config"))
  :in-order-to ((test-op (test-op "simple-config-test"))))
