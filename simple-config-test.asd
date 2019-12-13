;;;; simple-config-test.asd

(asdf:defsystem #:simple-config-test
  :description "tests simple-config"
  :author "ava fox <dev@computerfox.xyz>"
  :serial t
  :depends-on (#:simple-config #:prove)
  :components ((:module "t"
	        :components
	        ((:file "package")
		 (:file "main")))))
