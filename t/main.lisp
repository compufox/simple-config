(in-package :simple-config-test)

(plan 9)

(load-config (merge-pathnames "t/example.config"
			      (asdf:system-source-directory :simple-config-test)))

(is (config :number) 4)
(is (config :list) '("a" "b" "c" "d"))
(is (config :number-list) '(1 2 3 4))
(is (config :mixed-list) '(1 "b" 3 "d"))
(is (config :false-value) nil)
(is (config :also-false) nil)
(is (config :test-key) "thisIsASecretKey")
(is (config :keyword) :test)

(load-config (merge-pathnames "t/example.config"
			      (asdf:system-source-directory :simple-config-test))
	     :list-separator #\Space)

(is (config :prime-list) '(1 2 3 5))

(finalize)
