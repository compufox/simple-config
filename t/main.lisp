(in-package :simple-config-test)

(plan 9)

(defvar *config*
  (load-config (merge-pathnames "t/example.config"
                                (asdf:system-source-directory :simple-config-test))))

(defvar *space-sep*
  (load-config (merge-pathnames "t/example.config"
                                (asdf:system-source-directory :simple-config-test))
               :list-separator #\Space))

(is (config *config* :number) 4)
(is (config *config* :list) '("a" "b" "c" "d"))
(is (config *config* :number-list) '(1 2 3 4))
(is (config *config* :mixed-list) '(1 "b" 3 "d"))
(is (config *config* :false-value) nil)
(is (config *config* :also-false) nil)
(is (config *config* :test-key) "thisIsASecretKey")
(is (config *config* :keyword) :test)



(is (config *space-sep* :prime-list) '(1 2 3 5))

(finalize)
