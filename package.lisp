;;;; package.lisp

(defpackage #:simple-config
  (:use #:cl)
  (:nicknames #:conf)
  (:import-from #:uiop
		:with-safe-io-syntax
		:read-file-lines)
  (:import-from #:str
		:trim
		:digitp
		:split
		:blankp
		:replace-all
		:containsp
		:starts-with-p)
  (:export :config
	   :load-config))
