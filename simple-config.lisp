;;;; simple-config.lisp

(in-package #:simple-config)

(defvar *parse-lists*)
(defvar *list-separator*)

(defun config (config key &optional default)
  "retrieve the value from CONFIG using KEY, returns nil if not found

if DEFAULT is given returns it instead of nil when KEY is not found"
  (or (cdr (assoc key config :test #'equal))
      default))

(defun load-config (file-path &key (parse-lists t) (list-separator #\,))
  "loads config at FILE-PATH

PARSE-LISTS defaults to t
LIST-SEPARATOR defaults to #\,

if PARSE-LISTS is non-nil we check any value for character LIST-SEPARATOR and split it by that

returns the loaded config"
  (if parse-lists
      (unless list-separator
	(error "list-separator must be provided if parse-lists is non-nil")))
  
  (with-safe-io-syntax ()

    ;; shadow our defaults 
    (let ((*parse-lists* parse-lists)
	  (*list-separator* list-separator)

	  ;; go ahead and remove blank lines and commented out lines
	  (file-contents (remove-if #'line-comment-p
				    (remove-if #'blankp (read-file-lines file-path)))))

      (loop
        for line in file-contents
        for input = (mapcar #'trim (split #\= (strip-inline-comment line)))
		     
        collect (cons
                 (string-to-keyword (car input))
                 (parse-value (trim (cadr input))))))))

;; tell lisp how to setf a "config"
(defsetf config set-config-value)

(defmacro set-config-value (config key value)
  "cheeky macro to allow for setting configs in-place"
  `(setf ,config
         (append (remove ,key ,config :key #'car)
                 (list (cons ,key ,value)))))

(defun save-config (config file-path)
  "saves CONFIG to FILEPATH"
  (with-open-file (out filepath :direction :output
				:if-exists :overwrite)
    (format out "~{~/conf::print-config/~%~}" config)))

(defun print-config (stream data &optional colonp atsignp)
  "custom format function that prints out config"
  (declare (ignore colonp atsignp))
  (destructuring-bind (key . val) data
    (format stream 
	    (typecase val
	      (list "~a = ~{~a~^, ~}")
	      (t "~a = ~a"))
	    (string-downcase (symbol-name key))
	    val)))

(defun string-to-keyword (str)
  "converts STR into a keyword

replaces all underscores with hyphens"
  (intern (replace-all "_" "-" (string-upcase str))
	  :keyword))

(defun parse-value (value)
  "determines what kind of data VALUE is, and parses it correctly"
  (cond
    ;; parse digit
    ((digitp value) (parse-integer value))

    ;; if we are going to parse lists and our value has that separator
    ;;  then we split it up, and trim the results, and parse them
    ((and *parse-lists* (containsp (string *list-separator*) value))
     (mapcar #'parse-value (mapcar #'trim (split *list-separator* value))))

    ;; if false or blankp, we return nil
    ((or (string= "false" (string-downcase value))
	 (blankp value))
     nil)

    ;; if value is like :value then we interpret that as a keyword
    ((starts-with-p ":" value)
     (string-to-keyword (subseq value 1 (length value))))

    ;; if we're here then we just return the straight value
    (t value)))

(defun strip-inline-comment (line)
  "finds any inline comments in LINE and removes them"

  ;; WONTFIX
  ;; this doesn't allow for having # in a value at all?
  ;;  answer would be to check if its escaped? maybe?
  ;; solution is actually to probably change comment character
  ;;  but also? i rather like '#' being the comment character so its
  ;;  gonna stay until i need to change it lol
  (subseq line 0 (or (search "#" line :test #'string=)
                     (length line))))

(defun line-comment-p (line)
  "checks if LINE is a full line comments"
  (starts-with-p "#" line))
