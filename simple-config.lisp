;;;; simple-config.lisp

(in-package #:simple-config)

(defvar *config* nil
  "our loaded config")

(defvar *parse-lists*)
(defvar *list-separator*)

(defun config (key &optional default)
  "retrieve the value from config using KEY, returns nil if not found

if DEFAULT is given returns it instead of nil when KEY is not found"
  (or (cdr (assoc key *config* :test #'equal))
      default))

(defun load-config (file-path &key (parse-lists t) (list-separator #\,))
  "loads config at FILE-PATH

PARSE-LISTS defaults to t
LIST-SEPARATOR defaults to #\,

if PARSE-LISTS is non-nil we check any value for character LIST-SEPARATOR and split it by that

returns T on success, NIL otherwise"
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
      
      (setf *config*
	    (loop
	       for line in file-contents
	       with input
		 
	       ;; allows for inline comments
	       do (setf line (subseq line 0 (or (search "#" line :test #'string=)
						(length line))))

	       do (setf input (mapcar #'trim (split #\= line)))
		 
	       collect (cons
			(string-to-keyword (car input))
			(parse-value (trim (cadr input))))))))
  (when *config*
    t))

(defmethod (setf config) (value key)
  "allows us to do setf on (config) calls"
  (setf *config* (remove key *config* :key #'car))
  (push (cons key value) *config*))

(defun save-config (filepath)
  "saves currently loaded config to FILEPATH"
  (with-open-file (out filepath :direction :output
				:if-exists :overwrite)
    (format out "~{~/conf::print-config/~%~}" *config*)))

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

(defun line-comment-p (line)
  "checks if LINE is a full line comments"
  (starts-with-p "#" line))
