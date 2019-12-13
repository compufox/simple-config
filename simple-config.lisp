;;;; simple-config.lisp

(in-package #:simple-config)

(defvar *config* nil
  "our loaded config")

(defvar *parse-lists* t)
(defvar *list-separator* #\,)

(defun config (key &optional default)
  "retrieve the value from config using KEY, returns nil if not found

if DEFAULT is given returns it instead of nil when KEY is not found"
  (or (cdr (assoc key *config* :test #'equal))
      default))

(defun load-config (file-path &key (parse-lists *parse-lists*) (list-separator *list-separator*))
  "loads config at FILE-PATH

PARSE-LISTS defaults to t
LIST-SEPARATOR defaults to #\,

if PARSE-LISTS is non-nil we check any value for character LIST-SEPARATOR and split it by that

returns T on success, NIL otherwise"
  (if parse-lists
      (unless list-separator
	(error "list-separator must be provided if parse-lists is non-nil")))
  (let ((*parse-lists* parse-lists)
	(*list-separator* list-separator))
    (with-open-file (conf file-path)
      (setf *config*
	    (loop
	       for line = (read-line conf nil)
	       with input
		 
	       while line
		 
	       ;; allows for inline comments
	       unless (or (starts-with-p "#" line) (blankp line))
	       do (setf line (subseq line 0 (or (search "#" line :test #'string=)
						(length line))))

	       do (setf input (mapcar #'trim (split #\= line)))
		 
	       unless (or (starts-with-p "#" line) (blankp line))
	       collect (cons
			(intern (replace-all "_" "-"
					     (string-upcase (car input)))
				:keyword)
			(parse-value (trim (cadr input))))))))
  (when *config*
    t))
  
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
    ((or (string= "false" value)
	 (blankp value))
     nil)

    ;; if we're here then we just return the straight value
    (t value)))
