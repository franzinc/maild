(in-package :user)

(defstruct aliases-info
  mtime
  aliases)

(defparameter *aliases* nil)

(defstruct alias-rhs
  type ;; :prog, :file, :error, :include.  nil means normal
  addr ;; parsed
  file ;; for :file, :prog, and :include aliases
  prog-user ;; for :prog aliases
  errmsg ;; for :error aliases
  expanded-from ;; string
  escaped) ;; prefixed by a backslash?

(defstruct alias-exp
  rhs
  owner)
  

(defun update-aliases-info ()
  (if (null *aliases*)
      (setf *aliases* (make-aliases-info :mtime 0)))
  (let ((mtime (file-write-date *aliasesfile*)))
    (if* (> mtime (aliases-info-mtime *aliases*))
       then
	    (verify-root-only-file *aliasesfile*)
	    (if *debug* (maild-log "Reparsing aliases"))
	    (setf (aliases-info-aliases *aliases*)
	      (parse-aliases-file))
	    (setf (aliases-info-mtime *aliases*) mtime))))

(defun parse-aliases-file ()
  (let ((ht (make-hash-table :test #'equalp)))
    (with-open-file (f *aliasesfile*)
      (let (ali)
	(while (setf ali (aliases-get-alias f))
	  (multiple-value-bind (alias expansion)
	      (parse-alias-line ali)
	    (if (gethash alias ht)
		(error "Redefined alias: ~A" alias))
	    (setf (gethash alias ht) expansion)))))
    ht))

  
(defun parse-alias-line (line)
  (let ((len (length line)))
    (multiple-value-bind (lhs pos)
	(aliases-get-next-word #\: line 0 len :delim-required t)
      (if (null lhs)
	  (error "Invalid aliases line: ~A" line))
      (values lhs (parse-alias-right-hand-side lhs line pos len)))))

(defmacro include-alias-p (string)
  `(prefix-of-p ":include:" ,string))

(defmacro error-alias-p (string)
  `(prefix-of-p ":error:" ,string))

(defun file-alias-p (string)
  (and (> (length string) 0)
       (char= (schar string 0) #\/)))

(defun program-alias-p (string)
  (and (> (length string) 0)
       (char= (schar string 0) #\|)))

(defun escaped-alias-p (string)
  (and (> (length string) 0)
       (char= (schar string 0) #\\)))

(defun parse-alias-right-hand-side (lhs line pos len)
  (let (expansion ali)
    (loop
      (multiple-value-bind (word newpos)
	  (aliases-get-next-word #\, line pos len)
	(if (null word)
	    (return))
	(setf ali (make-alias-rhs :expanded-from lhs))

	;; See what we're dealing with:
	(cond
	 ((include-alias-p word)
	  (setf (alias-rhs-type ali) :include)
	  (setf (alias-rhs-file ali) (subseq word #.(length ":include:"))))
	 ((error-alias-p word)
	  (setf (alias-rhs-type ali) :error)
	  (setf (alias-rhs-errmsg ali) (subseq word #.(length ":error:"))))
	 ((file-alias-p word)
	  (setf (alias-rhs-type ali) :file)
	  (setf (alias-rhs-file ali) word))
	 ((program-alias-p word)
	  (setf (alias-rhs-type ali) :prog)
	  (multiple-value-bind (found whole prog-user prog)
	      (match-regexp "^|(\\([^)]+\\))\\(.*\\)" word)
	    (declare (ignore whole))
	    (if* found
	       then
		    (setf (alias-rhs-file ali) prog)
		    (setf (alias-rhs-prog-user ali) prog-user)
	       else
		    (setf (alias-rhs-file ali) (subseq word 1)))))
	 (t 
	  ;; Wasn't one of the special types.  It must be a regular
	  ;; (possibly escaped) email address.  Parse it and complain
	  ;; if it isn't proper.
	  (when (escaped-alias-p word)
	    (setf (alias-rhs-escaped ali) t)
	    (setf word (subseq word 1)))
	  (let ((parsed (parse-email-addr word)))
	    (if (null parsed)
		(error "Invalid email address ~S in aliases file" word))
	    (setf (alias-rhs-addr ali) parsed))))
	(push ali expansion)
	(setf pos newpos)))
    (if (null expansion)
	(error "Alias ~A has blank expansion" lhs))
    expansion))

  
(defun aliases-get-next-word (delim line pos len &key delim-required)
  (block nil
    (multiple-value-bind (word newpos)
	(aliases-get-next-word-help delim line pos len)
      (if (= pos newpos)
	  (return nil))
      (if (and delim-required (char/= (schar line (1- newpos)) delim))
	  nil
	(values word newpos)))))
    

;; Skips unquoted spaces.
(defun aliases-get-next-word-help (delim line pos len)
  (let ((newword (make-string (- len pos)))
	(outpos 0)
	inquote
	char)
    (loop
      (when (>= pos len)
	(if inquote
	    (error "Unterminated double-quote in: ~A" line))
	(return (values (subseq newword 0 outpos) pos)))
      (setf char (schar line pos))
      (cond 
       ((and (not inquote) (char= char delim))
	(return (values (subseq newword 0 outpos) (1+ pos))))
       ((char= char #\")
	(setf inquote (not inquote)))
       ((and inquote (char= char #\\))
	(incf pos)
	(if (>= pos len)
	    (error "Unterminated double-quote in: ~A" line))
	(setf char (schar line pos))
	(setf (schar newword outpos) char)
	(incf outpos))
       ((and (not inquote) (whitespace-p char))
	;; skip unquoted whitespace
	)
       (t
	(setf (schar newword outpos) char)
	(incf outpos)))
      (incf pos))))

;; Collects a full entry (first line plus any continuation lines)
(defun aliases-get-alias (f)
  (block nil
    (let ((line (aliases-get-good-line f))
	  nextline
	  lastchar
	  nextchar)
      (if (null line)
	  (return nil))
      (loop
	(setf lastchar (schar line (1- (length line))))
	(setf nextchar (peek-char nil f nil nil))
	;; See if we're done.
	(if (and (char/= lastchar #\\)
		 (not (member nextchar '(#\space #\tab))))
	  (return line))
	;; Strip trailing backslash if there is one.
	(if (char= lastchar #\\)
	    (setf line (subseq line 0 (1- (length line)))))
	(setf nextline (aliases-get-good-line f))
	(if (null nextline)
	    (error "Continuation line isn't there"))
	(setf line (concatenate 'string line nextline))))))

;; A good line is one that is non-blank and doesn't
;; start w/ the comment (#) character.
(defun aliases-get-good-line (f)
  (let (line)
    (loop
      (setf line (read-line f nil nil))
      (if (null line)
	  (return nil))
      (if (and (/= 0 (length line))
	       (char/= (schar line 0) #\#))
	  (return line)))))

;; :include: files should be treated as a big multi-line left hand side.
;; This function returns a bigass string.
(defun aliases-get-include-file (filename)
  (verify-security filename)
  (with-open-file (f filename)
    (let (lines line)
      (while (setf line (aliases-get-good-line f))
	;; Remove any comma and whitespace from end of string.
	(setf line (replace-regexp line "^\\(.*\\),\\b*$" "\\1"))
	(push line lines))
      (if lines 
	  (list-to-delimited-string lines #\,)
	nil))))

(defun aliases-parse-include-file (filename)
  (let ((line (aliases-get-include-file filename)))
    (parse-alias-right-hand-side filename line 0 (length line))))

;;; end parsing stuff... 

;;; begin expansion stuff...

;; returns a list of alias-exp structs.
;; may include duplicates.
(defun expand-alias (alias-orig)
  ;;; XXX - may want to move this out for performance reasons
  (update-aliases-info)
  (let ((alias (if (stringp alias-orig)
		   (parse-email-addr alias-orig)
		 alias-orig)))
    (if (null alias)
	(error "expand-alias: Invalid address: ~S" alias-orig))
    (expand-alias-inner alias (aliases-info-aliases *aliases*) nil nil)))

;; tries long match (w/ full domain) first.. 
;; then short match (just user part)
(defun expand-alias-inner (alias ht seen owner)
  (block nil
    (if (not (local-domain-p alias))
	(return nil))
    (if (member alias seen :test #'equalp)
	(error "Alias loop involving alias ~S" alias))
    (push alias seen)
    (let ((members (gethash (emailaddr-orig alias) ht)))
      (if members
	  (alias-expand-member-list alias members ht seen owner)
	;; try short address instead
	(let ((members (gethash (emailaddr-user alias) ht)))
	  (if members
	      (alias-expand-member-list alias members ht seen owner)))))))

;; 'members' is a list of alias-rhs structs.
(defun alias-expand-member-list (lhs members ht seen owner &key in-include)
  (let (res type ownerstring ownerparsed)
    (setf ownerstring (concatenate 'string "owner-" (emailaddr-orig lhs)))
    (setf ownerparsed (parse-email-addr ownerstring))
    (if (expand-alias ownerparsed)
	(setf owner ownerparsed))

    (dolist (member members)
      (setf type (alias-rhs-type member))
      (cond
       ;; sanity checks first
       ((and in-include (eq type :prog))
	(error "While processing :include: file ~A: program aliases are not allowed in :include: files"
	       in-include))
       ((and in-include (eq type :file))
	(error "While processing :include: file ~A: file aliases are not allowed in :include: files"
	       in-include))
       ((eq type :include)
	(when in-include
	  (error "While processing :include: file ~A: :include: is not allowed within an :include: file"
		 in-include))
	(let* ((includefile (alias-rhs-file member))
	       (newmembers (aliases-parse-include-file includefile)))
	  ;; recurse
	  (setf res 
	    (append 
	     (alias-expand-member-list lhs newmembers ht seen owner
				       :in-include includefile)
	     res))))
       ;; definite terminals
       ((or (member type '(:prog :file :error))
	    (alias-rhs-escaped member)
	    (not (local-domain-p (alias-rhs-addr member)))
	    (equalp (emailaddr-user (alias-rhs-addr member))
		    (emailaddr-user lhs)))
	(push (make-alias-exp :rhs member :owner owner) res))
       (t
	(let ((exp (expand-alias-inner (alias-rhs-addr member) ht seen owner)))
	  (if (null exp)
	      (push (make-alias-exp :rhs member :owner owner) res)
	    (setf res (append exp res)))))))
    res))

