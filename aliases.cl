(in-package :user)

(defstruct aliases-info
  mtime
  aliases)

(defparameter *aliases* nil)


(defun update-aliases-info ()
  (if (null *aliases*)
      (setf *aliases* (make-aliases-info :mtime 0)))
  (let ((mtime (file-write-date *aliasesfile*)))
    (if* (> mtime (aliases-info-mtime *aliases*))
       then
	    (verify-security *aliasesfile*)
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
		(error "Multiply defined alias: ~A" alias))
	    (setf (gethash alias ht) expansion)))))
    ht))
	  

(defun parse-alias-line (line)
  (let ((len (length line)))
    (multiple-value-bind (localpart pos)
	(aliases-get-next-word #\: line 0 len :delim-required t)
      (if (null localpart)
	  (error "Invalid aliases line: ~A" line))
      (values localpart
	      (parse-alias-left-hand-side line pos len)))))

(defun parse-alias-left-hand-side (line pos len)
  (let (expansion)
    (loop
      (multiple-value-bind (word newpos)
	  (aliases-get-next-word #\, line pos len)
	(if (null word)
	    (return))
	(push word expansion)
	(setf pos newpos)))
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
    (parse-alias-left-hand-side line 0 (length line))))

(defun expand-alias (alias &key parsed)
  ;;; XXX - may want to move this out for performance reasons
  (update-aliases-info)
  (if (emailaddr-p alias)
      (setf alias (emailaddr-orig alias)))
  (let ((res (expand-alias-help alias (aliases-info-aliases *aliases*) nil)))
    (if parsed
	(mapcar #'parse-special-alias res)
      res)))

(defun parse-special-alias (a)
  (let (prefix)
    (if (> (length a) 0)
	(setf prefix (schar a 0)))
    (if (member prefix '(#\/ #\| #\:) :test #'eq)
	(make-emailaddr :user a :orig a)
      (parse-email-addr a))))

(defmacro include-alias-p (string)
  `(prefix-of-p ":include:" ,string))

(defun file-alias-p (string)
  (and (> (length string) 0)
       (char= (schar string 0) #\/)))

(defun program-alias-p (string)
  (and (> (length string) 0)
       (char= (schar string 0) #\|)))
  

(defun expand-alias-help (alias ht seen)
  (block nil
    (if (member alias seen :test #'equalp)
	(error "Alias loop involving alias ~S" alias))
    (push alias seen)
    (let ((members (gethash alias ht)))
      (if (null members)
	  (return nil))
      (alias-expand-member-list members ht seen))))

(defun alias-expand-member-list (members ht seen &key in-include)
  (let (res member exp)
    (while members
      (setf member (pop members))
      (cond
       ((and in-include (program-alias-p member))
	(error "While processing :include: file ~A:~% ~A: program aliases are not allowed in :include: files"
	       in-include
	       member))
       ((and in-include (file-alias-p member))
	(error "While processing :include: file ~A:~% ~A: file aliases are not allowed in :include: files"
	       in-include
	       member))
       ((include-alias-p member)
	(when in-include
	  (error "While processing :include: file ~A:~% ~A: :include: is not allowed within an :include: file"
		 in-include
		 member))
	(let* ((includefile (subseq member (length ":include:"))) 
	       (newmembers (aliases-parse-include-file includefile)))
	  ;; recurse
	  (setf res 
	    (append 
	     (alias-expand-member-list newmembers ht seen 
				       :in-include includefile)
	     res))))
       ;; check for escaped members
       ((and (> (length member) 0) (char= (schar member 0) #\\))
	(push (subseq member 1) res))
       (t
	(setf exp (expand-alias-help member ht seen))
	(if (null exp)
	    (push member res)
	  (setf res (append exp res))))))
    res))

