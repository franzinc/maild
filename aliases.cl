;; copyright (C) 2003 Franz Inc, Oakland, CA  - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: aliases.cl,v 1.18 2005/12/20 00:39:40 dancy Exp $

(in-package :user)

(defstruct aliases-info
  files ;; list of (filename mtime) pairs
  aliases
  (lock (mp:make-process-lock))) ;; so only one thread will reparse

(defparameter *aliases* nil)

(defun update-aliases-info ()
  ;; Make sure only one thread creates the aliases-info
  (without-interrupts 
    (if (null *aliases*) 
	(setf *aliases* 
	  (make-aliases-info :files (list (list *aliases-file* 0))))))

  ;; Make sure only one thread reparses the aliases.
  (mp:with-process-lock ((aliases-info-lock *aliases*) 
			 :whostate "Waiting for aliases lock")
    (when (aliases-need-reparsing-p)
      (maild-log "Reparsing aliases")
      (parse-global-aliases))))


;; Call with the lock held.
(defun aliases-need-reparsing-p ()
  (dolist (pair (aliases-info-files *aliases*))
    (let ((filename (first pair))
	  (mtime (second pair)))
      (if (or (not (probe-file filename))
	      (> (file-write-date filename) mtime))
	  (return t)))))

(defun parse-global-aliases ()
  (let ((ht (make-hash-table :test #'equalp)))
    (setf (aliases-info-files *aliases*) nil)
    (parse-aliases-file *aliases-file* ht nil)
    (setf (aliases-info-aliases *aliases*) ht)))
    

(defun parse-aliases-file (filename ht files-seen &key (system t) domain)
  (when (not (probe-file filename))
    (maild-log "Skipping nonexistent aliases file ~A" filename)
    (return-from parse-aliases-file))
  (if system
      (verify-root-only-file filename))
  (push filename files-seen)
  (let ((files-entry (list filename 0)))
    (push files-entry (aliases-info-files *aliases*))
    (with-open-file (f filename)
      (let (ali)
	(while (setf ali (aliases-get-alias f))
	  (multiple-value-bind (alias expansion)
	      (parse-alias-line ali)
	    (multiple-value-bind (is-include include-sys include-domain)
		(aliases-include-directive-p alias)
	      (cond
	       ((and is-include (not system))
		(maild-log "~A: User include files are not allowed to contain includes" filename))
	       (is-include
		(dolist (entry expansion)
		  (if* (eq (recip-type entry) :file)
		     then
			  (let ((ifile (recip-file entry)))
			    (if (member ifile files-seen :test #'equal)
				(maild-log "~A: ~A~%Include loop" 
					   filename ali)
			      (parse-aliases-file ifile ht files-seen
						  :system include-sys
						  :domain include-domain)))
		     else
			  (maild-log "~A: ~A~%Right hand side of an include alias must be a list of absolute pathnames" filename ali))))
	       (t
		(setf alias (sanity-check-alias filename alias expansion
						:system system
						:domain domain))
		(when alias
		  (if (gethash alias ht)
		      (maild-log "~A: Redefined alias: ~A" filename alias))
		  (setf (gethash alias ht) expansion)))))))))
    ;; Indicate success
    (setf (second files-entry) (get-universal-time))))


;; returns values:  t, system, domain
(defun aliases-include-directive-p (string)
  (block nil
    (if (equalp string "include")
	(return (values t t nil)))
    (multiple-value-bind (found whole domain)
	(match-regexp "include(\\([^)]+\\))" string)
      (declare (ignore whole))
      (if found
	  (return (values t t domain))))
    (multiple-value-bind (found whole domain)
	(match-regexp "includeuser(\\([^)]+\\))" string)
      (declare (ignore whole))
      (if found
	  (return (values t nil domain))))
    nil))

(defun allowed-user-alias-type-p (entry)
  (or (null (recip-type entry)) 
      (error-recip-p entry)))

(defun sanity-check-alias (filename lhs rhs &key domain system)
  (macrolet ((bailout (format &rest rest)
	       `(progn 
		  (maild-log ,format ,@rest)
		  (return-from sanity-check-alias nil))))
    
    (let* ((parsed (parse-email-addr lhs))
	   (lhs-domain (if parsed (emailaddr-domain parsed))))
      (if (null parsed)
	  (bailout "~A: ~A is not a valid alias left-hand-side: ~A" 
		   filename lhs))
      (if (and domain lhs-domain (not (equalp domain lhs-domain)))
	  (bailout "~A: ~A: Domain portion must be blank or ~A" 
		   filename lhs domain))
      
      (dolist (entry rhs)
	(if (and (not system) (not (allowed-user-alias-type-p entry)))
	    (bailout "~A: ~A: ~A type right-hand-sides are not allowed in user includes" 
		     filename lhs (recip-type entry)))
	(if domain
	    (augment-recip-with-domain entry domain)))
      
      (if (and domain (not lhs-domain))
	  (setf lhs (concatenate 'string lhs "@" domain)))
      lhs)))
      
      
  
(defun parse-alias-line (line)
  (let ((len (length line)))
    (multiple-value-bind (lhs pos)
	(aliases-get-next-word #\: line 0 len :delim-required t)
      (if (null lhs)
	  (error "Invalid aliases line: ~A" line))
      (values lhs (parse-alias-right-hand-side lhs line pos)))))


(defun parse-alias-right-hand-side (lhs line pos)
  (let ((recips (parse-recip-list line :pos pos)))
    (if (null recips)
	(error "Alias ~A has blank expansion" lhs))
    (if (and (>= (count-if #'error-recip-p recips) 1)
	     (> (length recips) 1))
	(error "Problem with alias ~A:  :error: expansions must be alone" lhs))
    (when (find-if #'bad-recip-p recips)
      (format *error-output* "Problem with alias ~A:~%" lhs)
      (dolist (recip recips)
	(if (bad-recip-p recip)
	    (format *error-output* " ~A... ~A~%" 
		    (recip-orig recip)
		    (recip-errmsg recip))))
      (error "Aborting..."))
    recips))
  
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

;; :include: files should be treated as a big multi-line right hand side.
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
    (parse-alias-right-hand-side filename line 0)))

;;; end parsing stuff... 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; begin expansion stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; There are two entrypoints.  alias-transform and expand-alias.

;;; expand-alias attempts to expand an alias.  If there is no expansion,
;;; nil is returned.

;;; alias-transform is like expand-alias except that if no alias
;;; expansion exists, it returns a list of the the argument (converted
;;; to a recip).  This is for the convenience of lookup-recip.


;; Called by lookup-recip
(defun alias-transform (thing)
  (let* ((addr (make-parsed-and-unparsed-address thing))
	 (res (expand-alias addr)))
    (if* (null res)
       then (list (make-recip :addr addr))
       else res)))

;; 'thing' can be a string or an emailaddr struct
;; returns a list of recip structs
;; may include duplicates.
;; Called by quick-verify-recip alias-transform alias-expand-member-list
(defun expand-alias (thing &key (wild t) (require-qualified nil))
  ;;; XXX - may want to move this out for performance reasons
  (update-aliases-info)
  (let ((paddr (make-parsed-and-unparsed-address thing))
	exp)
    (while paddr 
      (setf exp 
	(expand-alias-inner paddr
			    (aliases-info-aliases *aliases*) 
			    nil ;; seen
			    nil ;; owner
			    :wild wild
			    :require-qualified require-qualified))
      (if exp
	  (return exp))
      
      ;; Try again if this address is using address extension syntax.
      (setf paddr (unextended-address paddr)))))
      

;; paddr should be a parsed address.
;; Returns the unextended parsed address and the extension string
(defun unextended-address (paddr)
  (block nil
    (if (null *address-extension-delimiter*)
	(return))
    (let* ((user (emailaddr-user paddr))
	   (pos (search *address-extension-delimiter* user)))
      (if (null pos)
	  (return))
      ;; Make sure not to change the original
      (setf paddr (copy-emailaddr paddr))
      (setf (emailaddr-user paddr) (subseq user 0 pos))
      (setf (emailaddr-orig paddr)
	(if (emailaddr-domain paddr)
	    (concatenate 'string (emailaddr-user paddr) "@"
			 (emailaddr-domain paddr))
	  (emailaddr-user paddr)))
      (values paddr (subseq user (1+ pos))))))
    

;; tries long match (w/ full domain) first.. 
;; then wildcard match (*@domain) [if desired]
;; then short match (just user part) [if require-qualified is nil]

(defun expand-alias-inner (alias ht seen owner &key wild require-qualified)
  (block nil
    ;; Check for loops
    (if (member alias seen :test #'equalp)
	(error "Alias loop involving alias ~S" (emailaddr-orig alias)))
    
    (multiple-value-bind (lhs members)
	(alias-get-entry alias ht :wild wild 
			 :require-qualified require-qualified)

      (if (null members)
	  (return))

      (push alias seen)

      (alias-expand-member-list lhs members ht seen owner))))

;; returns values (lhs rhs) 
;; lhs is returned in case a wildcard match was hit.
(defun alias-get-entry (alias ht &key wild require-qualified)
  (block nil
    (let (members)
      (if (not (local-domain-p alias))
	  (return nil))
      
      (setf members (gethash (emailaddr-orig alias) ht))
      (if members
	  (return (values alias members)))
      
      ;; if there's no domain part, give up.
      (if (null (emailaddr-domain alias))
	  (return nil))
      
      ;; try domain wildcard address instead
      (when wild
	(let ((wildcard (concatenate 'string "*@" (emailaddr-domain alias))))
	  (setf members (gethash wildcard ht))
	  (if members
	      (return (values (parse-email-addr wildcard) members)))))
    
      (if require-qualified
	  (return nil))
      
      ;; try just the local part.
      (let ((lookup (emailaddr-user alias)))
	(setf members (gethash lookup ht))
	(if members
	    (return (values (parse-email-addr lookup) members)))))))

;; 'entry' will be a recip.  It should be a regular recip because
;; the check for :prog, :error: and :file is done beforehand.  
;; sanity check is here anyway.
(defun alias-member-self-referential-p (current-lhs entry ht)
  (if (not (null (recip-type entry)))
      (error "alias-member-self-referential-p called with non-regular recip: ~S" entry))
  (let ((new-lhs (alias-get-entry (recip-addr entry) ht :wild t)))
    (and new-lhs  (emailaddr= new-lhs current-lhs))))
  

;; 'members' is a list of recip structs
(defun alias-expand-member-list (lhs members ht seen owner &key in-include)
  (let (res type ownerstring ownerparsed)
    (setf ownerstring (concatenate 'string "owner-" (emailaddr-orig lhs)))
    (setf ownerparsed (parse-email-addr ownerstring))
    (if (expand-alias ownerparsed :wild nil 
		      :require-qualified (emailaddr-domain lhs))
	(setf owner ownerparsed))
    
    (dolist (member members)
      (setf type (recip-type member))
      
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
	(let* ((includefile (recip-file member))
	       (newmembers (aliases-parse-include-file includefile)))
	  ;; recurse
	  (setf res 
	    (append 
	     (alias-expand-member-list lhs newmembers ht seen owner
				       :in-include includefile)
	     res))))
       
       ;; definite terminals
       ((or (member type '(:prog :file :error))
	    (recip-escaped member)
	    (not (local-domain-p (recip-addr member)))
	    (alias-member-self-referential-p lhs member ht))
	(push (aliases-set-recip-info member owner seen) res))
       
       ;; normal expansion.
       (t
	(let ((exp (expand-alias-inner (recip-addr member) ht seen owner
				       :wild t)))
	  (if (null exp)
	      (push (aliases-set-recip-info member owner seen) res)
	    (setf res (append exp res)))))))
    res))

  
(defun aliases-set-recip-info (recip owner expanded-from)
  (setf recip (copy-recip recip))
  (setf (recip-owner recip) owner)
  (setf (recip-expanded-from recip) expanded-from)
  recip)

