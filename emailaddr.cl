(in-package :user)

(defstruct emailaddr
  user
  domain
  orig) ;; without angle brackets
  
(defstruct addrspec 
  userprecomments
  user
  userpostcomments
  domainprecomments
  domain
  domainpostcomments)



(defun emailaddr= (addr1 addr2)
  (and (equalp (emailaddr-user addr1) (emailaddr-user addr2))
       (equalp (emailaddr-domain addr1) (emailaddr-domain addr2))))
  
(defun emailnullp (addr)
  (and (null (emailaddr-user addr)) 
       (null (emailaddr-domain addr))))


;; Front ends to the complex stuff below.
;; This is intended to be used for envelope addresses.

;; returns values:
;;   list of accepted, parsed addresses.
;;   list of ("rejected-address" "reason")
;;   remaining cruft
(defun parse-email-addr-list (string &key (pos 0) (max (length string))
					  allow-null)
  (block nil
    (let* ((tokens (emailaddr-lex string :pos pos :max max))
	   goods bads string addr)
      (multiple-value-bind (mblist remainder)
	  (parse-mailbox-list tokens)
	(dolist (mailbox (second mblist))
	  (setf addr (mailbox-to-addrspec mailbox))
	  (setf string (with-output-to-string (s) (print-token mailbox s)))
	  ;; null addresses aren't allowed (unless allow-null is t)
	  ;; null user parts aren't allowed.
	  (cond 
	   ((and (null (addrspec-user addr)) (null (addrspec-domain addr))
		 (not allow-null))
	    (push (list string "User unknown")
		  bads))
	   ((and (null (addrspec-user addr)) (addrspec-domain addr))
	    (push (list string "User address required") 
		  bads))
	   (t
	    (push (make-emailaddr :user (addrspec-user addr)
				  :domain (addrspec-domain addr)
				  :orig (strip-angle-brackets string))
		  goods))))
	(values (nreverse goods) (nreverse bads) 
		(with-output-to-string (s) (print-token-list remainder s)))))))

;; Doesn't strip if string= "<>"
(defun strip-angle-brackets (string)
  (if (string= string "<>")
      string
    (replace-regexp string "^<\\(.*\\)>$" "\\1")))
		  
  

(defun mailbox-to-addrspec (mailbox)
  (let ((thing (second mailbox)))
    (cond 
     ((addrspec-p thing)
      thing)
     ((eq (first thing) :name-addr)
      (angle-addr-to-addrspec (third thing)))
     (t
      (error "Unexpected mailbox subtype: ~S" thing)))))


(defun parse-email-addr (string &key (pos 0) (max (length string))
				     allow-null)
  (block nil
    (multiple-value-bind (goods bads cruft)
	(parse-email-addr-list string :pos pos :max max
			       :allow-null allow-null)
      (if (or (/= (length goods) 1) (> (length bads) 0) (string/= cruft ""))
	  (return nil))
      (first goods))))

;; stuff from RFC 2822

(defparameter *wsp* '(:or #\tab #\space))

(defparameter *obs-fws* '((:one-or-more *wsp*)
			  (:zero-or-more 
			   (#\newline (:one-or-more *wsp*)))))

(defparameter *fws* *obs-fws*)

(defun no-ws-ctl-p (char)
  (let ((code (char-code char)))
    (or (<= 1 code 8)
	(= code 11)
	(= code 12)
	(<= 14 code 31)
	(= code 127))))

(defun ctextp (char)
  (let ((code (char-code char)))
    (or (no-ws-ctl-p char)
	(<= 33 code 39)
	(<= 42 code 91)
	(<= 93 code 126))))

(defparameter *quoted-pair* '(#\\ (:char-predicate characterp)))


(defparameter *ccontent* '(:or 
			   (:char-predicate ctextp) 
			   *quoted-pair* 
			   *comment*))

(defparameter *comment* '(#\( 
			  (:zero-or-more 
			   ((:optional *fws*) *ccontent*))
			  (:optional *fws*)
			  #\)))

;; Not the definition from RFC2822.  It didn't work well
;; with my lexer.
(defparameter *cfws* '(:one-or-more (:or *fws* *comment*)))

;; Characters that are allowed in a user name or domain name.
(defun atext-char-p (char)
  (or (alphanumericp char)
      (member char '(#\! #\# #\$ #\% #\& #\' #\* #\+ #\- #\/ #\= #\?
		     #\^ #\_ #\` #\{ #\| #\} #\~))))

(defparameter *atom* '(:one-or-more (:char-predicate atext-char-p)))

(defparameter *dot-atom* '((:optional *cfws*)
			   *dot-atom-text*
			   (:optional *cfws*)))

(defparameter *dot-atom-text* '((:one-or-more (:char-predicate atext-char-p))
				(:zero-or-more 
				 (#\. (:one-or-more 
				       (:char-predicate atext-char-p))))))

(defun qtextp (char)
  (let ((code (char-code char)))
    (or (no-ws-ctl-p char)
	(= code 23)
	(<= 35 code 91)
	(<= 93 code 126))))

(defparameter *qcontent* '(:or (:char-predicate qtextp) *quoted-pair*))

(defparameter *quoted-string* '(#\"
				(:zero-or-more
				 ((:optional *fws*)
				  *qcontent*))
				(:optional *fws*)
				#\"))
				
;; lexical constructs:
;; whitespace [combined into a single space]
;; comments.
;; quoted strings
;; special characters
;; atoms (runs of characters that aren't special)

(defun emailaddr-lex-help (string &key (pos 0) (max (length string)))
  (block nil
    (if (>= pos max)
	(return nil))
    (let (newpos)
      (dolist (pair '((*fws* :whitespace)
		      (*comment* :comment)
		      (*quoted-string* :quoted-string)
		      (*atom* :atom)))
	(setf newpos (lex (first pair) string :pos pos :max max))
	(if newpos
	    (return-from emailaddr-lex-help
	      (values newpos 
		      (list (second pair) (subseq string pos newpos))))))
      ;; must be a special char
      (values (1+ pos) (list :special (schar string pos))))))

(defun emailaddr-lex (string &key (pos 0) (max (length string)))
  (let (res)
    (loop
      (multiple-value-bind (newpos data)
	  (emailaddr-lex-help string :pos pos :max max)
	(if (null newpos)
	    (return))
	(push data res)
	(setf pos newpos)))
    (nreverse res)))

(defun atom-token-p (token)
  (and (listp token) (eq (first token) :atom)))

(defun quoted-string-token-p (token)
  (and (listp token) (eq (first token) :quoted-string)))

(defun comment-token-p (token)
  (and (listp token) (eq (first token) :comment)))

(defun whitespace-token-p (token)
  (and (listp token) (eq (first token) :whitespace)))

(defun some-special-token-p (token char)
  (and (listp token) 
       (eq (first token) :special) 
       (char= char (second token))))

(defun dot-token-p (token)
  (some-special-token-p token #\.))

(defun @-token-p (token)
  (some-special-token-p token #\@))

(defun <-token-p (token)
  (some-special-token-p token #\<))

(defun >-token-p (token)
  (some-special-token-p token #\>))

(defun [-token-p (token)
  (some-special-token-p token #\[))

(defun ]-token-p (token)
  (some-special-token-p token #\]))

(defun colon-token-p (token)
  (some-special-token-p token #\:))

(defun semicolon-token-p (token)
  (some-special-token-p token #\;))

(defun comma-token-p (token)
  (some-special-token-p token #\,))

(defun word-token-p (token)
  (or (atom-token-p token)
      (quoted-string-token-p token)))

(defun strip-comments (tokens)
  (let (comments res)
    (dolist (token tokens)
      (if (comment-token-p token)
	  (push token comments)
	(push token res)))
    ;; Sanity check.  There should only be a single non-comment.
    (if (> (length res) 1)
	(error "strip-comments got more than one non-comment result"))
    (values (nreverse comments) (first res))))

;; Returns a list of the tokens that make up a word.  This could
;; include comments before and after.  Whitespace is stripped if desired.
;; Also return the remaining tokens
;; or.. if strip-comments is true, return
;;   word token, comments, remaining tokens
(defun parse-word (tokens &key strip-comments strip-white)
  (let (res token seenword)
    (loop
      (setf token (first tokens))
      (cond
       ((word-token-p token)
	(if seenword
	    (return)) ;; break out of loop.
	(setf seenword t)
	(push token res))
       ((comment-token-p token) ;; save comments
	(push token res))
       ((whitespace-token-p token)
	(if (not strip-white)
	    (push token res)))
       (t ;; must be a special or something.  Terminate
	(return)))
      (pop tokens))
    (if strip-comments
	(multiple-value-bind (comments non-comment)
	    (strip-comments (nreverse res))
	  (values non-comment comments tokens))
      (values (nreverse res) tokens))))

(defun print-phrase (tokens &optional (stream t))
  (dolist (token tokens)
    (print-token token stream)))

;; Return the tokens that make up a phrase.
;;   plus the remaining tokens
(defun parse-phrase (tokens)
  (block nil
    (let (res)
      (multiple-value-bind (wordtokens newtokens)
	  (parse-word tokens)
	(if (null wordtokens)
	    (return nil))
	(setf res (nconc res wordtokens))
	(setf tokens newtokens))
      (loop
	(if (or (dot-token-p (first tokens))
		(whitespace-token-p (first tokens)))
	    (setf res (nconc res (list (pop tokens))))
	  (multiple-value-bind (wordtokens newtokens)
	      (parse-word tokens)
	    (if (null wordtokens)
		(return))
	    (setf res (nconc res wordtokens))
	    (setf tokens newtokens))))
      (return (values res tokens)))))

(defun unquote-word (token)
  (cond
   ((atom-token-p token)
    (second token))
   ((quoted-string-token-p token)
    (unquote-quoted-string (second token)))
   (t 
    (error "Unexpected token passed to unquote-word: ~S" token))))

;; Returns the unquoted string that is represented by the dotted words.
;;  and the quoted string
;;  and any comments founds within it
;;  and the remaining tokens.
(defun parse-dotted-words (tokens)
  (let (resq resuq comments)
    (loop
      (multiple-value-bind (wordtoken cmnts newtokens)
	  (parse-word tokens :strip-comments t :strip-white t)
	(if (null wordtoken)
	    (return (values resuq resq comments tokens)))
	(if* (null resq)
	   then
		(setf resq "")
		(setf resuq ""))
	(setf resuq (concatenate 'string resuq (unquote-word wordtoken)))
	(setf resq (concatenate 'string resq (second wordtoken)))
	(setf comments (nconc comments cmnts))
	(setf tokens newtokens)
	(if (not (dot-token-p (first tokens)))
	    (return (values resuq resq comments tokens)))
	(setf resq (concatenate 'string resq "."))
	(setf resuq (concatenate 'string resuq "."))
	(pop tokens)))))
  

(defun print-addrspec (token &optional (stream t))
  (dolist (comment (addrspec-userprecomments token))
    (print-token comment stream))
  (if (addrspec-user token)
      (write-string (addrspec-user token) stream))
  (dolist (comment (addrspec-userpostcomments token))
    (print-token comment stream))
  (if (addrspec-domain token)
      (write-char #\@ stream))
  (dolist (comment (addrspec-domainprecomments token))
    (print-token comment stream))
  (if (addrspec-domain token)
      (write-string (addrspec-domain token) stream))
  (dolist (comment (addrspec-domainpostcomments token))
    (print-token comment stream)))


;; Sendmail takes the first comment it finds before the addr-spec and
;; restores it to the front.. and any comments found after that or in
;; the middle of the addr-spec go at the end.  It also allows either
;; the local part or the domain part to be missing.  Higher level
;; code will need to complain when necessary.
(defun parse-addr-spec (tokens)
  (block nil
    (let ((spec (make-addrspec)))
      ;; collect any comments/whitespace.
      (multiple-value-bind (cws newtokens)
	  (collect-comments-and-whitespace tokens)
	(setf (addrspec-userprecomments spec) cws)
	(setf tokens newtokens))
      (if (not (@-token-p (first tokens)))
	  (multiple-value-bind (localpart localpartq comments newtokens)
	      (parse-dotted-words tokens)
	    (declare (ignore localpartq))
	    (if (null localpart)
		(return nil))
	    (setf (addrspec-user spec) localpart)
	    (setf (addrspec-userpostcomments spec) comments)
	    (setf tokens newtokens)))
      ;; done w/ the local part.
      (if (not (@-token-p (first tokens)))
	  (return (values spec tokens)))
      (pop tokens)
      ;; collect any comments/whitespace.
      (multiple-value-bind (cws newtokens)
	  (collect-comments-and-whitespace tokens)
	(setf (addrspec-domainprecomments spec) cws)
	(setf tokens newtokens))

      ;; insert parse-domain-literal stuff here.
      
      (multiple-value-bind (domainpart domainpartq comments newtokens)
	  (parse-dotted-words tokens)
	(declare (ignore domainpartq))
	;; This situation is allowed in sendmail.  It is
	;; treated as a local addr.
	(if (null domainpart)
	    (return (values spec tokens)))
	(setf (addrspec-domain spec) domainpart)
	(setf (addrspec-domainpostcomments spec) comments)
	(values spec newtokens)))))

;; Not to spec
(defun parse-domain-literal (tokens)
  (block nil
    (if (not ([-token-p (first tokens)))
	(return nil))
    (pop tokens)
    (let (res token string)
      (loop
	(setf token (pop tokens))
	(if (null token)
	    (return-from parse-domain-literal))
	(if (]-token-p token)
	    (return))
	(push token res))
      (setf string 
	(with-output-to-string (s)
	  (write-char #\[ s)
	  (print-token-list (nreverse res) s)
	  (write-char #\] s)))
      (values string tokens))))

(defun collect-comments-and-whitespace (tokens)
  (let (res)
    (loop
      (if (or (whitespace-token-p (first tokens))
	      (comment-token-p (first tokens)))
	  (push (pop tokens) res)
	(return)))
    (values (nreverse res) tokens)))

(defun print-angle-addr (token &optional (stream t))
  (dolist (comment (second token))
    (print-token comment stream)
    (write-char #\space stream))
  (write-string "<" stream)
  (print-addrspec (third token) stream)
  (write-string ">" stream)
  (dolist (comment (fourth token))
    (print-token comment stream)
    (write-char #\space stream)))
    
(defun angle-addr-to-addrspec (angleaddr)
  (third angleaddr))

(defun parse-angle-addr (tokens)
  (block nil
    (let (precomments postcomments spec)
      (multiple-value-setq (precomments tokens)
	(collect-comments-and-whitespace tokens))
      (if (not (<-token-p (first tokens)))
	  (return nil))
      (pop tokens)
      (if (>-token-p (first tokens)) ;; <>
	  (return 
	    (values
	     (list :angle-addr precomments (make-addrspec) nil)
	     (rest tokens))))
      (multiple-value-setq (spec tokens)
	(parse-addr-spec tokens))
      (if (null spec)
	  (return nil))
      (if (not (>-token-p (first tokens)))
	  (return nil))
      (pop tokens)
      (multiple-value-setq (postcomments tokens)
	(collect-comments-and-whitespace tokens))
      (values 
       (list :angle-addr precomments spec postcomments)
       tokens))))

(defun print-name-addr (token &optional (stream t))
  (if (second token)
      (print-phrase (second token) stream))
  (print-angle-addr (third token) stream))
  
(defun parse-name-addr (tokens)
  (block nil
    (multiple-value-bind (phrase newtokens)
	(parse-phrase tokens)
      ;; phrase is optional.
      (if phrase
	  (setf tokens newtokens))
      (multiple-value-bind (angleaddr newtokens)
	  (parse-angle-addr tokens)
	(if (null angleaddr)
	    (return nil))
	(values (list :name-addr phrase angleaddr) newtokens)))))


(defun print-mailbox (token &optional (stream t))
  (print-token (second token) stream))

;; mailbox = name-addr / addr-spec
(defun parse-mailbox (tokens)
  (block nil
    (multiple-value-bind (nameaddr newtokens)
	(parse-name-addr tokens)
      (if nameaddr
	  (return (values (list :mailbox nameaddr) newtokens))))
    (multiple-value-bind (addrspec newtokens)
	(parse-addr-spec tokens)
      (if addrspec
	  (return (values (list :mailbox addrspec) newtokens)))
      nil)))

(defun print-group (token &optional (stream t))
  (print-phrase (second token) stream)
  (write-char #\: stream)
  (print-token (third token) stream)
  (write-char #\; stream))


;;group           =       display-name ":" [mailbox-list / CFWS] ";"
(defun parse-group (tokens)
  (block nil
    (multiple-value-bind (dispname newtokens)
	(parse-phrase tokens)
      (if (null dispname)
	  (return nil))
      (setf tokens newtokens)
      (if (not (colon-token-p (first tokens)))
	  (return nil))
      (pop tokens)
      (multiple-value-bind (mailboxlist newtokens)
	  (parse-mailbox-list tokens)
	(setf tokens newtokens)
	(if (not (semicolon-token-p (first tokens)))
	    (return nil))
	(pop tokens)
	(values (list :group dispname mailboxlist) tokens)))))

(defun print-address (token &optional (stream t))
  (print-token (second token) stream))

;; mailbox/group
;; we check for group first.. it works out better
(defun parse-address (tokens)
  (block nil
    (multiple-value-bind (group newtokens)
	(parse-group tokens)
      (if group
	  (return (values (list :address group) newtokens))))
    (multiple-value-bind (mailbox newtokens)
	(parse-mailbox tokens)
      (if mailbox
	  (return (values (list :address mailbox) newtokens))))
    nil))
      
  

;; To: can be an address-list.
;; From: can be a mailbox-list  
;; (address lists are a superset of mailbox lists because they can
;; contain groups)

(defun print-mailbox-list (token &optional (stream t))
  (let (needcomma)
    (dolist (mailbox (second token))
      (if needcomma
	  (write-char #\, stream))
      (print-mailbox mailbox stream)
      (setf needcomma t))))
    

(defun parse-mailbox-list (tokens)
  (block nil
    (let (mailboxes)
      (loop
	(multiple-value-bind (mailbox newtokens)
	    (parse-mailbox tokens)
	  (if (null mailbox)
	      (return (values (list :mailbox-list (nreverse mailboxes))
			      tokens)))
	  (push mailbox mailboxes)
	  (setf tokens newtokens))
	(if (not (comma-token-p (first tokens)))
	    (return (values (list :mailbox-list (nreverse mailboxes))
			    tokens)))
	(pop tokens)))))

(defun print-address-list (token &optional (stream t))
  (let (needcomma)
    (dolist (address (second token))
      (if needcomma
	  (write-char #\, stream))
      (print-address address stream)
      (setf needcomma t))))


(defun parse-address-list (tokens)
  (block nil
    (let (addresses)
      (loop
	(multiple-value-bind (address newtokens)
	    (parse-address tokens)
	  (if (null address)
	      (return (values (list :address-list (nreverse addresses))
			      tokens)))
	  (push address addresses)
	  (setf tokens newtokens))
	(if (not (comma-token-p (first tokens)))
	    (return (values (list :address-list (nreverse addresses))
			    tokens)))
	(pop tokens)))))

;;; for regenerating headers.
(defun print-token-list (tokens &optional (stream t))
  (dolist (token tokens)
    (print-token token stream)))

(defun print-token (token &optional (stream t))
  (if (addrspec-p token)
      (print-addrspec token stream)
    (case (first token)
      ((:comment :whitespace :quoted-string :atom)
       (write-string (second token) stream))
      (:name-addr
       (print-name-addr token stream))
      (:mailbox-list
       (print-mailbox-list token stream))
      (:group
       (print-group token stream))
      (:adddress
       (print-address token stream))
      (:mailbox
       (print-mailbox token stream))
      (:address-list
       (print-address-list token stream))
      (:mailbox-list
       (print-mailbox-list token stream))
      (:special
       (write-char (second token) stream))
      (t
       (error "token ~S not handled yet." token)))))
