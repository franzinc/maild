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
;; $Id: emailaddr.cl,v 1.13 2006/04/12 17:34:29 dancy Exp $

(in-package :user)

(defstruct emailaddr
  user
  domain
  
  ;; Note that in many cases this is not the actual string that was
  ;; supplied, but one constructed from previously parsed elements.
  ;; A better name for this slot would be "string" or something like
  ;; that.
  orig ;; Without angle brackets.
  )
  
(defstruct addrspec 
  userprecomments
  user
  userpostcomments
  domainprecomments
  domain
  domainpostcomments)

;; utilities

(defun emailaddr= (addr1 addr2)
  (and (equalp (emailaddr-user addr1) (emailaddr-user addr2))
       (equalp (emailaddr-domain addr1) (emailaddr-domain addr2))))
  
(defun emailnullp (addr)
  (and (null (emailaddr-user addr)) 
       (null (emailaddr-domain addr))))

(defun make-parsed-and-unparsed-address (thing)
  (if (stringp thing)
      (let ((parsed (parse-email-addr thing)))
	(if (null parsed)
	    (error "Invalid address: ~A" thing))
	(values parsed thing))
    (values thing (emailaddr-orig thing))))


;; Front ends to the complex stuff below.
;; This is intended to be used for envelope addresses.

(defun sep-tokens-by-comma (tokens)
  (let (res)
    (while tokens
	   (let (tmp)
	     ;; skip any leading whitespace
	     (if (whitespace-token-p (first tokens))
		 (pop tokens))
	     (while (and tokens (not (comma-token-p (first tokens))))
		    (push (pop tokens) tmp))
	     (if tmp ;; don't add blank entries
		 (push (nreverse tmp) res)))
	   ;; we're at a comma or done w/ tokens.
	   (pop tokens)) 
    (nreverse res)))

;; addrspec-to-emailaddr, :operator
(defun printable-from-addrspec (addr)
  (if (null (addrspec-user addr))
      (if (null (addrspec-domain addr))
	  "<>"
	(format nil "@~A" (addrspec-domain addr)))
    (if (null (addrspec-domain addr))
	(addrspec-user addr)
      (format nil "~A@~A" (quote-if-necessary (addrspec-user addr)) (addrspec-domain addr)))))

(defun mailbox-to-addrspec (mailbox)
  (let ((thing (second mailbox)))
    (cond 
     ((addrspec-p thing)
      thing)
     ((nameaddr-p thing)
      (angle-addr-to-addrspec (nameaddr-angleaddr thing)))
     (t
      (error "Unexpected mailbox subtype: ~S" thing)))))

;; mailbox-to-emailaddr, :operator
(defun addrspec-to-emailaddr (addr)
  (make-emailaddr :user (addrspec-user addr)
		  :domain (addrspec-domain addr)
		  :orig (printable-from-addrspec addr)))

;;;parse-email-addr, :operator
;;;make-recip-from-mailbox, :operator
(defun mailbox-to-emailaddr (mailbox)
  (addrspec-to-emailaddr (mailbox-to-addrspec mailbox)))

;; only accepts mailbox style addresses
;; Called by many functions.
(defun parse-email-addr (string &key (pos 0) allow-null)
  "Parses a mailbox style address (local-port@domain)
   and returns an emailaddr struct.  Returns nil
   if the address is unparseable."
  (multiple-value-bind (mb remaining-tokens)
      (parse-mailbox (emailaddr-lex string :pos pos))
    (when (and mb (null remaining-tokens))
      (let ((addr (mailbox-to-emailaddr mb)))
	(if* (or
	      ;; <> address but allow-null not true
	      (and (null (emailaddr-user addr)) (null (emailaddr-domain addr))
		   (not allow-null))
	      ;; empty user part but domain specified (@domain.com).  Bogus
	      (and (null (emailaddr-user addr)) (emailaddr-domain addr)))
	   then ;; Reject
		nil
	   else ;; Good to go.  Return the emailaddr struct.
		addr)))))


;; stuff from RFC 2822

;; Added #\return even though it's not in the spec
(defparameter *wsp* '(:or #\tab #\space #\return))

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
;; Note that this doesn't include ".".
(defun atext-char-p (char)
  (or (alphanumericp char)
      (member char '(#\! #\# #\$ #\% #\& #\' #\* #\+ #\- #\/ #\= #\?
		     #\^ #\_ #\` #\{ #\| #\} #\~))))

(defun dot-atom-char-p (char)
  (or (atext-char-p char) (char= char #\.)))

(defparameter *atom* '(:one-or-more (:char-predicate atext-char-p)))

(defparameter *dot-atom* '((:optional *cfws*)
			   *dot-atom-text*
			   (:optional *cfws*)))

(defparameter *dot-atom-text* '((:one-or-more (:char-predicate atext-char-p))
				(:zero-or-more 
				 (#\. (:one-or-more 
				       (:char-predicate atext-char-p))))))

;; Less strict than the specification since some popular email systems
;; (Yahoo mail, for example) pass emails with 8-bit characters in the
;; headers. 
(defun qtextp (char)
  (let ((code (char-code char)))
    (or (no-ws-ctl-p char)
	(= code 23)
	(<= 35 code 91)
	(<= 93 code 255)))) ;; strictly, should be (<= 93 code 126)

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

(defun unquote-quoted-string (string)
  (let ((outstring (make-string (length string)))
	(outpos 0)
	(inpos 0)
	(max (length string))
	quote
	char)
    (while (< inpos max)
      (setf char (schar string inpos))
      (cond
       (quote
	(setq quote nil)
	(setf (schar outstring outpos) char)
	(incf outpos)
	(incf inpos))
       ((char= char #\")
	(incf inpos))
       ((char= char #\\)
	(incf inpos)
	(setf quote t))
       (t
	(setf (schar outstring outpos) char)
	(incf outpos)
	(incf inpos))))
    (subseq outstring 0 outpos)))



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
  "If TOKENS can successfully be parsed as an addr-spec,
   returns an addrspec struct and any remaining tokens.
   Otherwise, returns nil"
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

(defstruct nameaddr
  displayname
  angleaddr)

(defun print-nameaddr (token &optional (stream t))
  (if (nameaddr-displayname token)
      (print-phrase (nameaddr-displayname token) stream))
  (print-angle-addr (nameaddr-angleaddr token) stream))
  
(defun parse-nameaddr (tokens)
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
	(values 
	 (make-nameaddr :displayname phrase :angleaddr angleaddr)
	 newtokens)))))

(defun print-mailbox (token &optional (stream t))
  (print-token (second token) stream))

;; mailbox = name-addr / addr-spec
(defun parse-mailbox (tokens)
  "If TOKENS can successfully be parsed as a mailbox,
   returns either 
     (:mailbox nameaddr-struct) and remaining tokens
       or
     (:mailbox addrspec-struct) and remaining tokens.
   Otherwise returns nil."
  (block nil
    (multiple-value-bind (nameaddr newtokens)
	(parse-nameaddr tokens)
      (if nameaddr
	  (return (values (list :mailbox nameaddr) newtokens))))
    (multiple-value-bind (addrspec newtokens)
	(parse-addr-spec tokens)
      (if addrspec
	  (return (values (list :mailbox addrspec) newtokens)))
      nil)))

(defun skip-cfws (tokens)
  (while (and tokens 
	      (or (whitespace-token-p (first tokens))
		  (comment-token-p (first tokens))))
    (pop tokens))
  tokens)


(defun collect-cfws (tokens)
  (let (res)
    (while (and tokens 
		(or (whitespace-token-p (first tokens))
		    (comment-token-p (first tokens))))
      (push (pop tokens) res))
    (values (nreverse res) tokens)))

(defun print-groupspec (token &optional (stream t))
  (print-phrase (groupspec-displayname token) stream)
  (write-char #\: stream)
  (print-token (groupspec-mailbox-list token) stream)
  (print-token-list (groupspec-pre-semi token) stream)
  (write-char #\; stream)
  (print-token-list (groupspec-post-semi token) stream))

(defstruct groupspec
  displayname
  mailbox-list
  pre-semi
  post-semi)

;;group           =       display-name ":" [mailbox-list / CFWS] ";"
(defun parse-group (tokens)
  (block nil
    (let ((g (make-groupspec)))
      (multiple-value-bind (dispname newtokens)
	  (parse-phrase tokens)
	(if (null dispname)
	    (return nil))
	(setf tokens newtokens)
	(if (not (colon-token-p (first tokens)))
	    (return nil))
	(pop tokens)
	(setf (groupspec-displayname g) dispname))
      (multiple-value-bind (mailboxlist newtokens)
	  (parse-mailbox-list tokens)
	(setf tokens newtokens)
	(setf (groupspec-mailbox-list g) mailboxlist))
      (multiple-value-bind (pre newtokens)
	  (collect-cfws tokens)
	(setf (groupspec-pre-semi g) pre)
	(setf tokens newtokens))
      ;; Check for the required semicolon
      (if (not (semicolon-token-p (first tokens)))
	  (return nil))
      (pop tokens) ;; pop it
      (multiple-value-bind (post newtokens)
	  (collect-cfws tokens)
	(setf (groupspec-post-semi g) post)
	(setf tokens newtokens))
      (values g tokens))))

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
  (cond
   ((addrspec-p token)
    (print-addrspec token stream))
   ((groupspec-p token)
    (print-groupspec token stream))
   ((nameaddr-p token)
    (print-nameaddr token stream))
   (t
    (case (first token)
      ((:comment :whitespace :quoted-string :atom)
       (write-string (second token) stream))
      (:mailbox-list
       (print-mailbox-list token stream))
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
       (error "token ~S not handled yet." token))))))

;; unquotes quoted strings
(defun tokens-to-string (tokens &key strip-trailing-white)
  (let ((string
	 (with-output-to-string (s)
	   (dolist (token tokens)
	     (case (first token)
	       ((:comment :whitespace :atom)
		(write-string (second token) s))
	       (:special
		(write-char (second token) s))
	       (:quoted-string
		(write-string 
		 (subseq (second token) 1 (1- (length (second token))))
		 s))
	       (t
		(error "token ~S not handled by tokens-to-string yet." 
		       token)))))))
    (if strip-trailing-white
	(replace-regexp string "\\b+$" "")
      string)))

;; Called by make-from-header and printable-from-addrspec
(defun quote-if-necessary (string)
  "If STRING consists entirely of a combination of dot-atom 
   and space characters, then it is returned unmolested.
   Otherwise returns a version of STRING which is surrounded
   by double quotes."
  (if* (every #'(lambda (char)
		  (or (dot-atom-char-p char) (excl::whitespace-char-p char)))
	      string)
     then string
     else (format nil "~s" string)))
