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
;; $Id: smtp.cl,v 1.4 2003/07/08 18:15:53 layer Exp $

(in-package :user)

;; Stuff common to the smtp server and client

;; handles Newline or CR/LF EOL convention.
(defun smtp-get-line (sock buf timeout)
  "Returns a keyword if something went wrong. Otherwise returns a string"
  (let ((pos 0)
	longline
	lastchar
	char)
    ;; with-timeout is simpler here than with-socket-timeout and error
    ;; handling
    (mp:with-timeout (timeout :timeout)
      (loop
	(setf char (ignore-errors (read-char sock nil nil)))
	(if (null char)
	    (return :eof))
	
	(when (eq char #\linefeed)
	  (if longline
	      (return :longline))
	  (if (and (eq lastchar #\return) (> pos 0))
	      (decf pos))
	  (return (subseq buf 0 pos)))
	
	(if* (not longline)
	   then
		(setf (schar buf pos) char)
		(incf pos))
	(if (>= pos *maxlinelen*)
	    (setf longline t))
	(setf lastchar char)))))

