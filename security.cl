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
;; $Id: security.cl,v 1.5 2003/07/08 18:15:53 layer Exp $

(in-package :user)

(defun world-or-group-writable-p (file &key sb)
  (let ((sb (if sb sb (stat file))))
    (values
     (/= 0 (logand #o22 (stat-mode sb)))
     sb))) ;; in case there are other tests to be done

(defun verify-security (file &key writable-file-okay)
  (if (world-or-group-writable-p (dirname file))
      (error 
       "~A is in a world writable directory.  Aborting for security reasons"
       file))
  (let ((sb (stat file)))
    (if (and (not writable-file-okay)
	     (world-or-group-writable-p file :sb sb))   
	(error "~A is a world writable file.  Aborting for security reasons"
	       file))
    sb))

(defun verify-root-only-file (file)
  (let ((sb (verify-security file)))
    (if (/= 0 (stat-uid sb))
	(error "File ~A isn't owned by root.  Aborting for security reasons" 
	       file))
    sb))

(defun verify-real-user-is-root ()
  (if (/= (getuid) 0)
      (error "Permission denied")))
