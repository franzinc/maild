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
;; $Id: lex.cl,v 1.3 2003/07/08 18:15:52 layer Exp $

(in-package :user)

(defun lex-list (spec string &key (pos 0) (max (length string)))
  (block nil
    ;; special lists
    (case (first spec)
      (:char-predicate
       (if (/= (length spec) 2)
	   (error ":function spec should only have one arg. ~S" spec))
       (if (>= pos max)
	   (return nil))
       (if (funcall (second spec) (schar string pos))
	   (return (1+ pos))
	 (return nil)))
      (:optional
       (if (/= (length spec) 2)
	   (error ":optional spec should only have one arg. ~S" spec))
       (let ((newpos (lex (second spec) string :pos pos :max max)))
	 (if newpos
	     (return newpos)
	   (return pos))))
      (:zero-or-more
       (if (/= (length spec) 2)
	   (error ":zero-or-more spec should only have one arg. ~S" spec))
       (loop
	 (let ((newpos (lex (second spec) string :pos pos :max max)))
	   (if (null newpos)
	       (return-from lex-list pos))
	   (setf pos newpos))))
      (:one-or-more
       (if (/= (length spec) 2)
	   (error ":one-or-more spec should only have one arg. ~S" spec))
       (let ((newpos (lex (second spec) string :pos pos :max max)))
	 (if (null newpos)
	     (return nil))
	 (setf pos newpos)
	 (loop 
	   (setf newpos (lex (second spec) string :pos pos :max max))
	   (if (null newpos)
	       (return pos))
	   (setf pos newpos))
	 (return pos)))
      (:or 
       ;; takes the longest match.
       (let (maxpos newpos)
	 (dolist (innerspec (rest spec))
	   (setf newpos (lex innerspec string :pos pos :max max))
	   (if newpos
	       (if (or (null maxpos) (> newpos maxpos))
		   (setf maxpos newpos))))
	 (return maxpos)))
      #|
       ;; takes the first match
       (let (newpos)
	 (dolist (innerspec (rest spec))
	   (setf newpos (lex innerspec string :pos pos :max max))
	   (if newpos
	   (return-from lex-list newpos)))
	       (return nil)))
	       |#
      (t
       (if (keywordp (first spec))
	   (error "Unrecognized spec: ~S" spec))))
    
    ;; ordinary lists.  Everything in the list must be lexable.
    (dolist (innerspec spec)
      (let ((newpos (lex innerspec string :pos pos :max max)))
	(if (null newpos)
	    (return-from lex-list nil))
	(setf pos newpos)))
    (return pos)))

(defun lex (spec string &key (pos 0) (max (length string)))
  (block nil
    (if (listp spec)
	(return (lex-list spec string :pos pos :max max)))
    (if (>= pos max)
	(return nil))
    (let ((char (schar string pos)))
      (if (symbolp spec)
	  (return (lex (symbol-value spec) string :pos pos :max max)))
      (if (characterp spec)
	  (if (char= char spec)
	      (return (1+ pos))
	    (return nil)))
      (if (not (listp spec))
	  (error "unexpected spec: ~S" spec)))))
      
