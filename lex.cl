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
      
