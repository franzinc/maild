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
;; $Id: spf.cl,v 1.3 2007/05/18 16:21:56 dancy Exp $

(in-package :user)

;; RFC4408

(define-bnf nil nil
  (record (version terms (:* sp))
	  #'(lambda (res)
	      (second res)))

  (version "v=spf1")
  
  (terms (:* ((:one-or-more sp) (:or directive modifier))))

  (directive ((:optional qualifier) mechanism))

  (qualifier (:or "+" "-" "?" "~"))

  (mechanism (:or all include a mx ptr ip4 ip6 exists))

  (all "all")

  (include ("include" ":" domain-spec))

  (a ("a" (:optional (":" domain-spec)) (:optional dual-cidr-length)))

  (mx ("mx" (:optional (":" domain-spec)) (:optional dual-cidr-length)))

  (ptr ("ptr" (:optional (":" domain-spec))))

  (ip4 ("ip4" ":" ip4-network (:optional ip4-cidr-length)))

  (ip6 ("ip6" ":" ip6-network (:optional ip6-cidr-length)))

  (exists ("exists" ":" domain-spec))

  (modifier (:or redirect explanation unknown-modifier))

  (redirect ("redirect" "=" domain-spec))

  (explanation ("exp" "=" domain-spec))

  (unknown-modifier (name "=" macro-string))

  (ip4-cidr-length ("/" (:one-or-more digit)))
  
  (ip6-cidr-length ("/" (:one-or-more digit)))
  
  (dual-cidr-length ((:optional ip4-cidr-length) (:optional ("/" ip6-cidr-length))))
  

  (ip4-network (qnum "." qnum "." qnum "." qnum))

  ;; simplified 
  (qnum (:one-or-more digit))
  
  (ip6-network (:one-or-more (:or hexdig ":"))) ;; not exact

  #+ignore
  (domain-spec (macro-string domain-end))
  
  #+ignore
  (domain-end (:or ("." toplabel (:optional "."))
		   macro-expand))
  
  (domain-spec macro-string)
  
  (toplabel (:or ((:* alphanum) alpha (:* alphanum))
		 ((:one-or-more alphanum) "-" (:* (:or alphanum "-")) alphanum)))
  
  (alphanum (:or alpha digit))
  
  (explain-string (:* (:or macro-string sp)))
  
  (macro-string (:* (:or macro-expand macro-literal)))
  
  (macro-expand (:or ("%{" macro-letter transformers (:* delimiter) "}")
		     "%%" "%_" "%-"))
  
  (macro-literal (:or (:char-range #x21 #x24) (:char-range #x26 #x7e)))
  
  (macro-letter (:or "s" "l" "o" "d" "i" "p" "h" "c" "r" "t" "v"))
  
  (transformers ((:* digit) (:optional "r"))
		(lambda (res)
		  (list 
		   (or (ignore-errors
			(parse-integer 
			 (list-to-delimited-string (first res) "")))
		       0)
		   (if (second res) :reverse))))
  
  (delimiter (:or "." "-" "+" "," "/" "_" "="))
  
  (name (alpha (:* (:or alpha digit "-" "_" ".")))
	#'(lambda (res)
	    (concatenate 'string
	      (first res)
	      (list-to-delimited-string (second res) "")))))

(defun test-parse (string)
  (let ((max (length string)))
    (multiple-value-bind (ok res pos)
	(bnf-parse-record string)
      (if (not ok)
	  (error "Parse failed.~%"))
      (if (/= pos max)
	  (error "Stopped parsing at: ~a~%" (subseq string pos)))
      (pprint res)
      t)))
      
