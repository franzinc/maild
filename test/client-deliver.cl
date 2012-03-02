#! /fi/cl/9.0.beta/bin/mlisp -#!

(eval-when (compile eval load)
  (require :smtp)
  (require :osi)
  (use-package :excl.osi))

(defvar *server* "quadra:9999")

(defvar *from* (pwent-name (getpwuid (getuid))))

(defun read-stdin (&aux line)
  (with-output-to-string (s)
    (loop
      (setq line (read-line *standard-input*
			    nil
			    *standard-input*))
      (when (eq line *standard-input*)
	(return))
      (write-string line s))))

(let ((rest (sys:command-line-arguments)))
  (or (= 2 (length (cdr rest)))
      (error "usage: to range"))
  (let ((to (second rest))
	(raw-range (third rest)))
    (multiple-value-bind (matched whole start end)
	(match-re "^(\\d+)-(\\d+)$" raw-range)
      (declare (ignore whole))
      (or matched (error "Bad range: ~a." raw-range))
      (setq start (parse-integer start))
      (setq end (parse-integer end))
      (do* ((i start (1+ i)))
	  ((> i end))
	(net.post-office:send-letter
	 *server*
	 *from*				;from
	 to				;to
	 (format nil "body~d" i)	;body
	 :subject (format nil "subject~d" i))))))
