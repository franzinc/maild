(in-package :user)

(define-condition data-timeout (error)
  ())

(define-condition data-read-timeout (data-timeout)
  ())

  