#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define opc-part-name-normalize : (-> String String)
  (lambda [name]
    name))

(define opc-part-name-normalize/zip : (-> String String)
  (lambda [name]
    ; MOX part names are prefixed with '/', which should be removed for zip entries
    (cond [(string=? name "") #| deadcode|# ""]
          [(char=? (string-ref name 0) #\/) (opc-part-name-normalize (substring name 1))]
          [else name])))
