#lang typed/racket/base

(provide (all-defined-out))

(define-type MOXML-Unzip (-> Bytes Symbol Input-Port (Option Void)))
(define-type (MOXML-Realize x) (-> x))
(define-type (MOXML-Agentof x) (-> (Values Symbol MOXML-Unzip (MOXML-Realize x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct moxml () #:type-name MOXML)
