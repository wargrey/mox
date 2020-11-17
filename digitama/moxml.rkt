#lang typed/racket/base

(provide (all-defined-out))

(define-type MOXML-Unzip (-> Bytes Boolean Input-Port Symbol (Option Natural) (Option Void)))
(define-type MOXML-Realize (All (x) (-> (∩ MOXML x))))
(define-type MOXML-Agentof (All (x) (-> (Values Symbol MOXML-Unzip (MOXML-Realize x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct moxml () #:type-name MOXML)
