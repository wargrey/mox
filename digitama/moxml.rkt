#lang typed/racket/base

(provide (all-defined-out))

(define-type MOXML-Package-Type (U 'full 'text 'template))

(define-type MOXML-Unzip (-> String Symbol Input-Port (Option Void)))
(define-type (MOXML-Realize x) (-> x))
(define-type (MOXML-Agentof x) (-> MOXML-Package-Type (Values Symbol MOXML-Unzip (MOXML-Realize x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct moxml () #:type-name MOXML)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define moxml-null-agent : (MOXML-Agentof MOXML)
  (lambda [pkg-type]
    (values 'mox moxml-null-unzip moxml)))

(define moxml-null-unzip : MOXML-Unzip
  (lambda [entry type /dev/pkgin]
    #false))
