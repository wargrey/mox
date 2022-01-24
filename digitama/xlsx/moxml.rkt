#lang typed/racket/base

(provide (all-defined-out))

(require "../moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct mox-excel moxml
  ()
  #:type-name MOX-Excel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define moxml-excel-agent : (MOXML-Agentof MOX-Excel)
  (lambda []
    (values 'xlsx

            (λ [[entry : String] [type : Symbol] [/dev/pkgin : Input-Port]] : (Option Void)
              #false)

            (λ [] : MOX-Excel
              (mox-excel)))))
