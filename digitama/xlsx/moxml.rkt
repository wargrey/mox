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

            (λ [[entry : Bytes] [dir? : Boolean] [/dev/pkgin : Input-Port] [type : Symbol] [timestamp : (Option Natural) #false]] : (Option Void)
              #false)

            (λ [] : MOX-Excel
              (mox-excel)))))
