#lang typed/racket/base

(provide (all-defined-out))

(require "../moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct mox-word moxml
  ()
  #:type-name MOX-Word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define moxml-word-agent : (MOXML-Agentof MOX-Word)
  (lambda []
    (values 'docx

            (λ [[entry : Bytes] [dir? : Boolean] [/dev/pkgin : Input-Port] [type : Symbol] [timestamp : (Option Natural) #false]] : (Option Void)
              #false)

            (λ [] : MOX-Word
              (mox-word)))))
