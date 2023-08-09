#lang typed/racket/base

(provide (all-defined-out))

(require racket/pretty)

(require digimon/port)

(require "cfb.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-encrypted-input-stream : (-> Input-Port MS-Crypto Input-Port)
  (lambda [/dev/cfbin crypto]
    (pretty-write crypto)
    /dev/cfbin))
