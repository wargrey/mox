#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require "../../../dialect.rkt")
(require "../../../shared/ml/common-simple-types.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-structs mox-extension-list : MOX-Extension-List ()
  #:substruct
  [(define-struct mox-office-art-extension-list : MOX-Office-Art-Extension-List () #:transparent)]
  
  #:transparent)
