#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require "color.rkt")

(require "../../../dialect.rkt")
(require "../../../shared/ml/common-simple-types.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-attribute blip-fill #:for mox
  ([dpi : Index #:= #false #:<-> xml:attr-value->index]
   [rotWithShape : XML-Boolean #:= #false #:<-> xml:attr-value->boolean]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-structs mox-fill-property : MOX-Fill-Property ()
  #:substruct
  [(define-struct mox-group-fill : MOX-Group-Fill () #:transparent)
   
   (define-struct mox-blip-fill : MOX-Blip-Fill
     ([attlist : MOX:Attr:Blip-Fill]
      #;[blip ])
     #:transparent)

   (define-struct mox-line-fill-property : MOX-Line-Fill-Property ()
     #:substruct
     [(define-struct mox-none-fill : MOX-None-Fill () #:transparent)

      (define-struct mox-solid-fill : MOX-Solid-Fill
        ([color : (Option MOX-Color-Attribute)])
        #:transparent)

      (define-struct mox-gradient-fill : MOX-Gradient-Fill
        ()
        #:transparent)

      (define-struct mox-pattern-fill : MOX-Pattern-Fill
        ()
        #:transparent)]
     
     #:transparent)]

  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-none-fill-singleton : MOX-None-Fill (mox-none-fill))
(define mox-group-fill-singleton : MOX-Group-Fill (mox-group-fill))
