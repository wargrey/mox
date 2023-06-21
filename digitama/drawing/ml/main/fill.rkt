#lang typed/racket/base

(provide (all-defined-out))

(require "color.rkt")

(require "../../../dialect.rkt")
(require "../../../shared/ml/common-simple-types.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-Line-Fill-Property (U MOX:None-Fill MOX:Solid-Fill MOX:Gradient-Fill MOX:Pattern-Fill))
(define-type MOX-Fill-Property (U MOX-Line-Fill-Property MOX:Group-Fill MOX:Blip-Fill))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-element none-fill #:for mox ())
(define-mox-element group-fill #:for mox ())

(define-mox-element solid-fill #:for mox ())
(define-mox-element gradient-fill #:for mox ())
(define-mox-element pattern-fill #:for mox ())

(define-mox-element blip-fill #:for mox
  #:attlist
  ([dpi : Index #:= #false #:<-> xml:attr-value->index]
   [rotWithShape : XML-Boolean #:= #false #:<-> xml:attr-value->boolean])
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-none-fill-singleton : MOX:None-Fill (mox:none-fill))
(define mox-group-fill-singleton : MOX:Group-Fill (mox:group-fill))
