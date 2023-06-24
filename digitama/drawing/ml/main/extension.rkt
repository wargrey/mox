#lang typed/racket/base

(provide (all-defined-out) XML-Attribute-Extract)

(require "../../../dialect.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (MOX-Art-Extension-With D) (MOX+Extension D MOX:Office-Art-Extension-List))

(struct (MOX Ext) mox+extension
  ([datum : MOX]
   [extLst : (Option Ext)])
  #:type-name MOX+Extension
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-element office-art-extension-list #:for mox ())
