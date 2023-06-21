#lang typed/racket/base

(provide (all-defined-out))

(require "../../../dialect.rkt")
(require "../../../shared/ml/common-simple-types.rkt")

(require "base.rkt")
(require "fill.rkt")
(require "extension.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Line-Dash-Property (U Preset-Line-Dash-Type (Listof MOX#Dash-Stop)))
(define-type Line-Join-Property (U 'round 'bevel MOX#Line-Join-Miter-Property))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-xml-enumeration line-end-type : Line-End-Type #:for mox [none triangle stealth diamond oval arrow])
(define-xml-enumeration line-end-width : Line-End-Width #:for mox [sm med lg])
(define-xml-enumeration line-end-length : Line-End-Length #:for mox [sm med lg])

(define-xml-enumeration line-cap : Line-Cap #:for mox [rnd sq flat])
(define-xml-enumeration compound-line : Compound-Line #:for mox [sng dbl thickThin thinThick tri])
(define-xml-enumeration pen-alignment : Pen-Alignment #:for mox [ctr in])

(define-xml-enumeration preset-line-dash-type : Preset-Line-Dash-Type #:for mox
  [solid dot dash lgDash dashDot lgDashDot lgDashDotDot sysDash sysDot sysDashDot sysDashDotDot])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox:attr-value->line-width : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v 0 20116800)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-attribute line-end-property #:for mox
  ([type : Line-End-Type #:= [#false 'none] #:<-> mox:attr-value->line-end-type]
   [w : Line-End-Width #:= #false #:<-> mox:attr-value->line-end-width]
   [len : Line-End-Length #:= #false #:<-> mox:attr-value->line-end-length]))

(define-mox-attribute line-join-miter-property #:for mox
  ([type : Line-End-Type #:= [#false 'none] #:<-> mox:attr-value->line-end-type]
   [w : Line-End-Width #:= #false #:<-> mox:attr-value->line-end-width]
   [len : Line-End-Length #:= #false #:<-> mox:attr-value->line-end-length]))

(define-mox-attribute dash-stop #:for mox
  ([d : XML-Nonnegative-Percentage #:<-> xml:attr-value+>percentage]
   [sp : XML-Nonnegative-Percentage #:<-> xml:attr-value+>percentage]))

(define-mox-attribute line-dash #:for mox
  ([val : Preset-Line-Dash-Type #:<-> mox:attr-value->preset-line-dash-type]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-element line-property #:for mox
  #:attlist
  ([w : Index #:= #false #:<-> mox:attr-value->line-width]
   [cap : Line-Cap #:= #false #:<-> mox:attr-value->line-cap]
   [cmpd : Compound-Line #:= #false #:<-> mox:attr-value->compound-line]
   [algn : Pen-Alignment #:= #false #:<-> mox:attr-value->pen-alignment])
  ([fill : (Option MOX-Line-Fill-Property) #false]
   [dash : (Option Line-Dash-Property) #false]
   [join : (Option Line-Join-Property) #false]
   [headEnd : (Option MOX#Line-End-Property) #false]
   [tailEnd : (Option MOX#Line-End-Property) #false]
   [extLst : (Option MOX:Office-Art-Extension-List) #false]))
