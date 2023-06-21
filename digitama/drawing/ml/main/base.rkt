#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [xml:attr-value->index xml:attr-value->drawing-element-id]
                     [xml:attr-value->index xml:attr-value->style-matrix-column-index]
                     [xml:attr-value->index xml:attr-value+>coordinate32]))

(require "../../../dialect.rkt")
(require "../../../shared/ml/common-simple-types.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-Coordinate (U Fixnum XML-Dimension))
(define-type MOX-Coordinate32 (U Fixnum XML-Dimension))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox:attr-value->coordinate32 : (XML-Attribute-Value->Datum (Option MOX-Coordinate32))
  (lambda [v]
    (or (xml:attr-value->fixnum v (assert #x-7FFFFFFF fixnum?) (assert #x7FFFFFFF fixnum?))
        (mox:attr-value->universal-measure v))))

(define mox:attr-value->coordinate : (XML-Attribute-Value->Datum (Option MOX-Coordinate))
  (lambda [v]
    (or (xml:attr-value->fixnum v (assert #x-18cdffffce64 fixnum?) (assert #x18cdffffce64 fixnum?))
        (mox:attr-value->universal-measure v))))

(define mox:attr-value+>coordinate32 : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v 0)))

(define mox:attr-value+>coordinate : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v 0 (assert #x18cdffffce64 #;27273042316900 index?))))

(define mox:attr-value->angle : (XML-Attribute-Value->Datum (Option Fixnum))
  (lambda [v]
    (xml:attr-value->fixnum v)))

(define mox:attr-value->fixed-angle : (XML-Attribute-Value->Datum (Option Fixnum))
  (lambda [v]
    (xml:attr-value->fixnum v -5400000 5400000)))

(define mox:attr-value+>fixed-angle : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v 0 21600000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-attribute ratio #:for mox
  ([n : Fixnum #:<-> xml:attr-value->fixnum]
   [d : Fixnum #:<-> xml:attr-value->fixnum]))

(define-mox-attribute point2d #:for mox
  ([x : MOX-Coordinate #:<-> mox:attr-value->coordinate]
   [y : MOX-Coordinate #:<-> mox:attr-value->coordinate]))

(define-mox-attribute positive-size2d #:for mox
  ([cx : Index #:<-> mox:attr-value+>coordinate]
   [cy : Index #:<-> mox:attr-value+>coordinate]))

(define-mox-attribute point3d #:for mox
  ([x : MOX-Coordinate #:<-> mox:attr-value->coordinate]
   [y : MOX-Coordinate #:<-> mox:attr-value->coordinate]
   [z : MOX-Coordinate #:<-> mox:attr-value->coordinate]))

(define-mox-attribute vector3d #:for mox
  ([dx : MOX-Coordinate #:<-> mox:attr-value->coordinate]
   [dy : MOX-Coordinate #:<-> mox:attr-value->coordinate]
   [dz : MOX-Coordinate #:<-> mox:attr-value->coordinate]))

(define-mox-attribute sphere-coordicate #:for mox
  ([lat : Index #:<-> mox:attr-value+>fixed-angle]
   [lon : Index #:<-> mox:attr-value+>fixed-angle]
   [rev : Index #:<-> mox:attr-value+>fixed-angle]))
