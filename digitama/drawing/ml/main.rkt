#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [xml:attr-value->symbol mox:attr-value->relationship-id]))

(require sgml/digitama/plain/grammar)
(require sgml/digitama/plain/dialect)
(require sgml/digitama/plain/datatype)

(require "../../dialect.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-xml-enumeration color-scheme-index : Color-Scheme-Index
  [dk1 lt1 dk2 lt2 accent1 accent2 accent3 accent4 accent5 accent6 hlink folHlink])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-attributes-extract-positive-2dsize : (-> (Listof XML-Element-Attribute) Symbol Symbol
                                                     (Values (Option (Pairof Index Index))
                                                             (Listof XML-Element-Attribute)))
  (lambda [attrs x y]
    (mox-attributes-extract-pair attrs x y mox:attr-value->coordinate mox:attr-value->coordinate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox:attr-value->coordinate : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v 0 (assert 27273042316900 index?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-attribute color-map #:for mox
  ([bg1 : Color-Scheme-Index #:= [] #:<-> xml:attr-value->color-scheme-index]
   [tx1 : Color-Scheme-Index #:= [] #:<-> xml:attr-value->color-scheme-index]
   [bg2 : Color-Scheme-Index #:= [] #:<-> xml:attr-value->color-scheme-index]
   [tx2 : Color-Scheme-Index #:= [] #:<-> xml:attr-value->color-scheme-index]
   [accent1 : Color-Scheme-Index #:= [] #:<-> xml:attr-value->color-scheme-index]
   [accent2 : Color-Scheme-Index #:= [] #:<-> xml:attr-value->color-scheme-index]
   [accent3 : Color-Scheme-Index #:= [] #:<-> xml:attr-value->color-scheme-index]
   [accent4 : Color-Scheme-Index #:= [] #:<-> xml:attr-value->color-scheme-index]
   [accent5 : Color-Scheme-Index #:= [] #:<-> xml:attr-value->color-scheme-index]
   [accent6 : Color-Scheme-Index #:= [] #:<-> xml:attr-value->color-scheme-index]
   [hlink : Color-Scheme-Index #:= [] #:<-> xml:attr-value->color-scheme-index]
   [folHlink : Color-Scheme-Index #:= [] #:<-> xml:attr-value->color-scheme-index]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-mox-color-map : MOX:Attr:Color-Map
  (make-mox:attr:color-map #:bg1 'lt1 #:tx1 'dk1 #:bg2 'lt2 #:tx2 'dk2
                           #:accent1 'accent1 #:accent2 'accent2 #:accent3 'accent3
                           #:accent4 'accent4 #:accent5 'accent5 #:accent6 'accent6
                           #:hlink 'hlink #:folHlink 'folHlink))
