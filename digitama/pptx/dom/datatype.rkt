#lang typed/racket/base

(provide (all-defined-out))

(require sgml/digitama/plain/datatype)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-xml-enumeration pptx-conformance-class : PPTX-Conformance-Class [strict transitional])
(define-xml-enumeration pptx-slide-size-type : PPTX-Slide-Size-Type
  [screen4x3 letter A4 35mm overhead banner custom ledger A3 B4ISO B5ISO B4JIS B5JIS hagakiCard screen16x9 screen16x10])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox:attr-value->slide-master-id : (XML-Attribute-Value->Datum (Option Nonnegative-Fixnum))
  (lambda [v]
    (xml:attr-value+>fixnum v (assert 2147483648 index?))))

(define mox:attr-value->slide-id : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v 256 (assert 2147483647 index?))))

(define mox:attr-value->slide-coordinate : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v 914400 51206400)))
