#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out sgml/digitama/xexpr/datatype))
(provide (rename-out [xml:attr-value->string mox:attr-value->lang]
                     [xml:attr-value->symbol mox:attr-value->relationship-id]))

(require sgml/digitama/xexpr/datatype)

(require digimon/dimension)

;;; WARNING: RNC might be wrong

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-xml-enumeration mox-conformance-class : MOX-Conformance-Class #:for mox
  [strict transitional])

(define-xml-enumeration mox-calendar-type : MOX-Calendar-Type #:for mox
  [none gregorian gregorianUs gregorianMeFrench gregorianArabic hijri hebrew
        taiwan japan thai korea saka gregorianXlitEnglish gregorianXlitFrench])

(define-xml-enumeration mox-vertical-align-run : MOX-Vertical-Align-Run #:for mox
  [baseline superscript subscript])

(define-xml-enumeration mox-x-align : MOX-X-Align #:for mox [left center right inside outside])
(define-xml-enumeration mox-y-align : MOX-Y-Align #:for mox [inline top center bottom inside outside])
(define-xml-enumeration mox-measure-unit : MOX-Measure-Unit #:for mox [mm cm in pt pc pi])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox:attr-value->universal-measure : (XML-Attribute-Value->Datum (Option XML-Dimension))
  (lambda [v]
    (define dim (xml:attr-value->dimension v))
    (and dim (mox-measure-unit? (#%dim-unit dim)) dim)))

(define mox:attr-value+>universal-measure : (XML-Attribute-Value->Datum (Option XML-Dimension))
  (lambda [v]
    (define dim (xml:attr-value+>dimension v))
    (and dim (mox-measure-unit? (#%dim-unit dim)) dim)))

(define mox:attr-value->hex-rgb-color : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->hexadecimal v 6)))
