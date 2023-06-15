#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out sgml/digitama/plain/datatype))

(require sgml/digitama/plain/datatype)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-xml-enumeration mox-conformance-class : MOX-Conformance-Class
  [strict transitional])

(define-xml-enumeration mox-calendar-type : MOX-Calendar-Type
  [none gregorian gregorianUs gregorianMeFrench gregorianArabic hijri hebrew
        taiwan japan thai korea saka gregorianXlitEnglish gregorianXlitFrench])

(define-xml-enumeration mox-vertical-align-run : MOX-Vertical-Align-Run
  [baseline superscript subscript])

(define-xml-enumeration mox-x-align : MOX-X-Align [left center right inside outside])
(define-xml-enumeration mox-y-align : MOX-Y-Align [inline top center bottom inside outside])
