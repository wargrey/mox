#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [mox:attr-value->slide-master-id mox:attr-value->slide-layout-id]))

(require "../../dialect.rkt")
(require "../../shared/ml/common-simple-types.rkt")
(require "../../drawing/ml/main.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-attribute presentation #:for pptx
  ([serverZoom : Flonum #:= [#false 0.5] #:<-> xml:attr-value->flonum]
   [firstSlideNum : Index #:= [#false 1] #:<-> xml:attr-value->index]
   [showSpecialPlsOnTitleSld : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean]
   [rtl : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [removePersonalInfoOnSave : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [compatMode : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [strictFirstAndLastChars : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean]
   [embedTrueTypeFonts : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [saveSubsetFonts : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [autoCompressPictures : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean]
   [bookmarkIdSeed : Index #:= [#false 1] #:<-> xml:attr-value->index]
   [conformance : MOX-Conformance-Class #:= #false #:<-> xml:attr-value->mox-conformance-class]))

(define-mox-attribute slide-master-entry #:for pptx
  ([id : Natural #:= [] #:<-> mox:attr-value->slide-master-id]
   [r:id : Symbol #:= [] #:<-> mox:attr-value->relationship-id]))

(define-mox-attribute slide-entry #:for pptx
  ([id : Natural #:= [] #:<-> mox:attr-value->slide-id]
   [r:id : Symbol #:= [] #:<-> mox:attr-value->relationship-id]))

(define-mox-attribute slide-layout-entry #:for pptx
  ([id : Natural #:= [] #:<-> mox:attr-value->slide-master-id]
   [r:id : Symbol #:= [] #:<-> mox:attr-value->relationship-id]))

(define-mox-attribute slide-size #:for pptx
  ([cx : Index #:<-> mox:attr-value->slide-coordinate]
   [cy : Index #:<-> mox:attr-value->slide-coordinate]
   [type : PPTX-Slide-Size-Type #:= [#false 'custom] #:<-> xml:attr-value->pptx-slide-size-type]))
