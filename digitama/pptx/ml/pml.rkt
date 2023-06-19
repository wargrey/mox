#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [mox:attr-value->slide-master-id mox:attr-value->slide-layout-id]))

(require digimon/struct)

(require "../../dialect.rkt")
(require "../../shared/ml/common-simple-types.rkt")
(require "../../drawing/ml/main.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-xml-enumeration pptx-slide-size-type : PPTX-Slide-Size-Type
  [screen4x3 letter A4 35mm overhead banner custom ledger A3 B4ISO B5ISO B4JIS B5JIS hagakiCard screen16x9 screen16x10])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox:attr-value->slide-master-id : (XML-Attribute-Value->Datum (Option Nonnegative-Fixnum))
  (lambda [v]
    (xml:attr-value+>fixnum v (assert #x80000000 #;2147483648 index?))))

(define mox:attr-value->slide-id : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v #x100 (assert #x7fffffff #;2147483647 index?))))

(define mox:attr-value->slide-coordinate : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v #xdf3e0 #;914400 #x30d5900 #;51206400)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-attribute presentation #:for pptx
  ([serverZoom : XML-Percentage #:= [#false (cons 50.0 '%)] #:<-> xml:attr-value->percentage]
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
   [conformance : MOX-Conformance-Class #:= #false #:<-> mox:attr-value->mox-conformance-class]))

(define-mox-attribute slide #:for pptx
  ([showMasterSp : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean]
   [showMasterPhAnim : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean]
   [show : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct* pptx-presentation : PPTX-Presentation
  ([namespaces : MOX-Namespaces null]
   [attlist : (Option PPTX:Attr:Presentation) #false]
   [slide-masters : (Listof PPTX:Attr:Slide-Master-Entry) null]
   [slides : (Listof PPTX:Attr:Slide-Entry) null]
   [slide-size : (Option PPTX:Attr:Slide-Size) #false]
   [notes-size : (Pairof Index Index) (cons 0 0)]
   [default-text-style : (Option MOX-Text-List-Style) #false])
  #:transparent)

(define-struct* pptx-slide-master : PPTX-Slide-Master
  ([namespaces : MOX-Namespaces null]
   [preserve? : XML-Boolean 'false]
   [color-map : MOX-Color-Map default-mox-color-map]
   [layouts : (Listof PPTX:Attr:Slide-Layout-Entry) null])
  #:transparent)

(define-struct* pptx-slide : PPTX-Slide
  ([namespaces : MOX-Namespaces null]
   [attlist : (Option PPTX:Attr:Slide) #false]
   [color-map : (Option MOX-Color-Map-Override) #false])
  #:transparent)
