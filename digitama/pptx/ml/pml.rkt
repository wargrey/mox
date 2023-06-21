#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [mox:attr-value->slide-master-id mox:attr-value->slide-layout-id]))

(require "../../dialect.rkt")
(require "../../shared/ml/common-simple-types.rkt")
(require "../../drawing/ml/main.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type PPTX-Background (U PPTX:Background-Property MOX:Style-Matrix-Reference))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-xml-enumeration pptx-direction : PPTX-Direction
  [horz vert])

(define-xml-enumeration pptx-slide-size-type : PPTX-Slide-Size-Type
  [screen4x3 letter A4 35mm overhead banner custom ledger A3 B4ISO B5ISO B4JIS B5JIS hagakiCard screen16x9 screen16x10])

(define-xml-enumeration pptx-placeholder-type : PPTX-Placeholder-Type
  [title body ctrTitle subTitle dt sldNum ftr hdr obj chart tbl clipArt dgm media sldImg pic])

(define-xml-enumeration pptx-placeholder-size : PPTX-Placeholder-Size
  [full half quarter])

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
(define-mox-element extension-list #:for pptx ())
(define-mox-element extension-list-modify #:for pptx ())

(define-mox-element background-property #:for pptx
  #:attlist
  ([shadeToTitle : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean])
  ([fill : MOX-Fill-Property]
   [extension : (Option PPTX:Extension-List) #false]))

(define-mox-element background #:for pptx
  #:attlist
  ([bwMode : Black-White-Mode #:= [#false 'white] #:<-> mox:attr-value->black-white-mode])
  ([choice : PPTX-Background]))

(define-mox-element placeholder #:for pptx
  #:attlist
  ([type : PPTX-Placeholder-Type #:= [#false 'obj] #:<-> xml:attr-value->pptx-placeholder-type]
   [orient : PPTX-Direction #:= [#false 'horz] #:<-> xml:attr-value->pptx-direction]
   [sz : PPTX-Placeholder-Size #:= [#false 'full] #:<-> xml:attr-value->pptx-placeholder-size]
   [idx : Index #:= [#false 0] #:<-> xml:attr-value->index]
   [hasCustomPromp : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean])
  ([extension : (Option PPTX:Extension-List-Modify) #false]))

(define-mox-element nvisual-application-drawing-property #:for pptx
  #:attlist
  ([isPhoto : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [useDrawn : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean])
  ([placeholder : (Option PPTX:Placeholder) #false]
   [media : (Option MOX-Media) #false]
   [extension : (Option PPTX:Extension-List) #false]))

(define-mox-element nvisual-group-shape #:for pptx
  ([drawing-property : MOX:Nvisual-Drawing-Property]
   [shape-property : MOX:Nvisual-Group-Drawing-Shape-Property]
   [application-property : PPTX:Nvisual-Application-Drawing-Property]))

(define-mox-element shape-tree #:for pptx
  ([nvisual-group-shape-property : PPTX:Nvisual-Group-Shape]))

(define-mox-element common-slide-data #:for pptx
  #:attlist
  ([name : String #:= #false #:<-> xml:attr-value->string])
  ([background : (Option PPTX-Background) #false]
   [shape-tree : PPTX:Shape-Tree]
   [extension : (Option PPTX:Extension-List) #false]))

(define-mox-element slide-master-text-styles #:for pptx
  ([title : (Option MOX:Text-List-Style) #false]
   [body : (Option MOX:Text-List-Style) #false]
   [other : (Option MOX:Text-List-Style) #false]
   [extension : (Option PPTX:Extension-List) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-element #:root presentation #:for pptx
  #:attlist
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
   [conformance : MOX-Conformance-Class #:= #false #:<-> mox:attr-value->mox-conformance-class])
  ([slide-masters : (Listof PPTX#Slide-Master-Entry) null]
   [slides : (Listof PPTX#Slide-Entry) null]
   [slide-size : (Option PPTX#Slide-Size) #false]
   [notes-size : MOX#Positive-2Dsize]
   [default-text-style : (Option MOX:Text-List-Style) #false]
   [extension : (Option PPTX:Extension-List-Modify) #false]))

(define-mox-element #:root slide-master #:for pptx
  #:attlist
  ([preserve : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean])
  ([color-map : MOX:Color-Map]
   [layouts : (Listof PPTX#Slide-Layout-Entry) null]
   [styles : (Option PPTX:Slide-Master-Text-Styles) #false]
   [extension : (Option PPTX:Extension-List-Modify) #false]))

(define-mox-element #:root slide #:for pptx
  #:attlist
  ([showMasterSp : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean]
   [showMasterPhAnim : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean]
   [show : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean])
  ([color-map : (Option MOX-Color-Map-Override) #false]
   [extension : (Option PPTX:Extension-List-Modify) #false]))
