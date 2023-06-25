#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [mox:attr-value->slide-master-id mox:attr-value->slide-layout-id]))

(require "../../dialect.rkt")
(require "../../shared/ml/common-simple-types.rkt")
(require "../../drawing/ml/main.rkt")

(require "extension.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type PPTX-Background (U PPTX:Background-Property MOX:Style-Matrix-Reference))
(define-type PPTX-Shape (U PPTX:Shape PPTX:Group-Shape PPTX:Picture
                           PPTX:Graphical-Object-Frame PPTX#Content-Part))

(struct (IdPr) pptx-nvisual-property
  ([cNvPr : MOX:Nvisual-Canvas-Property]
   [cNvIdPr : IdPr]
   [nvPr : PPTX:Nvisual-Application-Property])
  #:type-name PPTX-NVisual-Property
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-xml-enumeration direction : Direction #:for pptx
  [horz vert])

(define-xml-enumeration slide-size-type : Slide-Size-Type #:for pptx
  [screen4x3 letter A4 35mm overhead banner custom ledger A3 B4ISO B5ISO
             B4JIS B5JIS hagakiCard screen16x9 screen16x10])

(define-xml-enumeration placeholder-type : Placeholder-Type #:for pptx
  [title body ctrTitle subTitle dt sldNum ftr hdr obj chart tbl clipArt dgm media sldImg pic])

(define-xml-enumeration placeholder-size : Placeholder-Size #:for pptx
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
   [type : Slide-Size-Type #:= [#false 'custom] #:<-> pptx:attr-value->slide-size-type]))

(define-mox-attribute content-part #:for pptx
  ([r:id : Symbol #:<-> mox:attr-value->relationship-id]))

(define-mox-attribute placeholder #:for pptx
  ([type : Placeholder-Type #:= [#false 'obj] #:<-> pptx:attr-value->placeholder-type]
   [orient : Direction #:= [#false 'horz] #:<-> pptx:attr-value->direction]
   [sz : Placeholder-Size #:= [#false 'full] #:<-> pptx:attr-value->placeholder-size]
   [idx : Index #:= [#false 0] #:<-> xml:attr-value->index]
   [hasCustomPromp : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-element background-property #:for pptx
  #:attlist
  ([shadeToTitle : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean])
  ([fill : MOX-Fill-Property]
   [extLst : (Option PPTX:Extension-List) #false]))

(define-mox-element background #:for pptx
  #:attlist
  ([bwMode : Black-White-Mode #:= [#false 'white] #:<-> mox:attr-value->black-white-mode])
  ([bg : PPTX-Background]))

(define-mox-element nvisual-application-property #:for pptx
  #:attlist
  ([isPhoto : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [useDrawn : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean])
  ([ph : (Option (PPTX-Extension-Modify-With (Option PPTX#Placeholder))) #false]
   [media : (Option MOX-Media) #false]
   [extLst : (Option PPTX:Extension-List) #false]))

(define-mox-element shape #:for pptx
  #:attlist
  ([useBgFill : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean])
  ([nvSpPr : (PPTX-NVisual-Property MOX:Nvisual-Shape-Property)]
   [spPr : MOX:Shape-Property]
   [style : (Option MOX:Shape-Style) #false]
   [txBody : (Option MOX:Text-Body) #false]
   [extLst : (Option PPTX:Extension-List-Modify) #false]))

(define-mox-element group-shape #:for pptx
  ([nvGrpSpPr : (PPTX-NVisual-Property MOX:Nvisual-Group-Shape-Property)]
   [grpSpPr : MOX:Group-Shape-Property]
   [children : (Listof PPTX-Shape) null]
   [extLst : (Option PPTX:Extension-List-Modify) #false]))

(define-mox-element graphical-object-frame #:for pptx
  #:attlist
  ([bwMode : Black-White-Mode #:= [#false 'white] #:<-> mox:attr-value->black-white-mode])
  ([nvGraphicFramePr : (PPTX-NVisual-Property MOX:Nvisual-Graphic-Frame-Property)]
   [xfrm : (Option MOX:Transform2d) #false]
   [graphic : MOX:Graphical-Object-Data]
   [extLst : (Option MOX:Office-Art-Extension-List) #false]))

(define-mox-element picture #:for pptx
  ([nvPicPr : (PPTX-NVisual-Property MOX:Nvisual-Picture-Property)]
   [blipFill : MOX:Blip-Fill]
   [spPr : MOX:Shape-Property]
   [style : (Option MOX:Shape-Style) #false]
   [extLst : (Option MOX:Office-Art-Extension-List) #false]))

(define-mox-element connector #:for pptx
  ([nvCxnSpPr : (PPTX-NVisual-Property MOX:Nvisual-Connector-Property)]
   [spPr : MOX:Shape-Property]
   [style : (Option MOX:Shape-Style) #false]
   [extLst : (Option MOX:Office-Art-Extension-List) #false]))

(define-mox-element common-slide-data #:for pptx
  #:attlist
  ([name : String #:= #false #:<-> xml:attr-value->string])
  ([bg : (Option PPTX-Background) #false]
   [spTree : PPTX:Group-Shape]
   [extLst : (Option PPTX:Extension-List) #false]))

(define-mox-element slide-master-text-styles #:for pptx
  ([titleStyle : (Option MOX:Text-List-Style) #false]
   [bodyStyle : (Option MOX:Text-List-Style) #false]
   [otherStyle : (Option MOX:Text-List-Style) #false]
   [extLst : (Option PPTX:Extension-List) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-element #:root presentation #:for pptx
  #:attlist
  ([serverZoom : XML-Percentage #:= [#false 50%] #:<-> xml:attr-value->percentage]
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
  ([sldMasterIdLst : (Listof PPTX#Slide-Master-Entry) null]
   [sldIdLst : (Listof PPTX#Slide-Entry) null]
   [sldSz : (Option PPTX#Slide-Size) #false]
   [notesSz : MOX#Positive-Size2d]
   [defaultTextStyle : (Option MOX:Text-List-Style) #false]
   [extLst : (Option PPTX:Extension-List-Modify) #false]))

(define-mox-element #:root slide-master #:for pptx
  #:attlist
  ([preserve : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean])
  ([cSld : PPTX:Common-Slide-Data]
   [clrMap : MOX:Color-Map]
   [sldLayoutIdLst : (Listof PPTX#Slide-Layout-Entry) null]
   [txStyles : (Option PPTX:Slide-Master-Text-Styles) #false]
   [extLst : (Option PPTX:Extension-List-Modify) #false]))

(define-mox-element #:root slide #:for pptx
  #:attlist
  ([showMasterSp : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean]
   [showMasterPhAnim : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean]
   [show : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean])
  ([cSld : PPTX:Common-Slide-Data]
   [clrMapOvr : (Option MOX-Color-Map-Override) #false]
   [extLst : (Option PPTX:Extension-List-Modify) #false]))
