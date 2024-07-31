#lang typed/racket/base

(provide (all-defined-out))

(require digimon/digitama/predicate)
(require digimon/dimension)

(require "../../../dialect.rkt")
(require "../../../shared/ml/common-simple-types.rkt")

(require "base.rkt")
(require "fill.rkt")
(require "hyperlink.rkt")
(require "geometry.rkt")
(require "extension.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-Text-Point (U Fixnum XML-Dimension))
(define-type MOX-Text-Run (U MOX:Text-Run MOX:Text-Line-Break MOX:Text-Field))
(define-type MOX-Text-Autofit (U 'none 'shape MOX#Text-Autofit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-xml-enumeration text-align-type : Text-Align-Type #:for mox [l ctr r just justLow dist thaiDist])
(define-xml-enumeration text-font-align-type : Text-Font-Align-Type #:for mox [auto t ctr base b])
(define-xml-enumeration text-strike-type : Text-Strike-Type #:for mox [noStrike sngStrike dblStrike])
(define-xml-enumeration text-caps-type : Text-Caps-Type #:for mox [none small all])
(define-xml-enumeration text-underline-type : Text-Underline-Type #:for mox
  [none words sng dbl heavy dotted dottedHeavy dash dashHeavy dashLong dashLongHeavy
        dotDash dotDashHeavy dotDotDash dotDotDashHeavy wavy wavyHeavy wavyDbl])

(define-xml-enumeration text-anchoring-type : Text-Anchoring-Type #:for mox [t ctr b just dist])
(define-xml-enumeration text-vert-overflow-type : Text-Vert-Overflow-Type #:for mox [overflow ellipsis clip])
(define-xml-enumeration text-horz-overflow-type : Text-Horz-Overflow-Type #:for mox [overflow clip])
(define-xml-enumeration text-wrapping-type : Text-Wrapping-Type #:for mox [square none])
(define-xml-enumeration text-vertical-type : Text-Vertical-Type #:for mox
  [horz vert vert270 wordArtVer eaVert mongolianVert wordArtVertRtl])

(define-xml-enumeration text-shape-type : Text-Shape-Type #:for mox
  [textNoShape textPlain textStop textTriangle textTriangleInverted textChevron textChevronInverted
               textRingInside textRingOutside textArchUp textArchDown textCircle textButton
               textArchUpPour textArchDownPour textCirclePour textButtonPour textCurveUp
               textCurveDown textCanUp textCanDown textWave1 textWave2 textDoubleWave1 textWave4
               textInflate textDeflate textInflateBottom textDeflateBottom textInflateTop
               textDeflateTop textDeflateInflate textDeflateInflateDeflate textFadeRight
               textFadeLeft textFadeUp textFadeDown textSlantUp textSlantDown textCascadeUp
               textCascadeDown])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox:attr-value->text-point : (XML-Attribute-Value->Datum (Option MOX-Text-Point))
  (lambda [v]
    (or (xml:attr-value->fixnum v -400000 400000)
        (mox:attr-value->universal-measure v))))

(define mox:attr-value->text-unqualified-point : (XML-Attribute-Value->Datum (Option Fixnum))
  (lambda [v]
    (xml:attr-value->fixnum v -400000 400000)))

(define mox:attr-value->text-margin : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v 0 #x30d5900 #;51206400)))

(define mox:attr-value->text-indent : (XML-Attribute-Value->Datum (Option Fixnum))
  (lambda [v]
    (xml:attr-value->fixnum v #x-30d5900 #x30d5900)))

(define mox:attr-value->text-column-count : (XML-Attribute-Value->Datum (Option Positive-Byte))
  (lambda [v]
    (xml:attr-value->number v positive-byte? 1 16)))

(define mox:attr-value->text-indent-level : (XML-Attribute-Value->Datum (Option Byte))
  (lambda [v]
    (xml:attr-value->byte v 0 8)))

(define mox:attr-value->text-font-size : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v 100 400000)))

(define mox:attr-value->text-nonnegative-font-size : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v 0 400000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-attribute text-autofit #:for mox
  ([fontScale : XML-Percentage #:= [#false 100%] #:<-> xml:attr-value->percentage]
   [lnSpcReduction : XML-Percentage #:= [#false 0%] #:<-> xml:attr-value->percentage]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-element text-character-property #:for mox
  #:attlist
  ([kumimoji : XML-Boolean #:= #false #:<-> xml:attr-value->boolean]
   [atlLang : String #:= #false #:<-> mox:attr-value->lang]
   [lang : String #:= #false #:<-> mox:attr-value->lang]
   [b : XML-Boolean #:= #false #:<-> xml:attr-value->boolean]
   [i : XML-Boolean #:= #false #:<-> xml:attr-value->boolean]
   [u : Text-Underline-Type #:= #false #:<-> mox:attr-value->text-underline-type]
   [strike : Text-Strike-Type #:= #false #:<-> mox:attr-value->text-strike-type]
   [cap : Text-Caps-Type #:= #false #:<-> mox:attr-value->text-caps-type]
   [sz : Index #:= #false #:<-> mox:attr-value->text-font-size]
   [kern : Index #:= #false #:<-> mox:attr-value->text-nonnegative-font-size]
   [spc : MOX-Text-Point #:= #false #:<-> mox:attr-value->text-point]
   [baseline : XML-Percentage #:= #false #:<-> xml:attr-value->percentage]
   [normalizeH : XML-Boolean #:= #false #:<-> xml:attr-value->boolean]
   [noProof : XML-Boolean #:= #false #:<-> xml:attr-value->boolean]
   [dirty : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean]
   [err : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [smtClean : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean]
   [smtId : Index #:= [#false 0] #:<-> xml:attr-value->index]
   [bmk : String #:= #false #:<-> xml:attr-value->string])
  ([fill : (Option MOX-Fill-Property) #false]
   [hlinkClick : (Option MOX:Hyperlink) #false]
   [hlinkMouseOver : (Option MOX:Hyperlink) #false]
   [extLst : (Option MOX:Office-Art-Extension-List) #false]))

(define-mox-element text-paragraph-property #:for mox
  #:attlist
  ([marL : Index #:= #false #:<-> mox:attr-value->text-margin]
   [marR : Index #:= #false #:<-> mox:attr-value->text-margin]
   [lvl : Byte #:= #false #:<-> mox:attr-value->text-indent-level]
   [indent : Fixnum #:= #false #:<-> mox:attr-value->text-indent]
   [algn : Text-Align-Type #:= #false #:<-> mox:attr-value->text-align-type]
   [defTabSz : MOX-Coordinate32 #:= #false #:<-> mox:attr-value->coordinate32]
   [rtl : XML-Boolean #:= #false #:<-> xml:attr-value->boolean]
   [eaLnBrk : XML-Boolean #:= #false #:<-> xml:attr-value->boolean]
   [fontAlgn : Text-Font-Align-Type #:= #false #:<-> mox:attr-value->text-font-align-type]
   [latinLnBrk : XML-Boolean #:= #false #:<-> xml:attr-value->boolean]
   [hangingPunct : XML-Boolean #:= #false #:<-> xml:attr-value->boolean])
  ([defRPr : (Option MOX:Text-Character-Property) #false]
   [extLst : (Option MOX:Office-Art-Extension-List) #false]))

(define-mox-element text-list-style #:for mox
  ([pPrs : (Option (Immutable-HashTable Symbol MOX:Text-Paragraph-Property)) #false]
   [extLst : (Option MOX:Office-Art-Extension-List) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-element preset-text-shape #:for mox
  #:attlist
  ([prst : Text-Shape-Type #:<-> mox:attr-value->text-shape-type])
  ([avLst : (Listof MOX#Geometry-Guide) null]))

(define-mox-element text-body-property #:for mox
  #:attlist
  ([rot : Fixnum #:= #false #:<-> mox:attr-value->angle]
   [spcFirstLastPara : XML-Boolean #:= #false #:<-> xml:attr-value->boolean]
   [vertOverflow : Text-Vert-Overflow-Type #:= #false #:<-> mox:attr-value->text-vert-overflow-type]
   [horzOverflow : Text-Horz-Overflow-Type #:= #false #:<-> mox:attr-value->text-horz-overflow-type]
   [vert : Text-Vertical-Type #:= #false #:<-> mox:attr-value->text-vertical-type]
   [wrap : Text-Wrapping-Type #:= #false #:<-> mox:attr-value->text-wrapping-type]
   [lIns : MOX-Coordinate32 #:= #false #:<-> mox:attr-value->coordinate32]
   [tIns : MOX-Coordinate32 #:= #false #:<-> mox:attr-value->coordinate32]
   [rIns : MOX-Coordinate32 #:= #false #:<-> mox:attr-value->coordinate32]
   [bIns : MOX-Coordinate32 #:= #false #:<-> mox:attr-value->coordinate32]
   [numCol : Positive-Byte #:= #false #:<-> mox:attr-value->text-column-count]
   [spcCol : Index #:= #false #:<-> mox:attr-value+>coordinate32]
   [rtlCol : XML-Boolean #:= #false #:<-> xml:attr-value->boolean]
   [fromWordArt : XML-Boolean #:= #false #:<-> xml:attr-value->boolean]
   [anchor : Text-Anchoring-Type #:= #false #:<-> mox:attr-value->text-anchoring-type]
   [anchorCtr : XML-Boolean #:= #false #:<-> xml:attr-value->boolean]
   [forceAA : XML-Boolean #:= #false #:<-> xml:attr-value->boolean]
   [upright : XML-Boolean #:= #false #:<-> xml:attr-value->boolean]
   [compatLnSpc : XML-Boolean #:= #false #:<-> xml:attr-value->boolean])
  ([prstTxWarp : (Option MOX:Preset-Text-Shape) #false]
   [autofit : (Option MOX-Text-Autofit) #false]
   [extLst : (Option MOX:Office-Art-Extension-List) #false]))

(define-mox-element text-paragraph #:for mox
  ([pPr : (Option MOX:Text-Paragraph-Property) #false]
   [r* : (Listof MOX-Text-Run) null]
   [endParaRPr : (Option MOX:Text-Character-Property) #false]))

(define-mox-element text-body #:for mox
  ([bodyPr : MOX:Text-Body-Property]
   [lstStyle : (Option MOX:Text-List-Style) #false]
   [p+ : (Pairof MOX:Text-Paragraph (Listof MOX:Text-Paragraph))]))

(define-mox-element text-run #:for mox
  ([rPr : (Option MOX:Text-Character-Property) #false]
   [t : String]))

(define-mox-element text-line-break #:for mox
  ([rPr : (Option MOX:Text-Character-Property) #false]))

(define-mox-element text-field #:for mox
  #:attlist
  ([id : String #:<-> xml:attr-value->guid-string]
   [type : String #:= #false #:<-> xml:attr-value->string])
  ([rPr : (Option MOX:Text-Character-Property) #false]
   [pPr : (Option MOX:Text-Paragraph-Property) #false]
   [t : (Option String) #false]))
