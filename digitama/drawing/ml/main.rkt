#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "main/color.rkt" "main/fill.rkt" "main/shape.rkt"))
(provide (all-from-out "main/extension.rkt"))

(require digimon/struct)

(require "../../dialect.rkt")
(require "../../shared/ml/common-simple-types.rkt")

(require "main/color.rkt")
(require "main/fill.rkt")
(require "main/shape.rkt")
(require "main/extension.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-Coordinate32 (U Fixnum XML-Dimension))
(define-type MOX-Text-Point (U Fixnum XML-Dimension))

(define-type MOX-Media (U MOX-Audio-CD MOX-Audio-File MOX#Embedded-File))

(define-xml-enumeration text-align-type : Text-Align-Type #:for mox [l ctr r just justLow dist thaiDist])
(define-xml-enumeration text-font-align-type : Text-Font-Align-Type #:for mox [auto t ctr base b])
(define-xml-enumeration text-strike-type : Text-Strike-Type #:for mox [noStrike sngStrike dblStrike])
(define-xml-enumeration text-caps-type : Text-Caps-Type #:for mox [none small all])
(define-xml-enumeration text-underline-type : Text-Underline-Type #:for mox
  [none words sng dbl heavy dotted dottedHeavy dash dashHeavy dashLong dashLongHeavy
        dotDash dotDashHeavy dotDotDash dotDotDashHeavy wavy wavyHeavy wavyDbl])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox:attr-value->coordinate32 : (XML-Attribute-Value->Datum (Option MOX-Coordinate32))
  (lambda [v]
    (or (xml:attr-value->fixnum v (assert #x-7FFFFFFF fixnum?) (assert #x7FFFFFFF fixnum?))
        (mox:attr-value->universal-measure v))))

(define mox:attr-value->text-point : (XML-Attribute-Value->Datum (Option MOX-Text-Point))
  (lambda [v]
    (or (xml:attr-value->fixnum v -400000 400000)
        (mox:attr-value->universal-measure v))))

(define mox:attr-value->coordinate : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v 0 (assert #x18cdffffce64 #;27273042316900 index?))))

(define mox:attr-value->text-unqualified-point : (XML-Attribute-Value->Datum (Option Fixnum))
  (lambda [v]
    (xml:attr-value->fixnum v -400000 400000)))

(define mox:attr-value->text-margin : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v 0 #x30d5900 #;51206400)))

(define mox:attr-value->text-indent : (XML-Attribute-Value->Datum (Option Fixnum))
  (lambda [v]
    (xml:attr-value->fixnum v #x-30d5900 #x30d5900)))

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
(define-mox-attribute positive-2dsize #:for mox
  ([cx : Index #:<-> mox:attr-value->coordinate]
   [cy : Index #:<-> mox:attr-value->coordinate]))

; QuickTime file doesn't have the `contentType` attribute
(define-mox-attribute mime-file #:for mox
  ([r:link : Symbol #:<-> mox:attr-value->relationship-id]
   [contentType : String #:= #false #:<-> xml:attr-value->string]))

; For embedded wav file
(define-mox-attribute embedded-file #:for mox
  ([r:embed : Symbol #:<-> mox:attr-value->relationship-id]
   [name : String #:<-> xml:attr-value->string]))

(define-mox-attribute cd-time #:for mox
  ([track : Byte #:<-> xml:attr-value->byte]
   [time : Index #:= #false #:<-> xml:attr-value->index]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-element style-matrix-reference #:for mox
  #:attlist
  ([idx : Index #:<-> xml:attr-value->style-matrix-column-index])
  ([color : (Option MOX-Color) #false]))

#;(define-struct mox-group-shape-property : MOX-Group-Shape-Property
  ([bw-mode : (Option Black-White-Mode) #false]
   [fill : (Option MOX-Fill-Property) #false]
   [extension : (Option MOX:Office-Art-Extension-List) #false])
  #:transparent)

(define-mox-element hyperlink #:for mox
  #:attlist
  ([r:id : Symbol #:<-> mox:attr-value->relationship-id]
   [invalidUrl : String #:= #false #:<-> xml:attr-value->uri-string]
   [action : String #:= #false #:<-> xml:attr-value->string]
   [tgtFrame : String #:= #false #:<-> xml:attr-value->string]
   [tooltip : String #:= #false #:<-> xml:attr-value->string]
   [history : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean]
   [highlightClick : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [endSnd : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean])
  ([snd : (Option MOX#Embedded-File) #false]
   [extension : (Option MOX:Office-Art-Extension-List) #false]))

(define-mox-element nvisual-drawing-property #:for mox
  #:attlist
  ([id : Index #:<-> xml:attr-value->drawing-element-id]
   [name : String #:<-> xml:attr-value->string]
   [descr : String #:= #false #:<-> xml:attr-value->string]
   [hidden : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [title : String #:= #false #:<-> xml:attr-value->string])
  ([hlinkClick : (Option MOX:Hyperlink) #false]
   [hlinlHover : (Option MOX:Hyperlink) #false]
   [extension : (Option MOX:Office-Art-Extension-List) #false]))

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
  ([extension : (Option MOX:Office-Art-Extension-List) #false]))

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
   [extension : (Option MOX:Office-Art-Extension-List) #false]))

(define-mox-element text-list-style #:for mox
  ([defPPr : (Option MOX:Text-Paragraph-Property) #false]
   [lvl1pPr : (Option MOX:Text-Paragraph-Property) #false]
   [lvl2pPr : (Option MOX:Text-Paragraph-Property) #false]
   [lvl3pPr : (Option MOX:Text-Paragraph-Property) #false]
   [lvl4pPr : (Option MOX:Text-Paragraph-Property) #false]
   [lvl5pPr : (Option MOX:Text-Paragraph-Property) #false]
   [lvl6pPr : (Option MOX:Text-Paragraph-Property) #false]
   [lvl7pPr : (Option MOX:Text-Paragraph-Property) #false]
   [lvl8pPr : (Option MOX:Text-Paragraph-Property) #false]
   [lvl9pPr : (Option MOX:Text-Paragraph-Property) #false]
   [extension : (Option MOX:Office-Art-Extension-List) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct mox-audio-cd : MOX-Audio-CD
  ([st : MOX#Cd-Time]
   [end : MOX#Cd-Time]
   [extension : (Option MOX:Office-Art-Extension-List) #false])
  #:transparent)

(define-struct mox-audio-file : MOX-Audio-File
  ([attlist : MOX#Mime-File]
   [extension : (Option MOX:Office-Art-Extension-List) #false])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-positive-2dsize : MOX#Positive-2Dsize
  (make-mox#positive-2dsize #:cx 0 #:cy 0))
