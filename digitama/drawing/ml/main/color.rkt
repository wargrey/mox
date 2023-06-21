#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require "../../../dialect.rkt")
(require "../../../shared/ml/common-simple-types.rkt")

(require "extension.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-Color-Map-Override (U True MOX:Color-Map))

(struct mox-color-attribute () #:type-name MOX-Color-Attribute #:transparent)
(struct mox-color-transform-attribute () #:type-name MOX-Color-Transform-Attribute #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-xml-enumeration color-scheme-index : Color-Scheme-Index #:for mox
  [dk1 lt1 dk2 lt2 accent1 accent2 accent3 accent4 accent5 accent6 hlink folHlink])

(define-xml-enumeration system-color-name : System-Color-Name #:for mox
  [scrollBar background activeCaption inactiveCaption menu window windowFrame menuText windowText
             captionText activeBorder inactiveBorder appWorkspace highlight highlightText btnFace
             btnShadow grayText btnText inactiveCaptionText btnHighlight 3dDkShadow 3dLight infoText
             infoBk hotLight gradientActiveCaption gradientInactiveCaption menuHighlight menuBar])

(define-xml-enumeration scheme-color-name : Scheme-Color-Name #:for mox
  [bg1 tx1 bg2 tx2 dk1 lt1 dk2 lt2 accent1 accent2 accent3 accent4 accent5 accent6 hlink folHlink phClr])

(define-xml-enumeration preset-color-name : Preset-Color-Name #:for mox
  [aliceBlue antiqueWhite aqua aquamarine azure beige bisque black blanchedAlmond blue blueViolet
             brown burlyWood cadetBlue chartreuse chocolate coral cornflowerBlue cornsilk crimson
             cyan darkBlue darkCyan darkGoldenrod darkGray darkGrey darkGreen darkKhaki darkMagenta
             darkOliveGreen darkOrange darkOrchid darkRed darkSalmon darkSeaGreen darkSlateBlue
             darkSlateGray darkSlateGrey darkTurquoise darkViolet dkBlue dkCyan dkGoldenrod dkGray
             dkGrey dkGreen dkKhaki dkMagenta dkOliveGreen dkOrange dkOrchid dkRed dkSalmon dkSeaGreen
             dkSlateBlue dkSlateGray dkSlateGrey dkTurquoise dkViolet deepPink deepSkyBlue dimGray dimGrey
             dodgerBlue firebrick floralWhite forestGreen fuchsia gainsboro ghostWhite gold goldenrod
             gray grey green greenYellow honeydew hotPink indianRed indigo ivory khaki lavender lavenderBlush
             lawnGreen lemonChiffon lightBlue lightCoral lightCyan lightGoldenrodYellow lightGray lightGrey
             lightGreen lightPink lightSalmon lightSeaGreen lightSkyBlue lightSlateGray lightSlateGrey
             lightSteelBlue lightYellow ltBlue ltCoral ltCyan ltGoldenrodYellow ltGray ltGrey ltGreen
             ltPink ltSalmon ltSeaGreen ltSkyBlue ltSlateGray ltSlateGrey ltSteelBlue ltYellow lime
             limeGreen linen magenta maroon medAquamarine medBlue medOrchid medPurple medSeaGreen
             medSlateBlue medSpringGreen medTurquoise medVioletRed mediumAquamarine mediumBlue
             mediumOrchid mediumPurple mediumSeaGreen mediumSlateBlue mediumSpringGreen mediumTurquoise
             mediumVioletRed midnightBlue mintCream mistyRose moccasin navajoWhite navy oldLace olive
             oliveDrab orange orangeRed orchid paleGoldenrod paleGreen paleTurquoise paleVioletRed
             papayaWhip peachPuff peru pink plum powderBlue purple red rosyBrown royalBlue saddleBrown
             salmon sandyBrown seaGreen seaShell sienna silver skyBlue slateBlue slateGray slateGrey
             snow springGreen steelBlue tan teal thistle tomato turquoise violet wheat white whiteSmoke
             yellow yellowGreen])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox:attr-value->angle : (XML-Attribute-Value->Datum (Option Fixnum))
  (lambda [v]
    (xml:attr-value->fixnum v)))

(define mox:attr-value->fixed-angle : (XML-Attribute-Value->Datum (Option Fixnum))
  (lambda [v]
    (xml:attr-value->fixnum v -5400000 5400000)))

(define mox:attr-value+>fixed-angle : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v 0 21600000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-attribute scrgb-color #:for mox #:-> mox-color-attribute
  ([r : XML-Nonnegative-Percentage #:<-> xml:attr-value+>fixed-percentage]
   [g : XML-Nonnegative-Percentage #:<-> xml:attr-value+>fixed-percentage]
   [b : XML-Nonnegative-Percentage #:<-> xml:attr-value+>fixed-percentage]))

(define-mox-attribute srgb-color #:for mox #:-> mox-color-attribute
  ([val : Index #:<-> mox:attr-value->hex-rgb-color]))

(define-mox-attribute hsl-color #:for mox #:-> mox-color-attribute
  ([hue : Index #:<-> mox:attr-value+>fixed-angle]
   [sat : XML-Nonnegative-Percentage #:<-> xml:attr-value+>fixed-percentage]
   [lum : XML-Nonnegative-Percentage #:<-> xml:attr-value+>fixed-percentage]))

(define-mox-attribute system-color #:for mox #:-> mox-color-attribute
  ([val : System-Color-Name #:<-> mox:attr-value->system-color-name]
   [lastClr : Index #:= #false #:<-> mox:attr-value->hex-rgb-color]))

(define-mox-attribute scheme-color #:for mox #:-> mox-color-attribute
  ([val : Scheme-Color-Name #:<-> mox:attr-value->scheme-color-name]))

(define-mox-attribute preset-color #:for mox #:-> mox-color-attribute
  ([val : Preset-Color-Name #:<-> mox:attr-value->preset-color-name]))

(define-mox-attribute tint #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Nonnegative-Percentage #:<-> xml:attr-value+>fixed-percentage]))
  
(define-mox-attribute shade #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Nonnegative-Percentage #:<-> xml:attr-value+>fixed-percentage]))

(define-mox-attribute alpha #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Nonnegative-Percentage #:<-> xml:attr-value+>fixed-percentage]))

(define-mox-attribute alphaOff #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Percentage #:<-> xml:attr-value->fixed-percentage]))

(define-mox-attribute alphaMod #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Nonnegative-Percentage #:<-> xml:attr-value+>percentage]))

(define-mox-attribute hue #:for mox #:-> mox-color-transform-attribute
  ([val : Fixnum #:<-> mox:attr-value->fixed-angle]))

(define-mox-attribute hueOff #:for mox #:-> mox-color-transform-attribute
  ([val : Fixnum #:<-> mox:attr-value->angle]))

(define-mox-attribute hueMod #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Nonnegative-Percentage #:<-> xml:attr-value+>percentage]))

(define-mox-attribute sat #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Percentage #:<-> xml:attr-value->percentage]))

(define-mox-attribute satOff #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Percentage #:<-> xml:attr-value->percentage]))

(define-mox-attribute satMod #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Percentage #:<-> xml:attr-value->percentage]))

(define-mox-attribute lum #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Percentage #:<-> xml:attr-value->percentage]))

(define-mox-attribute lumOff #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Percentage #:<-> xml:attr-value->percentage]))

(define-mox-attribute lumMod #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Percentage #:<-> xml:attr-value->percentage]))

(define-mox-attribute red #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Percentage #:<-> xml:attr-value->percentage]))

(define-mox-attribute redOff #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Percentage #:<-> xml:attr-value->percentage]))

(define-mox-attribute redMod #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Percentage #:<-> xml:attr-value->percentage]))

(define-mox-attribute green #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Percentage #:<-> xml:attr-value->percentage]))

(define-mox-attribute greenOff #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Percentage #:<-> xml:attr-value->percentage]))

(define-mox-attribute greenMod #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Percentage #:<-> xml:attr-value->percentage]))

(define-mox-attribute blue #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Percentage #:<-> xml:attr-value->percentage]))

(define-mox-attribute blueOff #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Percentage #:<-> xml:attr-value->percentage]))

(define-mox-attribute blueMod #:for mox #:-> mox-color-transform-attribute
  ([val : XML-Percentage #:<-> xml:attr-value->percentage]))

(define-mox-attribute gray #:for mox #:-> mox-color-transform-attribute ())
(define-mox-attribute comp #:for mox #:-> mox-color-transform-attribute ())
(define-mox-attribute inv #:for mox #:-> mox-color-transform-attribute ())
(define-mox-attribute gamma #:for mox #:-> mox-color-transform-attribute ())
(define-mox-attribute invGamma #:for mox #:-> mox-color-transform-attribute ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-element color-map #:for mox
  #:attlist
  ([bg1 : Color-Scheme-Index #:= [] #:<-> mox:attr-value->color-scheme-index]
   [tx1 : Color-Scheme-Index #:= [] #:<-> mox:attr-value->color-scheme-index]
   [bg2 : Color-Scheme-Index #:= [] #:<-> mox:attr-value->color-scheme-index]
   [tx2 : Color-Scheme-Index #:= [] #:<-> mox:attr-value->color-scheme-index]
   [accent1 : Color-Scheme-Index #:= [] #:<-> mox:attr-value->color-scheme-index]
   [accent2 : Color-Scheme-Index #:= [] #:<-> mox:attr-value->color-scheme-index]
   [accent3 : Color-Scheme-Index #:= [] #:<-> mox:attr-value->color-scheme-index]
   [accent4 : Color-Scheme-Index #:= [] #:<-> mox:attr-value->color-scheme-index]
   [accent5 : Color-Scheme-Index #:= [] #:<-> mox:attr-value->color-scheme-index]
   [accent6 : Color-Scheme-Index #:= [] #:<-> mox:attr-value->color-scheme-index]
   [hlink : Color-Scheme-Index #:= [] #:<-> mox:attr-value->color-scheme-index]
   [folHlink : Color-Scheme-Index #:= [] #:<-> mox:attr-value->color-scheme-index])
  ([extension : (Option MOX:Office-Art-Extension-List) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE
; This is not a concrete element.
; By not defining structs for each color elements that only differ in attributes
;   with one additional field for transformations.
; I provide this struct as an abstract and virtual color element.
(define-struct mox-color : MOX-Color
  ([attlist : MOX-Color-Attribute]
   [transforms : (Listof MOX-Color-Transform-Attribute) null])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-mox-color-map : MOX:Color-Map
  (make-mox:color-map #:attlist
                      (make-mox#color-map #:bg1 'lt1 #:tx1 'dk1 #:bg2 'lt2 #:tx2 'dk2
                                          #:accent1 'accent1 #:accent2 'accent2 #:accent3 'accent3
                                          #:accent4 'accent4 #:accent5 'accent5 #:accent6 'accent6
                                          #:hlink 'hlink #:folHlink 'folHlink)))
