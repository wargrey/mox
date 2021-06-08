#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-System-Color-Datum (U String (Pairof String Index)))
(define-type MOX-Scheme-Color-Datum (U 'accent1 'accent2 'accent3 'accent4 'accent5 'accent6
                                       'bg1 'bg2 'dk1 'dk2 'lt1 'lt2 'tx1 'tx2 'hlink 'folHlink
                                       'phClr #| use the style color |#))
(define-type MOX-Gradient-Fill-Flip (U 'none 'x 'xy 'y))
(define-type MOX-Gradient-Fill-Angle (U Nonnegative-Flonum (Boxof Nonnegative-Flonum #| with #true as the `scaled` |#)))
(define-type MOX-Gradient-Fill-Path (U 'circle 'rect 'shape))

;; NOTE
; Other color representations, such as hslClr, prstClr, and scrgbClr,
; should be transformed into instances of srgbClr before using

(define-type MOX-Color-Datum (U Index MOX-System-Color-Datum MOX-Color-Transform))
(define-type MOX-Font-Scripts (HashTable Symbol String))

(define-type MOX-Solid-Fill-Datum (U MOX-Color-Datum MOX-Scheme-Color-Datum))
(define-type MOX-Fill-Datum (U False MOX-Solid-Fill-Datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These values are borrowed from the theme named "Facet"

(define-struct mox-color-alter : MOX-Color-Alter
  ([value : Nonnegative-Flonum +nan.0]
   [modulation : Nonnegative-Flonum +nan.0]
   [offset : Nonnegative-Flonum +nan.0])
  #:transparent)

(define-struct mox-color-transform : MOX-Color-Transform
  ([color : MOX-Color-Datum]
   [alpha : MOX-Color-Alter #%mox-intact-color]
   [red : MOX-Color-Alter #%mox-intact-color]
   [green : MOX-Color-Alter #%mox-intact-color]
   [blue : MOX-Color-Alter #%mox-intact-color]
   [hue : MOX-Color-Alter #%mox-intact-color]
   [saturation : MOX-Color-Alter #%mox-intact-color]
   [luminance : MOX-Color-Alter #%mox-intact-color]
   [tint : Nonnegative-Flonum +nan.0]   ; make the color lighter with (1.0 - tint) white
   [shadow : Nonnegative-Flonum +nan.0] ; make the color darker with (1.0 - shadow) black
   [complement? : Boolean #false]
   [grayscale? : Boolean #false]
   [gamma? : Boolean #false]
   [inverse? : Boolean #false])
  #:transparent)

(define-struct mox-color-theme : MOX-Color-Theme
  ; Fundamentals and Markup Language Reference, 20.1.6.2
  ([dark1 : MOX-Color-Datum #%mox-window-text]
   [light1 : MOX-Color-Datum #%mox-window]
   [dark2 : MOX-Color-Datum #x2C3C43]
   [light2 : MOX-Color-Datum #xEBEBEB]

   [accent1 : MOX-Color-Datum #x90c226]
   [accent2 : MOX-Color-Datum #x54A021]
   [accent3 : MOX-Color-Datum #xE6B91E]
   [accent4 : MOX-Color-Datum #xE76618]
   [accent5 : MOX-Color-Datum #xC42F1A]
   [accent6 : MOX-Color-Datum #x918655]

   [hyperlink : MOX-Color-Datum #x99CA3C]
   [visited-link : MOX-Color-Datum #xB9D181])
  #:transparent)

(define-struct mox-font-theme : MOX-Font-Theme
  ; Fundamentals and Markup Language Reference, 20.1.4.1.24/25
  ([latin : String "Arial Black"]
   [asian : String ""]
   [complex-script : String ""]
   [extension : MOX-Font-Scripts #%mox-fonts])
  #:transparent)

(define-struct mox-gradient-fill : MOX-Gradient-Fill
  ([flip : MOX-Gradient-Fill-Flip 'none]
   [rotation : Boolean #true]
   [stops : (Listof (Pairof Nonnegative-Flonum MOX-Color-Datum))]
   [style : (U MOX-Gradient-Fill-Angle MOX-Gradient-Fill-Path)])
  #:transparent)

(define-struct mox-fill-style : MOX-Fill-Style
  ; Fundamentals and Markup Language Reference, 20.1.4.1.13
  ([subtle : MOX-Fill-Datum 'phClr]
   [moderate : MOX-Fill-Datum #false]
   [intense : MOX-Fill-Datum #false])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct mox-theme : MOX-Theme
  ([name : String]
   [color : MOX-Color-Theme]
   [major-font : MOX-Font-Theme] ; a.k.a heading font
   [minor-font : MOX-Font-Theme] ; a.k.a body font
   [fill-style : MOX-Fill-Style]
   [bg-fill-style : MOX-Fill-Style])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #%mox-window-text : MOX-Color-Datum (cons "windowText" #x000000))
(define #%mox-window : MOX-Color-Datum (cons "window" #xFFFFFF))

(define #%mox-intact-color : MOX-Color-Alter (make-mox-color-alter))

(define #%mox-fonts : MOX-Font-Scripts
  (make-hasheq '((Jpan . "MS ゴシック")
                 (Hang . "굴림")
                 (Hans . "微软雅黑")
                 (Hant . "微軟正黑體")
                 (Arab . "Tahoma")
                 (Hebr . "Tahoma")
                 (Thai . "FreesiaUPC")
                 (Ethi . "Nyala")
                 (Beng . "Vrinda")
                 (Gujr . "Shruti")
                 (Khmr . "DaunPenh")
                 (Knda . "Tunga")
                 (Guru . "Raavi")
                 (Cans . "Euphemia")
                 (Cher . "Plantagenet Cherokee")
                 (Yiii . "Microsoft Yi Baiti")
                 (Tibt . "Microsoft Himalaya")
                 (Thaa . "MV Boli")
                 (Deva . "Mangal")
                 (Telu . "Gautami")
                 (Taml . "Latha")
                 (Syrc . "Estrangelo Edessa")
                 (Orya . "Kalinga")
                 (Mlym . "Kartika")
                 (Laoo . "DokChampa")
                 (Sinh . "Iskoola Pota")
                 (Mong . "Mongolian Baiti")
                 (Viet . "Verdana")
                 (Uigh . "Microsoft Uighur")
                 (Geor . "Sylfaen"))))
