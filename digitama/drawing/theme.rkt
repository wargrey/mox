#lang typed/racket/base

(provide (all-defined-out))

(require css)

(require "../css/datatype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-Percentage (U Nonnegative-Fixnum Nonnegative-Flonum))

(define-type MOX-Scheme-Color-Datum (U 'accent1 'accent2 'accent3 'accent4 'accent5 'accent6
                                       'bg1 'bg2 'dk1 'dk2 'lt1 'lt2 'tx1 'tx2 'hlink 'folHlink
                                       'phClr #| use the style color |#))

;; NOTE
; Other color representations, such as hslClr, prstClr, and scrgbClr,
; should be transformed into instances of srgbClr before using

(define-type MOX-Color-Datum (U Index FlRGBA MOX-System-Color-Datum))
(define-type MOX-Color-Datum2 (U MOX-Color-Datum MOX-Scheme-Color-Datum))

(define-type MOX-Gradient-Fill-Flip (U 'none 'x 'xy 'y))
(define-type MOX-Gradient-Fill-Angle (U MOX-Percentage (Boxof MOX-Percentage #| with #true as the `scaled` |#)))
(define-type MOX-Gradient-Fill-Path (U 'circle 'rect 'shape))

(define-type MOX-Solid-Fill-Datum (U MOX-Color-Datum2 MOX-Color-Transform))
(define-type MOX-Fill-Datum (U False MOX-Solid-Fill-Datum MOX-Gradient-Fill))

(define-type MOX-Font-Scripts (HashTable Symbol String))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These values are borrowed from the theme named "Facet"

(define-preference mox-color-alter : MOX-Color-Alter
  ([value : Flonum              #:= +nan.0]
   [modulation : MOX-Percentage #:= +nan.0]
   [offset : Flonum             #:= +nan.0])
  #:transparent)

(define-preference mox-color-transform : MOX-Color-Transform
  ([color : MOX-Color-Datum2     #:= 'phClr]
   [alpha : MOX-Color-Alter      #:= #%mox-intact-color]
   [red : MOX-Color-Alter        #:= #%mox-intact-color]
   [green : MOX-Color-Alter      #:= #%mox-intact-color]
   [blue : MOX-Color-Alter       #:= #%mox-intact-color]
   [hue : MOX-Color-Alter        #:= #%mox-intact-color]
   [saturation : MOX-Color-Alter #:= #%mox-intact-color]
   [luminance : MOX-Color-Alter  #:= #%mox-intact-color]
   [tint : MOX-Percentage        #:= +nan.0] ; make the color lighter with (1.0 - tint) white
   [shade : MOX-Percentage       #:= +nan.0] ; make the color darker with (1.0 - shadow) black
   [complement? : Boolean        #:= #false]
   [grayscale? : Boolean         #:= #false]
   [gamma? : Boolean             #:= #false]
   [inverse? : Boolean           #:= #false])
  #:transparent)

(define-preference mox-color-theme : MOX-Color-Theme
  ; Fundamentals and Markup Language Reference, 20.1.6.2
  ([dark1 : MOX-Color-Datum        #:= #%mox-window-text]
   [light1 : MOX-Color-Datum       #:= #%mox-window]
   [dark2 : MOX-Color-Datum        #:= #x2C3C43]
   [light2 : MOX-Color-Datum       #:= #xEBEBEB]

   [accent1 : MOX-Color-Datum      #:= #x90c226]
   [accent2 : MOX-Color-Datum      #:= #x54A021]
   [accent3 : MOX-Color-Datum      #:= #xE6B91E]
   [accent4 : MOX-Color-Datum      #:= #xE76618]
   [accent5 : MOX-Color-Datum      #:= #xC42F1A]
   [accent6 : MOX-Color-Datum      #:= #x918655]

   [hyperlink : MOX-Color-Datum    #:= #x99CA3C]
   [visited-link : MOX-Color-Datum #:= #xB9D181])
  #:transparent)

(define-preference mox-font-theme : MOX-Font-Theme
  ; Fundamentals and Markup Language Reference, 20.1.4.1.24/25
  ([latin : String               #:= "Arial Black"]
   [asian : String               #:= ""]
   [complex-script : String      #:= ""]
   [extension : MOX-Font-Scripts #:= #%mox-fonts])
  #:transparent)

(define-preference mox-gradient-fill : MOX-Gradient-Fill
  ([flip : MOX-Gradient-Fill-Flip #:= 'none]
   [rotation : Boolean            #:= #true]
   [stops : (Listof (Pairof MOX-Percentage MOX-Solid-Fill-Datum)) #:= null]
   [style : (U MOX-Gradient-Fill-Angle MOX-Gradient-Fill-Path)    #:= 0.0])
  #:transparent)

(define-preference mox-fill-style : MOX-Fill-Style
  ; Fundamentals and Markup Language Reference, 20.1.4.1.13
  ([subtle : MOX-Fill-Datum   #:= 'phClr]
   [moderate : MOX-Fill-Datum #:= #%mox-moderate-gradient-fill]
   [intense : MOX-Fill-Datum  #:= #%mox-intense-gradient-fill])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct mox-theme
  ([name : String]
   [color : MOX-Color-Theme]
   [major-font : MOX-Font-Theme] ; a.k.a heading font
   [minor-font : MOX-Font-Theme] ; a.k.a body font
   [fill-style : MOX-Fill-Style]
   [bg-fill-style : MOX-Fill-Style])
  #:type-name MOX-Theme
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~color : CSS-Subject (make-css-subject #:type 'clrScheme))

(define mox-color-ref : (All (c) (-> CSS-Values (Option CSS-Values) Symbol c (U FlRGBA c)))
  (lambda [declared-values inherited-values property def-value]
    (define maybe-color (css-rgba-ref declared-values inherited-values property))

    (cond [(rgba? maybe-color) maybe-color]
          [else def-value])))

(define mox-clrsheme-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (<mox-color>)))

(define mox-clrscheme-filter : (CSS-Cascaded-Value-Filter MOX-Color-Theme)
  (lambda [declared-values inherited-values]
    (current-css-element-color (css-rgba-ref declared-values inherited-values))
    
    (make-mox-color-theme #:dark1 (mox-color-ref declared-values inherited-values 'dk1 (#%mox-color-theme-dark1))
                          #:dark2 (mox-color-ref declared-values inherited-values 'dk2 (#%mox-color-theme-dark2))
                          #:light1 (mox-color-ref declared-values inherited-values 'lt1 (#%mox-color-theme-light1))
                          #:light2 (mox-color-ref declared-values inherited-values 'lt2 (#%mox-color-theme-light2))
                          #:hyperlink (mox-color-ref declared-values inherited-values 'hlink (#%mox-color-theme-hyperlink))
                          #:visited-link (mox-color-ref declared-values inherited-values 'folhlink (#%mox-color-theme-visited-link))
                          
                          #:accent1 (mox-color-ref declared-values inherited-values 'accent1 (#%mox-color-theme-accent1))
                          #:accent2 (mox-color-ref declared-values inherited-values 'accent2 (#%mox-color-theme-accent2))
                          #:accent3 (mox-color-ref declared-values inherited-values 'accent3 (#%mox-color-theme-accent3))
                          #:accent4 (mox-color-ref declared-values inherited-values 'accent4 (#%mox-color-theme-accent4))
                          #:accent5 (mox-color-ref declared-values inherited-values 'accent5 (#%mox-color-theme-accent5))
                          #:accent6 (mox-color-ref declared-values inherited-values 'accent6 (#%mox-color-theme-accent6)))))

(define read-mox-theme-from-css : (-> CSS-StdIn Any)
  (lambda [/dev/cssin]
    (define theme.css : CSS-Stylesheet (read-css-stylesheet /dev/cssin))
    (define-values (clrScheme _) (css-cascade theme.css ~color mox-clrsheme-parsers mox-clrscheme-filter #false))

    clrScheme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #%mox-window-text : MOX-Color-Datum (cons "windowText" #x000000))
(define #%mox-window : MOX-Color-Datum (cons "window" #xFFFFFF))

(define #%mox-intact-color : MOX-Color-Alter (make-mox-color-alter))
(define #%mox-moderate-gradient-stops : (Listof (Pairof MOX-Percentage MOX-Solid-Fill-Datum))
  (list (cons 0 (make-mox-color-transform #:color 'phClr #:tint 65000 #:luminance (make-mox-color-alter #:modulation 110000)))
        (cons 88000 (make-mox-color-transform #:color 'phClr #:tint 90000))))
(define #%mox-intense-gradient-stops : (Listof (Pairof MOX-Percentage MOX-Solid-Fill-Datum))
  (list (cons 0 (make-mox-color-transform #:color 'phClr #:tint 96000 #:luminance (make-mox-color-alter #:modulation 104000)))
        (cons 78000 (make-mox-color-transform #:color 'phClr #:shade 94000 #:luminance (make-mox-color-alter #:modulation 94000)))))

(define #%mox-moderate-gradient-fill : MOX-Gradient-Fill (make-mox-gradient-fill #:stops #%mox-moderate-gradient-stops #:style 5400000))
(define #%mox-intense-gradient-fill : MOX-Gradient-Fill (make-mox-gradient-fill #:stops #%mox-intense-gradient-stops #:style 5400000))

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
