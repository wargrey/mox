#lang typed/racket/base

(provide (all-defined-out))

(require css)

(require racket/symbol)

(require "../css/datatype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-Percentage (U Nonnegative-Fixnum Nonnegative-Flonum))

;; NOTE
; Other color representations, such as hslClr, prstClr, and scrgbClr,
; should be transformed into instances of srgbClr before using

(define-type MOX-Gradient-Fill-Flip (U 'none 'x 'xy 'y))
(define-type MOX-Gradient-Fill-Angle (U MOX-Percentage (Boxof MOX-Percentage #| with #true as the `scaled` |#)))
(define-type MOX-Gradient-Fill-Path (U 'circle 'rect 'shape))

(define-type MOX-Solid-Fill-Datum (U MOX-Color-Datum MOX-Color-Transform))
(define-type MOX-Fill-Datum (U False MOX-Solid-Fill-Datum MOX-Gradient-Fill))

(define-type MOX-Font-Scripts (HashTable Symbol MOX-Font-Datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These values are borrowed from the theme named "Facet"

(define-preference mox-color-alter : MOX-Color-Alter
  ([value : Flonum              #:= +nan.0]
   [modulation : MOX-Percentage #:= +nan.0]
   [offset : Flonum             #:= +nan.0])
  #:transparent)

(define-preference mox-color-transform : MOX-Color-Transform
  ([color : MOX-Color-Datum     #:= 'phClr]
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

(define-preference mox-color-scheme : MOX-Color-Scheme
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

(define-preference mox-font-scheme : MOX-Font-Scheme
  ; Fundamentals and Markup Language Reference, 20.1.4.1.24/25
  ([latin : MOX-Font-Datum          #:= "Arial Black"]
   [east-asian : MOX-Font-Datum     #:= ""]
   [complex-script : MOX-Font-Datum #:= ""]
   [scripts : MOX-Font-Scripts      #:= #%mox-no-scripts])
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
   [color : MOX-Color-Scheme]
   [major-font : MOX-Font-Scheme] ; a.k.a heading font
   [minor-font : MOX-Font-Scheme] ; a.k.a body font
   [fill-style : MOX-Fill-Style]
   [bg-fill-style : MOX-Fill-Style])
  #:type-name MOX-Theme
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~clrScheme : CSS-Subject (make-css-subject #:type 'clrScheme #::classes '(root)))
(define ~headFont : CSS-Subject (make-css-subject #:type 'fontScheme #:id '#:head #::classes '(root)))
(define ~bodyFont : CSS-Subject (make-css-subject #:type 'fontScheme #:id '#:body #::classes '(root)))

(define mox-clrscheme-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (<mox-color>)))

(define mox-fontscheme-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (<mox-font>)))

(define mox-clrscheme-filter : (CSS-Cascaded-Value-Filter MOX-Color-Scheme)
  (lambda [declared-values inherited-values]
    (current-css-element-color (css-rgba-ref declared-values inherited-values))
    
    (make-mox-color-scheme #:dark1 (css-rgba-ref declared-values inherited-values 'dk1)
                           #:dark2 (css-rgba-ref declared-values inherited-values 'dk2)
                           #:light1 (css-rgba-ref declared-values inherited-values 'lt1)
                           #:light2 (css-rgba-ref declared-values inherited-values 'lt2)
                           #:hyperlink (css-rgba-ref declared-values inherited-values 'hlink)
                           #:visited-link (css-rgba-ref declared-values inherited-values 'folhlink)
                           
                           #:accent1 (css-rgba-ref declared-values inherited-values 'accent1)
                           #:accent2 (css-rgba-ref declared-values inherited-values 'accent2)
                           #:accent3 (css-rgba-ref declared-values inherited-values 'accent3)
                           #:accent4 (css-rgba-ref declared-values inherited-values 'accent4)
                           #:accent5 (css-rgba-ref declared-values inherited-values 'accent5)
                           #:accent6 (css-rgba-ref declared-values inherited-values 'accent6))))

(define mox-fontscheme-filter : (CSS-Cascaded-Value-Filter MOX-Font-Scheme)
  (lambda [declared-values inherited-values]
    (define declared-scripts : (Listof Symbol) (remove* '(latin ea cs) (hash-keys declared-values)))
    
    (make-mox-font-scheme #:latin (css-ref declared-values inherited-values 'latin mox-font? (#%mox-font-scheme-latin))
                          #:east-asian (css-ref declared-values inherited-values 'ea mox-font? (#%mox-font-scheme-east-asian))
                          #:complex-script (css-ref declared-values inherited-values 'cs mox-font? (#%mox-font-scheme-complex-script))
                          #:scripts (cond [(null? declared-scripts) #%mox-no-scripts]
                                          [else (for/hasheq : (HashTable Symbol MOX-Font-Datum) ([s (in-list declared-scripts)])
                                                  (values (string->symbol (string-titlecase (symbol->immutable-string s)))
                                                          (css-ref declared-values inherited-values s mox-font? "")))]))))

(define read-mox-theme-from-css : (->* (CSS-StdIn) (Symbol) Any)
  (lambda [/dev/cssin [root-type 'base]]
    (css-root-element-type root-type)

    (define theme.css : CSS-Stylesheet (read-css-stylesheet /dev/cssin))
    (define ~root : CSS-Subject (make-css-subject))
    (define *root : CSS-Values (css-variable-cascade theme.css ~root #false))
    (define-values (clrScheme _clr) (css-cascade theme.css (list ~clrScheme ~root) mox-clrscheme-parsers mox-clrscheme-filter *root))
    (define-values (headFont _maf) (css-cascade theme.css (list ~headFont ~root) mox-fontscheme-parsers mox-fontscheme-filter *root))
    (define-values (bodyFont _mif) (css-cascade theme.css (list ~bodyFont ~root) mox-fontscheme-parsers mox-fontscheme-filter *root))

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

(define #%mox-no-scripts : MOX-Font-Scripts #hasheq())

(define #%mox-scripts : MOX-Font-Scripts
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
