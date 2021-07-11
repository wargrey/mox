#lang typed/racket/base

(provide (all-defined-out))

(require css)
(require css/digitama/image)

(require racket/symbol)

(require "../css/datatype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-Clean-Color-Datum (U FlColor MOX-System-Color-Datum MOX-Scheme-Color MOX-Color-Transform))
(define-type MOX-Fill-Datum (U MOX-Clean-Color-Datum MOX-Gradient-Fill))

(define-type MOX-Font-Scripts (HashTable Symbol MOX-Font-Datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These values are borrowed from the theme named "Facet"
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
  ([datum : CSS-Image                   #:= #%mox-moderate-gradient-fill]
   [tile-rectangle : MOX-Tile-Rectangle #:= (list 'none)]
   [rotate-with-shape : Boolean         #:= #true])
  #:transparent)

(define-preference mox-fill-style : MOX-Fill-Style
  ; Fundamentals and Markup Language Reference, 20.1.4.1.13
  ([subtle : MOX-Fill-Datum   #:= 'phClr]
   [moderate : MOX-Fill-Datum #:= #%mox-moderate-fill]
   [intense : MOX-Fill-Datum  #:= #%mox-intense-fill])
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
(define ~headFont : CSS-Subject (make-css-subject #:type 'fontScheme #:id '#:head))
(define ~bodyFont : CSS-Subject (make-css-subject #:type 'fontScheme #:id '#:body))
(define ~shapeFill : CSS-Subject (make-css-subject #:type 'fillStyle #:id '#:shape))
(define ~backgroundFill : CSS-Subject (make-css-subject #:type 'fillStyle #:id '#:background))

(define mox-clrscheme-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (<mox-color>)))

(define mox-fillstyle-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (case suitcased-name
      [(subtle moderate intense) (<mox-fill-style>)]
      [(tile-rectangle subtle-tile-rectangle moderate-tile-rectangle intense-tile-rectangle) (<:mox-tile-rectangle:>)]
      [(rotate-with-shape subtle-rotate-with-shape moderate-rotate-with-shape intense-rotate-with-shape) (<css-boolean>)])))

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

(define mox-fillstyle-filter : (CSS-Cascaded-Value-Filter MOX-Fill-Style)
  (lambda [declared-values inherited-values]
    (current-css-element-color (css-rgba-ref declared-values inherited-values))

    (make-mox-fill-style #:subtle (css-fillstyle-ref declared-values inherited-values 'subtle)
                         #:moderate (css-fillstyle-ref declared-values inherited-values 'moderate)
                         #:intense (css-fillstyle-ref declared-values inherited-values 'intense))))

(define mox-fontscheme-filter : (CSS-Cascaded-Value-Filter MOX-Font-Scheme)
  (lambda [declared-values inherited-values]
    (define declared-scripts : (Listof Symbol) (remove* '(latin ea cs) (hash-keys declared-values)))
    
    (make-mox-font-scheme #:latin (css-ref declared-values inherited-values 'latin mox-font-datum? (#%mox-font-scheme-latin))
                          #:east-asian (css-ref declared-values inherited-values 'ea mox-font-datum? (#%mox-font-scheme-east-asian))
                          #:complex-script (css-ref declared-values inherited-values 'cs mox-font-datum? (#%mox-font-scheme-complex-script))
                          #:scripts (cond [(null? declared-scripts) #%mox-no-scripts]
                                          [else (for/hasheq : (HashTable Symbol MOX-Font-Datum) ([s (in-list declared-scripts)])
                                                  (values (string->symbol (string-titlecase (symbol->immutable-string s)))
                                                          (css-ref declared-values inherited-values s mox-font-datum? "")))]))))

(define read-mox-theme-from-css : (->* (CSS-StdIn) (Symbol) MOX-Theme)
  (lambda [/dev/cssin [root-type 'base]]
    (css-root-element-type root-type)

    (define theme.css : CSS-Stylesheet (read-css-stylesheet /dev/cssin))
    (define ~root : CSS-Subject (make-css-subject))
    (define *root : CSS-Values (css-variable-cascade theme.css ~root #false))
    (define-values (clrScheme _clr) (css-cascade theme.css (list ~clrScheme ~root) mox-clrscheme-parsers mox-clrscheme-filter *root))
    (define-values (shapeFill _shf) (css-cascade theme.css (list ~shapeFill ~root) mox-fillstyle-parsers mox-fillstyle-filter *root))
    (define-values (backgFill _bgf) (css-cascade theme.css (list ~backgroundFill ~root) mox-fillstyle-parsers mox-fillstyle-filter *root))
    (define-values (headFont _maf) (css-cascade theme.css (list ~headFont ~root) mox-fontscheme-parsers mox-fontscheme-filter *root))
    (define-values (bodyFont _mif) (css-cascade theme.css (list ~bodyFont ~root) mox-fontscheme-parsers mox-fontscheme-filter *root))

    (mox-theme "Facet" clrScheme headFont bodyFont shapeFill backgFill)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css->mox-fillstyle : (CSS->Racket (U MOX-Fill-Datum CSS-Image))
  (lambda [desc-name v]
    (cond [(css-image? v) v]
          [(mox-color-transform? v) v]
          [(mox-scheme-color? v) v]
          [(mox-sysclr? v) v]
          [else (let ([fillstyle (css->color desc-name v)])
                  (cond [(css-wide-keyword? fillstyle) (#%mox-fill-style-subtle)]
                        [(eq? fillstyle 'currentcolor) 'phClr]
                        [else fillstyle]))])))

(define css-fillstyle-ref : (-> CSS-Values (Option CSS-Values) Symbol MOX-Fill-Datum)
  (lambda [declared-values inherited-values property]
    (define fillstyle : (U MOX-Fill-Datum CSS-Image) (css-ref declared-values inherited-values property css->mox-fillstyle))

    (cond [(not (css-image? fillstyle)) fillstyle]
          [else (let ([tileRect (css-fillstyle-setting-ref declared-values inherited-values 'tile-rectangle property)]
                      [rotWithShape (css-fillstyle-setting-ref declared-values inherited-values 'rotate-with-shape property)])
                  (mox-gradient-fill fillstyle
                                     (if (mox-tile-rectangle? tileRect) tileRect (#%mox-gradient-fill-tile-rectangle))
                                     (and rotWithShape #true)))])))

(define css-fillstyle-setting-ref : (-> CSS-Values (Option CSS-Values) Symbol Symbol Any)
  (lambda [declared-values inherited-values property prefix]
    (define prefixed-property (string->symbol (format "~a-~a" prefix property)))
    (define p (css-ref declared-values inherited-values prefixed-property))

    (cond [(not (css-wide-keyword? p)) p]
          [else (let ([v (css-ref declared-values inherited-values property)])
                  (cond [(css-wide-keyword? v) #false]
                        [else v]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #%mox-window-text : MOX-Color-Datum (cons "windowText" #x000000))
(define #%mox-window : MOX-Color-Datum (cons "window" #xFFFFFF))

; untested default values
(define #%mox-moderate-gradient-fill : CSS-Image
  (mox-linear-gradient  (mox-angle 5400000) #false
                        (list (cons (mox-color-transform 'phClr (list (mox-color-component-alteration 'tint (mox+fixed-percentage 65000))
                                                                      (mox-color-component-alteration 'luminance (mox+fixed-percentage 110000))))
                                    (list (mox+fixed-percentage 0)))
                              (cons (mox-color-transform 'phClr (list (mox-color-component-alteration 'tint (mox-fixed-percentage 90000))))
                                    (list (mox+fixed-percentage 88000))))))

(define #%mox-intense-gradient-fill : CSS-Image
  (mox-linear-gradient  (mox-angle 5400000) #false
                        (list (cons (mox-color-transform 'phClr (list (mox-color-component-alteration 'tint (mox+fixed-percentage 96000))
                                                                      (mox-color-component-alteration 'luminance (mox+fixed-percentage 104000))))
                                    (list (mox+fixed-percentage 0)))
                              (cons (mox-color-transform 'phClr (list (mox-color-component-alteration 'shade (mox-fixed-percentage 94000))
                                                                      (mox-color-component-alteration 'luminance (mox+fixed-percentage 94000))))
                                    (list (mox+fixed-percentage 78000))))))

(define #%mox-moderate-fill : MOX-Gradient-Fill (make-mox-gradient-fill #:datum #%mox-moderate-gradient-fill))
(define #%mox-intense-fill : MOX-Gradient-Fill (make-mox-gradient-fill #:datum #%mox-intense-gradient-fill))

(define #%mox-no-scripts : MOX-Font-Scripts #hasheq())
