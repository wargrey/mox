#lang typed/racket/base

(provide (all-defined-out))

(require css)
(require css/digitama/image)

(require digimon/symbol)

(require "../css/datatype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-Clean-Color-Datum (U FlColor MOX-System-Color-Datum MOX-Scheme-Color MOX-Color-Transform))
(define-type MOX-Fill-Datum (U MOX-Clean-Color-Datum MOX-Special-Fill MOX-Gradient-Fill))

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
  ; Fundamentals and Markup Language Reference, 20.1.4.1.13
  ([datum : CSS-Image                   #:= #%mox-gradient-fill]
   [tile-rectangle : CSS-Region         #:= css-full-region]
   [flip : Symbol                       #:= 'none]
   [rotate-with-shape : CSS-Boolean     #:= 1])
  #:transparent)

(define-preference mox-line : MOX-Line
  ; Fundamentals and Markup Language Reference, 20.1.4.1.21
  ([fill : MOX-Fill-Datum      #:= 'phClr]
   [align : Symbol             #:= 'ctr]
   [cap : Symbol               #:= 'rnd]
   [compound : Symbol          #:= 'sng]
   [width : Nonnegative-Flonum #:= 0.0]
   [join : MOX-Line-Join-Datum #:= 'none]
   [dash : MOX-Line-Dash-Datum #:= 'solid]
   [head : (Listof Symbol)     #:= '(none)]
   [tail : (Listof Symbol)     #:= '(none)])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct mox-theme
  ([name : String]
   [color : MOX-Color-Scheme]
   [major-font : MOX-Font-Scheme] ; a.k.a heading font
   [minor-font : MOX-Font-Scheme] ; a.k.a body font
   [fill-styles : (Listof MOX-Fill-Datum)]
   [bg-fill-styles : (Listof MOX-Fill-Datum)]
   [line-styles : (Listof MOX-Line)])
  #:type-name MOX-Theme
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~clrScheme : CSS-Subject (make-css-subject #:type 'clrScheme #::classes '(root)))
(define ~headFont : CSS-Subject (make-css-subject #:type 'fontScheme #:classes '(head)))
(define ~bodyFont : CSS-Subject (make-css-subject #:type 'fontScheme #:classes '(body)))
(define ~lineStyle : CSS-Subject (make-css-subject #:type 'lineStyle))
(define (~fillStyle [class : Symbol]) : CSS-Subject (make-css-subject #:type 'fillStyle #:classes `(,class)))

(define mox-clrscheme-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (<mox-color>)))

(define mox-fillstyle-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (case suitcased-name
      [(datum) (<mox-fill-style>)]
      [(tile-flip) (<css-keyword> mox-tile-flip-options)]
      [(tile-rectangle) (<:mox-region:>)]
      [(rotate-with-shape) (<css-boolean>)])))

(define mox-linestyle-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (case suitcased-name
      [(fill) (<mox-fill-style>)]
      [(pen-align) (<css-keyword/cs> mox-pen-alignment-options)]
      [(end-cap) (<css-keyword/cs> mox-end-cap-types)]
      [(compound) (<css-keyword/cs> mox-compound-line-types)]
      [(width) (<mox-line-width>)]
      [(join) (<mox-line-join>)]
      [(dash) (<:mox-line-dash:>)]
      [(head tail) (<:mox-line-end-shape:>)])))

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

(define mox-fillstyle-filter : (CSS-Cascaded-Value-Filter MOX-Fill-Datum)
  (lambda [declared-values inherited-values]
    (current-css-element-color (css-rgba-ref declared-values inherited-values))
    (mox-extract-fill declared-values inherited-values 'datum)))

(define mox-linestyle-filter : (CSS-Cascaded-Value-Filter MOX-Line)
  (lambda [declared-values inherited-values]
    (current-css-element-color (css-rgba-ref declared-values inherited-values))

    (make-mox-line #:fill (mox-extract-fill declared-values inherited-values 'fill)
                   #:align (css-ref declared-values inherited-values 'pen-align symbol? (#%mox-line-align))
                   #:cap (css-ref declared-values inherited-values 'end-cap symbol? (#%mox-line-cap))
                   #:compound (css-ref declared-values inherited-values 'compound symbol? (#%mox-line-compound))
                   #:width (css-ref declared-values inherited-values 'width nonnegative-flonum? (#%mox-line-width))
                   #:join (css-ref declared-values inherited-values 'join mox-line-join-datum? (#%mox-line-join))
                   #:dash (css-ref declared-values inherited-values 'dash (make-css->unboxed-datum mox-line-dash-datum? (#%mox-line-dash)))
                   #:head (css-ref declared-values inherited-values 'head symbol-list+? (#%mox-line-head))
                   #:tail (css-ref declared-values inherited-values 'tail symbol-list+? (#%mox-line-tail)))))

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

(define read-mox-theme-from-css : (->* (CSS-Stdin)
                                       (Symbol #:fill-count Positive-Index #:bg-fill-count Positive-Index #:line-count Positive-Index)
                                       MOX-Theme)
  (lambda [/dev/cssin [root-type 'base] #:fill-count [fill-count 3] #:bg-fill-count [bg-fill-count 3] #:line-count [line-count 3]]
    (css-root-element-type root-type)

    (define theme.css : CSS-Stylesheet (read-css-stylesheet /dev/cssin))
    (define ~root : CSS-Subject (make-css-subject))
    (define *root : CSS-Values (css-variable-cascade theme.css ~root #false))
    (define-values (clrScheme _clr) (css-cascade theme.css (list ~clrScheme ~root) mox-clrscheme-parsers mox-clrscheme-filter *root))
    (define-values (headFont _maf) (css-cascade theme.css (list ~headFont ~root) mox-fontscheme-parsers mox-fontscheme-filter *root))
    (define-values (bodyFont _mif) (css-cascade theme.css (list ~bodyFont ~root) mox-fontscheme-parsers mox-fontscheme-filter *root))
    (define shapeFills (css-cascade-children theme.css (list (~fillStyle 'shape) ~root) mox-fillstyle-parsers mox-fillstyle-filter *root fill-count))
    (define backgFills (css-cascade-children theme.css (list (~fillStyle 'background) ~root) mox-fillstyle-parsers mox-fillstyle-filter *root bg-fill-count))
    (define lines (css-cascade-children theme.css (list ~lineStyle ~root) mox-linestyle-parsers mox-linestyle-filter *root line-count))

    (mox-theme "Facet" clrScheme headFont bodyFont shapeFills backgFills lines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-extract-fill : (-> CSS-Values (Option CSS-Values) Symbol MOX-Fill-Datum)
  (lambda [declared-values inherited-values property]
    (define datum : (U MOX-Fill-Datum CSS-Image) (css-ref declared-values inherited-values property css->mox-fillstyle))

    (cond [(not (css-image? datum)) datum]
          [else (mox-gradient-fill datum
                                   (css-ref declared-values inherited-values 'tile-rectangle (make-css->unboxed-datum css-region? (#%mox-gradient-fill-tile-rectangle)))
                                   (css-ref declared-values inherited-values 'tile-flip symbol? (#%mox-gradient-fill-flip))
                                   (css-ref declared-values inherited-values 'rotate-with-shape css-boolean? (#%mox-gradient-fill-rotate-with-shape)))])))

(define css->mox-fillstyle : (CSS->Racket (U MOX-Fill-Datum CSS-Image))
  (lambda [desc-name v]
    (cond [(css-image? v) v]
          [(mox-color-transform? v) v]
          [(mox-scheme-color? v) v]
          [(mox-sysclr? v) v]
          [(mox-special-fill? v) v]
          [else (let ([fillstyle (css->color desc-name v)])
                  (cond [(css-wide-keyword? fillstyle) 'phClr]
                        [(eq? fillstyle 'currentcolor) 'phClr]
                        [else fillstyle]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #%mox-window-text : MOX-Color-Datum (cons "windowText" #x000000))
(define #%mox-window : MOX-Color-Datum (cons "window" #xFFFFFF))

; untested default values
(define #%mox-gradient-fill : CSS-Image
  (mox-linear-gradient  (mox-angle 5400000) #false
                        (list (cons (mox-color-transform 'phClr (list (mox-color-component-alteration 'tint (mox+1000ths-percentage 65000))
                                                                      (mox-color-component-alteration 'luminance (mox+1000ths-percentage 110000))))
                                    (list (mox+1000ths-percentage 0)))
                              (cons (mox-color-transform 'phClr (list (mox-color-component-alteration 'tint (mox+1000ths-percentage 90000))))
                                    (list (mox+1000ths-percentage 88000))))))

(define #%mox-default-line : MOX-Line (make-mox-line))

(define #%mox-no-scripts : MOX-Font-Scripts #hasheq())
