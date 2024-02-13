#lang typed/racket/base

(provide (all-defined-out))

(require css)
(require css/digitama/color)
(require css/digitama/image)
(require css/digitama/syntax/misc)
(require css/digitama/syntax/dimension)

(require digimon/enumeration)
(require digimon/predicate)
(require digimon/dimension)
(require digimon/symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-System-Color-Datum (U String (Pairof String Index)))
(define-type MOX-Font-Datum (U String (Pairof String Keyword)))

(define-type MOX-Raw-Color-Datum (U Index FlColor Symbol MOX-System-Color-Datum MOX-Scheme-Color))
(define-type MOX-Color-Datum (U MOX-Raw-Color-Datum MOX-Color-Transform))

(define-type MOX-Fill-Style (U MOX-Color-Datum CSS-Image))
(define-type MOX-Linear-Color-Stop (Pairof MOX-Fill-Style (Listof CSS-%)))
(define-type MOX-Linear-Color-Stops (Pairof MOX-Linear-Color-Stop (Listof+ MOX-Linear-Color-Stop)))
(define-type MOX-Line-Join-Datum (U Symbol Nonnegative-Flonum))
(define-type MOX-Line-Dash-Datum (U Symbol MOX-Line-Dasharray))

(define-enumeration mox-system-color : MOX-System-Color
  [background scrollBar activeCaption inactiveCaption menu window windowFrame menuText windowText
              captionText activeBorder inactiveBorder appWorkspace highlight highlightText btnFace
              btnShadow grayText btnText inactiveCaptionText btnHighlight 3dDkShadow 3dLight infoText
              infoBk hotLight gradientActiveCaption gradientInactiveCaption menuHighlight menuBar])

(define-enumeration mox-scheme-color : MOX-Scheme-Color
  [phClr #| use the style color |#
   accent1 accent2 accent3 accent4 accent5 accent6
   bg1 bg2 dk1 dk2 lt1 lt2 tx1 tx2 hlink folHlink])

(define-enumeration mox-path-gradient-shape : MOX-Path-Gradient-Shape [circle rect shape])
(define-enumeration mox-tile-flip-option : MOX-Tile-Flip [none x y xy])
(define-enumeration mox-special-fill : MOX-Special-Fill [none group])

(define-enumeration mox-pen-alignment-option : MOX-Pen-Alignment [ctr in])
(define-enumeration mox-end-cap-type : MOX-End-Cap-Type [flat rnd sq])
(define-enumeration mox-compound-line-type : MOX-Compound-Line-Type [dbl sng thickThin thinThick tri])
(define-enumeration mox-line-join-type : MOX-Line-Join-Type [none bevel miter round])
(define-enumeration mox-preset-dash-name : MOX-Prefab-Dash-Name [solid dash dot dashDot lgDash lgDashDot lgDashDotDot sysDash sysDashDot sysDashDotDot sysDot])
(define-enumeration mox-line-end-size-option : MOX-Line-End-Size-Option [lg med sm]) ; for both length and width
(define-enumeration mox-line-end-type : MOX-Line-End-Type [none arrow diamond oval stealth triangle])
(define-enumeration mox-effect-type : MOX-Effect-Type [effectDag effectLst scene3d sp3d])

(define mox-color-transformation-elements : (Listof Symbol) '(complement inverse gamma gray comp inv invgamma inverse-gamma))

(define-css-value mox-color-component-alteration #:as MOX-Color-Component-Alteration ([type : Symbol] [value : CSS-Flonum-%]))
(define-css-value mox-color-transform #:as MOX-Color-Transform ([target : MOX-Raw-Color-Datum] [alterations : (Listof (U MOX-Color-Component-Alteration Symbol))]))
(define-css-value mox-line-dasharray #:as MOX-Line-Dasharray ([stops : (Listof (Pairof Nonnegative-Flonum Nonnegative-Flonum))]))

(define-css-value mox-gradient #:as MOX-Gradient #:=> css-gradient ())
(define-css-value mox-linear-gradient #:as MOX-Linear-Gradient #:=> mox-gradient ([angle : Flonum] [scaled : Boolean] [stops : MOX-Linear-Color-Stops]))
(define-css-value mox-path-gradient #:as MOX-Path-Gradient #:=> mox-gradient ([path : Symbol] [region : (Option CSS-Region)] [stops : MOX-Linear-Color-Stops]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <mox-system-color-keyword> : (CSS:Filter String)
  (CSS:<~> (<css-keyword/cs> mox-system-colors) symbol->immutable-string))

(define-css-function-filter <mox-color-component-alteration> #:-> MOX-Color-Component-Alteration
  ;;; Fundamentals and Markup Language References, 20.1.2.3
  [(alpha)          #:=> [(mox-color-component-alteration 'alpha      [value ? css-%? flonum?])]
   <:nonneg-percent+mod+off:>]
  [(red)            #:=> [(mox-color-component-alteration 'red        [value ? css-%? flonum?])]
   <:percent+mod+off:>]
  [(green)          #:=> [(mox-color-component-alteration 'green      [value ? css-%? flonum?])]
   <:percent+mod+off:>]
  [(blue)           #:=> [(mox-color-component-alteration 'blue       [value ? css-%? flonum?])]
   <:percent+mod+off:>]
  [(hue)            #:=> [(mox-color-component-alteration 'hue        [value ? css-%? flonum?])]
   <:angle+mod+off:>]
  [(sat saturation) #:=> [(mox-color-component-alteration 'saturation [value ? css-%? flonum?])]
   <:percent+mod+off:>]
  [(lum luminance)  #:=> [(mox-color-component-alteration 'luminance  [value ? css-%? flonum?])]
   <:percent+mod+off:>]
  [(tint)           #:=> [(mox-color-component-alteration 'tint       [value ? css-%?])]
   <:fixed-percentage:>]
  [(shade)          #:=> [(mox-color-component-alteration 'shade      [value ? css-%?])]
   <:fixed-percentage:>]
  #:where
  [(define <:mod+off:>
     (CSS<?> [(<css:hash> '(#:mod #:Mod #:modulation #:Modulation)) ((inst CSS:<^> Any) (<mox+percentage>))]
             [(<css:hash> '(#:off #:Off #:offset #:Offset)) ((inst CSS:<^> Any) (<mox-percentage>))]))

   (define <:percent+mod+off:> (CSS<+> (CSS:<^> (CSS:<~> (<mox-percentage>) css-%-value)) <:mod+off:>))
   (define <:nonneg-percent+mod+off:> (CSS<+> (CSS:<^> (CSS:<~> (<mox+percentage>) css+%-value)) <:mod+off:>))
   (define <:angle+mod+off:> (CSS<+> (CSS:<^> (<mox-angle>)) <:mod+off:>))
   (define <:fixed-percentage:> (CSS:<^> (<mox+percentage>)))])

(define-css-function-filter <mox-system-color> #:-> MOX-System-Color-Datum
  ;;; Fundamentals and Markup Language References, 20.1.2.3.33
  ;;    eg: sysClr(val, lastClr) or sysClr(val lastClr)
  [(sysclr sysClr systemclr systemClr) #:=> [(cons [val ? string?] [lastClr ? index?])]
   (CSS<&> ((inst CSS:<^> Any) <mox-system-color-keyword>)
           (css-omissible-comma-parser
            (CSS:<^> (CSS:<~> (<css#color> '#:no-alpha) hexa-digits))))])

(define-css-function-filter <mox-color-transformation> #:-> MOX-Color-Transform
  ;;; Fundamentals and Markup Language References, 20.1.2.3.33
  ;;    eg: clrTransform(color, tranformation-list)
  [(clrtransform clrTransform) #:=> [(mox-color-transform [color ? mox-raw-color-datum?] [transforms ? mox-color-transformations?])]
   (CSS<&> (css-comma-followed-parser (CSS:<^> (<mox-color>)))
           (CSS<!> (CSS:<#> (CSS:<+> (<mox-color-component-alteration>)
                                     (<css-keyword> mox-color-transformation-elements)))))])

(define-css-function-filter <mox-fill-gradient> #:-> CSS-Gradient
  ;;; Fundamentals and Markup Language References, 20.1.4.1.13
  [(linear-gradient-fill lin)
   #:=> [(mox-linear-gradient [angle ? flonum?] [scaled? ? boolean?] [stops ? mox-linear-color-stop-list?])
         (mox-linear-gradient (css-named-direction->degree 'bottom) [scaled? ? boolean?] [stops ? mox-linear-color-stop-list?])
         (mox-linear-gradient [angle ? flonum?] #false [stops ? mox-linear-color-stop-list?])
         (mox-linear-gradient (css-named-direction->degree 'bottom) #false [stops ? mox-linear-color-stop-list?])]
   (CSS<&> (css-comma-followed-parser (<:scaled-angle:>)) (<:mox-length-color-stop:>))]
  [(path-gradient-fill path)
   #:=> [(mox-path-gradient [path ? symbol?] [region ? css-region?] [stops ? mox-linear-color-stop-list?])
         (mox-path-gradient 'default [region ? css-region?] [stops ? mox-linear-color-stop-list?])
         (mox-path-gradient [path ? symbol?] css-full-region [stops ? mox-linear-color-stop-list?])
         (mox-path-gradient 'default css-full-region [stops ? mox-linear-color-stop-list?])]
   (CSS<&> (css-comma-followed-parser (<:path-region:>)) (<:mox-length-color-stop:>))]
  #:where
  [(define (<:mox-length-color-stop:>) (<:css-color-stop-list:> (CSS:<^> (<mox-color>)) (<mox+percentage>)))

   (define (<:scaled-angle:>)
     (CSS<&> (CSS:<^> (<mox+angle>))
             (CSS:<*> (CSS:<=> (<css:hash> '#:scaled) #true) '?)))
   
   (define (<:path-region:>)
     (CSS<&> (CSS:<^> (<css-keyword> mox-path-gradient-shapes))
             (CSS<?> [(<css-keyword:in>) (<:mox-region:>)]
                     [else values])))])

(define-css-function-filter <mox-panose-font> #:-> MOX-Font-Datum
  ;;; Fundamentals and Markup Language References, 21.1.2.5
  ;;    eg: panose(typeface, value)
  [(panose) #:=> [(cons [typeface ? string?] [number ? keyword?])]
   (CSS<&> ((inst CSS:<^> Any) (<css:string>))
           (css-omissible-comma-parser (CSS:<^> <mox-panose>)))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-css-disjoint-filter <mox-raw-color> #:-> (U MOX-Raw-Color-Datum CSS-Wide-Keyword)
  (<css-color>)
  <mox-system-color-keyword>
  (<css-keyword/cs> mox-scheme-colors)
  (<mox-system-color>))

(define-css-disjoint-filter <mox-color> #:-> (U MOX-Color-Datum CSS-Wide-Keyword)
  (<mox-color-transformation>)
  (<mox-raw-color>))

(define-css-disjoint-filter <mox-fill-style> #:-> (U MOX-Color-Datum CSS-Gradient CSS-Wide-Keyword)
  (<mox-color>)
  (<css-gradient-notation>)
  (<mox-fill-gradient>)
  (<css-keyword> mox-special-fills))

(define-css-disjoint-filter <mox-line-join> #:-> (U Symbol Nonnegative-Flonum)
  (<css-keyword> mox-line-join-types)
  (CSS:<~> (<mox+percentage> mox+1000ths-percentage) css+%-value))

(define-css-disjoint-filter <mox-font> #:-> (U MOX-Font-Datum CSS-Wide-Keyword)
  (<css:string>)
  (<mox-panose-font>))

(define-css-disjoint-filter <mox-percentage> #:-> CSS-%
  (<css-percentage>)
  (CSS:<~> (<css:integer>) mox-1000ths-percentage))

(define-css-disjoint-filter <mox+percentage> #:-> CSS+%
  #:with [[css->racket : (-> Natural CSS+%) mox+1000ths-percentage]]
  (<css+percentage>)
  (CSS:<~> (<css:integer> nonnegative-fixnum?) css->racket))

(define-css-disjoint-filter <mox-angle> #:-> Flonum
  (CSS:<~> (<css:integer>) mox-angle))

(define-css-disjoint-filter <mox+angle> #:-> Nonnegative-Flonum
  (CSS:<~> (<css:integer> nonnegative-fixnum?) mox-angle))

(define-css-disjoint-filter <mox-line-width> #:-> Nonnegative-Flonum
  (<css+length>)
  (CSS:<~> (<css-natural>) mox-line-width))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (<:mox-region:>) : (CSS-Parser (Listof CSS-Region))
  (<:css-region:> (CSS:<~> (<mox-percentage>) css-%-value)))

(define (<:mox-line-dash:>) : (CSS-Parser (Listof MOX-Line-Dash-Datum))
  (CSS<+> (CSS:<^> (<css-keyword> mox-preset-dash-names))
          (CSS<~> (CSS<#> (CSS<~> (CSS:<*> (<mox+percentage> mox+1000ths-percentage) '2) mox-line-dash-stop) '+)
                  mox-line-dasharray)))

(define (<:mox-line-end-shape:>) : (CSS-Parser (Listof Symbol))
  (CSS<++> (CSS:<^> (<css-keyword> mox-line-end-types))
           (CSS:<^> (<css-keyword> mox-line-end-size-options))
           (CSS:<*> (<css-keyword> mox-line-end-size-options) '?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <mox-panose> : (CSS:Filter Keyword)
  (lambda [t]
    (cond [(not (css:hash? t)) (make-exn:css:type t)]
          [else (let ([number (css:hash-datum t)])
                  (if (not (= (string-length (keyword->immutable-string number)) 20))
                      (make-exn:css:digit t)
                      number))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-1000ths-percentage : (-> Integer CSS-%)
  (lambda [v]
    (make-css-% (* (exact->inexact v) 0.00001))))

(define mox+1000ths-percentage : (-> Natural CSS+%)
  (lambda [v]
    (make-css+% (* (real->double-flonum v) 0.00001))))

(define mox-angle : (case-> [Natural -> Nonnegative-Flonum]
                            [Integer -> Flonum])
  (lambda [v]
    (real->double-flonum (/ v 60000))))

(define mox-line-width : (-> Natural Nonnegative-Flonum)
  (lambda [v]
    (dim:length (real->double-flonum (/ v 12700)) 'pt css-dimenv)))

(define mox-line-dash-stop : (-> (Listof CSS+%) (Pairof Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [da]
    (cond [(and (pair? da) (pair? (cdr da)))
           (cons (css+%-value (car da))
                 (css+%-value (cadr da)))]
          [else '#:deadcode (cons +nan.0 +nan.0)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-raw-color-datum? : (-> Any Boolean : MOX-Raw-Color-Datum)
  (lambda [c]
    (or (flcolor? c)
        (index? c)
        (symbol? c)
        (mox-scheme-color? c)
        (mox-sysclr? c))))

(define mox-color-datum? : (-> Any Boolean : MOX-Color-Datum)
  (lambda [c]
    (or (mox-raw-color-datum? c)
        (mox-color-transform? c))))

(define mox-sysclr? : (-> Any Boolean : MOX-System-Color-Datum)
  (lambda [v]
    (or (string? v)
        (and (pair? v)
             (string? (car v))
             (index? (cdr v))))))

(define mox-color-transformation+element? : (-> Any Boolean : (U MOX-Color-Component-Alteration Symbol))
  (lambda [t]
    (or (mox-color-component-alteration? t)
        (symbol? t))))

(define mox-color-transformations? : (-> Any Boolean : (Listof (U MOX-Color-Component-Alteration Symbol)))
  (lambda [ts]
    (listof? ts mox-color-transformation+element?)))

(define mox-fill-style? : (-> Any Boolean : MOX-Fill-Style)
  (lambda [t]
    (or (css-image? t)
        (mox-color-datum? t))))

(define mox-linear-color-stop? : (-> Any Boolean : MOX-Linear-Color-Stop)
  (lambda [v]
    (and (pair? v)
         (mox-fill-style? (car v))
         ((inst listof? CSS-%) (cdr v) css-%?))))

(define mox-linear-color-stop-list? : (-> Any Boolean : MOX-Linear-Color-Stops)
  (lambda [cs]
    (and (list? cs)
         (pair? cs)
         (mox-linear-color-stop? (car cs))
         ((inst listof+? MOX-Linear-Color-Stop) (cdr cs) mox-linear-color-stop?))))

(define mox-line-join-datum? : (-> Any Boolean : #:+ MOX-Line-Join-Datum)
  (lambda [v]
    (or (symbol? v)
        (nonnegative-flonum? v))))

(define mox-line-dash-datum? : (-> Any Boolean : #:+ MOX-Line-Dash-Datum)
  (lambda [v]
    (or (symbol? v)
        (mox-line-dasharray? v))))

(define mox-font-datum? : (-> Any Boolean : MOX-Font-Datum)
  (lambda [v]
    (or (string? v)
        (and (pair? v)
             (string? (car v))
             (keyword? (cdr v))))))
