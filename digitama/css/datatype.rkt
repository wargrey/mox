#lang typed/racket/base

(provide (all-defined-out))

(require css)
(require css/digitama/color)
(require css/digitama/image)
(require css/digitama/syntax/misc)

(require digimon/enumeration)
(require digimon/predicate)

(require racket/keyword)
(require racket/symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-System-Color-Datum (U String (Pairof String Index)))
(define-type MOX-Font-Datum (U String (Pairof String Keyword)))

(define-type MOX-Simple-Color (U Index FlColor MOX-System-Color-Datum))
(define-type MOX-Color-Datum (U MOX-Simple-Color MOX-Scheme-Color))

(define-type MOX-Fill-Style (U MOX-Color-Datum MOX-Color-Transform))
(define-type MOX-Linear-Color-Stop (Pairof MOX-Fill-Style (Listof CSS-%)))
(define-type MOX-Linear-Color-Stops (Pairof MOX-Linear-Color-Stop (Listof+ MOX-Linear-Color-Stop)))

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

(define mox-color-transformation-elements : (Listof Symbol) '(complement inverse gamma gray comp inv invgamma inverse-gamma))

(define-css-value mox-color-component-transform #:as MOX-Color-Component-Transform ([type : Symbol] [value : CSS-Flonum-%]))
(define-css-value mox-color-transform #:as MOX-Color-Transform ([target : MOX-Color-Datum] [alterations : (Listof (U MOX-Color-Component-Transform Symbol))]))

(define-css-value mox-linear-gradient #:as MOX-Linear-Gradient #:=> css-gradient ([angle : Flonum] [stops : MOX-Linear-Color-Stops]))
(define-css-value mox-path-gradient #:as MOX-Path-Gradient #:=> css-gradient ([path : Symbol] [region : (Option CSS-Region)] [stops : MOX-Linear-Color-Stops]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-simple-color? : (-> Any Boolean : MOX-Simple-Color)
  (lambda [c]
    (or (flcolor? c)
        (index? c)
        (mox-sysclr? c))))

(define mox-color-datum? : (-> Any Boolean : MOX-Color-Datum)
  (lambda [c]
    (or (mox-simple-color? c)
        (mox-scheme-color? c))))

(define mox-sysclr? : (-> Any Boolean : MOX-System-Color-Datum)
  (lambda [v]
    (or (string? v)
        (and (pair? v)
             (string? (car v))
             (index? (cdr v))))))

(define mox-color-transformation+element? : (-> Any Boolean : (U MOX-Color-Component-Transform Symbol))
  (lambda [t]
    (or (mox-color-component-transform? t)
        (symbol? t))))

(define mox-color-transformations? : (-> Any Boolean : (Listof (U MOX-Color-Component-Transform Symbol)))
  (lambda [ts]
    (and (list? ts)
         (andmap mox-color-transformation+element? ts))))

(define mox-fill-style? : (-> Any Boolean : MOX-Fill-Style)
  (lambda [t]
    (or (mox-color-datum? t)
        (mox-color-transform? t))))

(define mox-linear-color-stop? : (-> Any Boolean : MOX-Linear-Color-Stop)
  (lambda [v]
    (and (pair? v)
         (mox-fill-style? (car v))
         ((inst is-listof? CSS-%) (cdr v) css-%?))))

(define mox-linear-color-stop-list? : (-> Any Boolean : MOX-Linear-Color-Stops)
  (lambda [datum]
    (and (list? datum)
         (pair? datum)
         (mox-linear-color-stop? (car datum))
         ((inst is-listof+? MOX-Linear-Color-Stop) (cdr datum) mox-linear-color-stop?))))

(define mox-font-datum? : (-> Any Boolean : MOX-Font-Datum)
  (lambda [v]
    (or (string? v)
        (and (pair? v)
             (string? (car v))
             (keyword? (cdr v))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <mox-system-color-keyword> : (CSS:Filter String)
  (CSS:<~> (<css-keyword/cs> mox-system-colors) symbol->immutable-string))

(define-css-function-filter <mox-color-component-transform> #:-> MOX-Color-Component-Transform
  ;;; Fundamentals and Markup Language References, 20.1.2.3
  [(alpha)          #:=> [(mox-color-component-transform 'alpha      [value ? css-%? flonum?])]
   <:nonneg-percent+mod+off:>]
  [(red)            #:=> [(mox-color-component-transform 'red        [value ? css-%? flonum?])]
   <:percent+mod+off:>]
  [(green)          #:=> [(mox-color-component-transform 'green      [value ? css-%? flonum?])]
   <:percent+mod+off:>]
  [(blue)           #:=> [(mox-color-component-transform 'blue       [value ? css-%? flonum?])]
   <:percent+mod+off:>]
  [(hue)            #:=> [(mox-color-component-transform 'hue        [value ? css-%? flonum?])]
   <:angle+mod+off:>]
  [(sat saturation) #:=> [(mox-color-component-transform 'saturation [value ? css-%? flonum?])]
   <:percent+mod+off:>]
  [(lum luminance)  #:=> [(mox-color-component-transform 'luminance  [value ? css-%? flonum?])]
   <:percent+mod+off:>]
  [(tint)           #:=> [(mox-color-component-transform 'tint       [value ? css-%?])]
   <:fixed-percentage:>]
  [(shade)          #:=> [(mox-color-component-transform 'shade      [value ? css-%?])]
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
           (css-omissible-comma-parser (CSS:<^> (CSS:<~> (<css#color> '#:no-alpha)
                                                         hexa-digits))))])

(define-css-function-filter <mox-color-transformation> #:-> MOX-Color-Transform
  ;;; Fundamentals and Markup Language References, 20.1.2.3.33
  ;;    eg: clrTransform(color, tranformation-list)
  [(clrtransform clrTransform) #:=> [(mox-color-transform [color ? mox-color-datum?] [transforms ? mox-color-transformations?])]
   (CSS<&> (css-comma-followed-parser (CSS:<^> (<mox-color>)))
           (CSS<!> (CSS:<#> (CSS:<+> (<mox-color-component-transform>)
                                     (<css-keyword> mox-color-transformation-elements)))))])

(define-css-function-filter <mox-fill-gradient> #:-> CSS-Gradient
  ;;; Fundamentals and Markup Language References, 20.1.4.1.13
  [(linear-gradient-fill lin)
   #:=> [(mox-linear-gradient [angle ? flonum?] [stops ? mox-linear-color-stop-list?])
         (mox-linear-gradient (css-named-direction->degree 'bottom) [stops ? mox-linear-color-stop-list?])]
   (CSS<&> (css-comma-followed-parser (CSS:<^> (<mox+angle>))) (<:mox-length-color-stop:>))]
  [(path-gradient-fill path)
   #:=> [(mox-path-gradient [path ? symbol?] [region ? css-region?] [stops ? mox-linear-color-stop-list?])
         (mox-path-gradient 'default [region ? css-region?] [stops ? mox-linear-color-stop-list?])
         (mox-path-gradient [path ? symbol?] css-no-region [stops ? mox-linear-color-stop-list?])
         (mox-path-gradient 'default css-no-region [stops ? mox-linear-color-stop-list?])]
   (CSS<&> (css-comma-followed-parser (<:path-region:>)) (<:mox-length-color-stop:>))]
  #:where
  [(define (<:mox-length-color-stop:>) (<:css-color-stop-list:> (CSS:<^> (<mox-color+transform>)) (<mox+percentage>)))
   (define (<:path-region:>)
     (CSS<&> (CSS:<^> (<css-keyword> mox-path-gradient-shapes))
             (CSS<?> [(<css-keyword:in>) (<:css-region:> (CSS:<~> (<mox-percentage>) css-%-value))]
                     [else values])))])

(define-css-function-filter <mox-panose-font> #:-> MOX-Font-Datum
  ;;; Fundamentals and Markup Language References, 21.1.2.5
  ;;    eg: panose(typeface, value)
  [(panose) #:=> [(cons [typeface ? string?] [number ? keyword?])]
   (CSS<&> ((inst CSS:<^> Any) (<css:string>))
           (css-omissible-comma-parser (CSS:<^> <mox-panose>)))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-css-disjoint-filter <mox-color> #:-> (U MOX-System-Color-Datum CSS-Color-Datum)
  (<css-color>)
  <mox-system-color-keyword>
  (<css-keyword/cs> mox-scheme-colors)
  (<mox-system-color>))

(define-css-disjoint-filter <mox-color+transform> #:-> (U MOX-System-Color-Datum CSS-Color-Datum MOX-Color-Transform)
  (<mox-color>)
  (<mox-color-transformation>))

(define-css-disjoint-filter <mox-fill-style> #:-> (U MOX-System-Color-Datum CSS-Color-Datum MOX-Color-Transform CSS-Gradient)
  (<mox-color+transform>)
  (<css-gradient-notation>)
  (<mox-fill-gradient>))

(define-css-disjoint-filter <mox-font> #:-> (U MOX-Font-Datum CSS-Wide-Keyword)
  (<css:string>)
  (<mox-panose-font>))

(define-css-disjoint-filter <mox-percentage> #:-> CSS-%
  (<css-percentage>)
  (CSS:<~> (<css:integer>) mox-fixed-percentage))

(define-css-disjoint-filter <mox+percentage> #:-> CSS+%
  (<css+percentage>)
  (CSS:<~> (<css:integer> nonnegative-fixnum?) mox+fixed-percentage))

(define-css-disjoint-filter <mox-angle> #:-> Flonum
  (CSS:<~> (<css:integer>) mox-angle))

(define-css-disjoint-filter <mox+angle> #:-> Nonnegative-Flonum
  (CSS:<~> (<css:integer> nonnegative-fixnum?) mox-angle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <mox-panose> : (CSS:Filter Keyword)
  (lambda [t]
    (cond [(not (css:hash? t)) (make-exn:css:type t)]
          [else (let ([number (css:hash-datum t)])
                  (if (not (= (string-length (keyword->immutable-string number)) 20))
                      (make-exn:css:digit t)
                      number))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-fixed-percentage : (-> Integer CSS-%)
  (lambda [v]
    (make-css-% (* (exact->inexact v) 0.00001))))

(define mox+fixed-percentage : (-> Natural CSS+%)
  (lambda [v]
    (make-css+% (* (real->double-flonum v) 0.00001))))

(define mox-angle : (case-> [Natural -> Nonnegative-Flonum]
                            [Integer -> Flonum])
  (lambda [v]
    (real->double-flonum (/ v 60000))))
