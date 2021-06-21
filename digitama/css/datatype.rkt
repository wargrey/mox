#lang typed/racket/base

(provide (all-defined-out))

(require css)
(require digimon/enumeration)

(require racket/keyword)
(require racket/symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-System-Color-Datum (U String (Pairof String Index)))
(define-type MOX-Font-Datum (U String (Pairof String Keyword)))
(define-type MOX-Color-Component-Transformation-Value (U Nonnegative-Flonum CSS-%))

(define-type MOX-Simple-Color (U Index FlColor MOX-System-Color-Datum))
(define-type MOX-Color-Datum (U MOX-Simple-Color MOX-Scheme-Color))

(define-enumeration mox-system-color : MOX-System-Color
  [background scrollBar activeCaption inactiveCaption menu window windowFrame menuText windowText
              captionText activeBorder inactiveBorder appWorkspace highlight highlightText btnFace
              btnShadow grayText btnText inactiveCaptionText btnHighlight 3dDkShadow 3dLight infoText
              infoBk hotLight gradientActiveCaption gradientInactiveCaption menuHighlight menuBar])

(define-enumeration mox-scheme-color : MOX-Scheme-Color
  [phClr #| use the style color |#
   accent1 accent2 accent3 accent4 accent5 accent6
   bg1 bg2 dk1 dk2 lt1 lt2 tx1 tx2 hlink folHlink])

(define-css-value mox-color-component-transform #:as MOX-Color-Component-Transform ([type : Symbol] [value : MOX-Color-Component-Transformation-Value]))
(define-css-value mox-color-transform #:as MOX-Color-Transform ([target : MOX-Color-Datum] [alterations : (Listof MOX-Color-Component-Transform)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <mox-system-color-keyword> : (CSS:Filter String)
  (CSS:<~> (<css-keyword/cs> mox-system-colors) symbol->immutable-string))

(define mox-simple-color? : (-> Any Boolean : #:+ MOX-Simple-Color)
  (lambda [c]
    (or (flcolor? c)
        (index? c)
        (mox-sysclr? c))))

(define mox-color-datum? : (-> Any Boolean : #:+ MOX-Color-Datum)
  (lambda [c]
    (or (mox-simple-color? c)
        (mox-scheme-color? c))))

(define mox-sysclr? : (-> Any Boolean : MOX-System-Color-Datum)
  (lambda [v]
    (or (string? v)
        (and (pair? v)
             (string? (car v))
             (index? (cdr v))))))

(define mox-color-transformations? : (-> Any Boolean : (Listof MOX-Color-Component-Transform))
  (lambda [ts]
    (and (list? ts)
         (andmap mox-color-component-transform? ts))))

(define mox-font? : (-> Any Boolean : MOX-Font-Datum)
  (lambda [v]
    (or (string? v)
        (and (pair? v)
             (string? (car v))
             (keyword? (cdr v))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-css-function-filter <mox-color-component-transform> #:-> MOX-Color-Component-Transform
  ;;; Fundamentals and Markup Language References, 20.1.2.3
  [(alpha) #:=> [(mox-color-component-transform 'alpha [value ? css-%? nonnegative-flonum?])]
   <:nonneg-percent+mod+off:>]
  [(red) #:=> [(mox-color-component-transform 'red [value ? css-%? nonnegative-flonum?])]
   <:percent+mod+off:>]
  [(green) #:=> [(mox-color-component-transform 'green [value ? css-%? nonnegative-flonum?])]
   <:percent+mod+off:>]
  [(blue) #:=> [(mox-color-component-transform 'blue [value ? css-%? nonnegative-flonum?])]
   <:percent+mod+off:>]
  [(hue) #:=> [(mox-color-component-transform 'hue [value ? css-%? nonnegative-flonum?])]
   <:angle+mod+off:>]
  [(sat saturation) #:=> [(mox-color-component-transform 'saturation [value ? css-%? nonnegative-flonum?])]
   <:percent+mod+off:>]
  [(lum luminance) #:=> [(mox-color-component-transform 'luminance [value ? css-%? nonnegative-flonum?])]
   <:percent+mod+off:>]
  #:where
  [(define-css-function-filter <mox-color-component-modulation> #:-> CSS+%
     [(mod Mod modulation Modulation)#:=> [(values [value ? css+%?])] #:<+> (CSS:<^> (<mox+percentage>))])

   (define-css-function-filter <mox-color-component-offset-percentage> #:-> CSS-%
     [(off Off offset Offset) #:=> [(values [value ? css-%?])] #:<+> (CSS:<^> (<mox-percentage>))])

   (define-css-function-filter <mox-color-component-offset-angle> #:-> Flonum
     [(off Off offset Offset) #:=> [(values [value ? flonum?])] #:<+> (CSS:<^> (<mox-angle>))])

   (define <:percent+mod+off:> (CSS:<^> (CSS:<+> (<mox-percentage>) (<mox-color-component-modulation>) (<mox-color-component-offset-percentage>))))
   (define <:nonneg-percent+mod+off:> (CSS:<^> (CSS:<+> (<mox+percentage>) (<mox-color-component-modulation>) (<mox-color-component-offset-percentage>))))
   (define <:angle+mod+off:> (CSS:<^> (CSS:<+> (<mox+angle>) (<mox-color-component-modulation>) (<mox-color-component-offset-angle>))))])

(define-css-function-filter <mox-system-color> #:-> MOX-System-Color-Datum
  ;;; Fundamentals and Markup Language References, 20.1.2.3.33
  ;;    eg: sysClr(val, lastClr) or sysClr(val lastClr)
  [(sysclr sysClr systemclr systemClr) #:=> [(cons [val ? string?] [lastClr ? index?])]
   (CSS<&> (CSS:<^> <mox-system-color-keyword>)
           (css-comma-parser (CSS:<^> (CSS:<~> (<css#color> '#:no-alpha)
                                               hexa-digits))))])

(define-css-function-filter <mox-color-transformation> #:-> MOX-Color-Transform
  ;;; Fundamentals and Markup Language References, 20.1.2.3.33
  ;;    eg: sysClr(val, lastClr)
  [(clrtransform clrTransform) #:=> [(mox-color-transform [color ? mox-color-datum?] [transforms ? mox-color-transformations?])]
   (CSS<&> (CSS:<^> (<mox-color>))
           (CSS<!> (css-comma-parser (CSS:<^> (<mox-color-component-transform>)))))])

(define-css-function-filter <mox-panose-font> #:-> MOX-Font-Datum
  ;;; Fundamentals and Markup Language References, 21.1.2.5
  ;;    eg: panose(typeface, value)
  [(panose) #:=> [(cons [typeface ? string?] [number ? keyword?])]
   (CSS<&> (CSS:<^> (<css:string>))
           (css-comma-parser (CSS:<^> <mox-panose>)))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-css-disjoint-filter <mox-color> #:-> (U MOX-System-Color-Datum Symbol FlColor CSS-Wide-Keyword)
  (<css-color>)
  <mox-system-color-keyword>
  (<css-keyword/cs> mox-scheme-colors)
  (<mox-system-color>))

(define-css-disjoint-filter <mox-color+transform> #:-> (U MOX-System-Color-Datum Symbol FlColor MOX-Color-Transform CSS-Wide-Keyword)
  (<mox-color>)
  (<mox-color-transformation>))

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
    (css-% (* (exact->inexact v) 0.00001))))

(define mox+fixed-percentage : (-> Natural CSS+%)
  (lambda [v]
    (let ([% (* (real->double-flonum v) 0.00001)])
      (css+% % %))))

(define mox-angle : (case-> [Natural -> Nonnegative-Flonum]
                            [Integer -> Flonum])
  (lambda [v]
    (real->double-flonum (/ v 60000))))
