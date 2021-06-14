#lang typed/racket/base

(provide (all-defined-out))

(require css)

(require racket/keyword)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-System-Color-Datum (U String (Pairof String Index)))
(define-type MOX-Font-Datum (U String (Pairof String Keyword)))

(define mox-font? : (-> Any Boolean : MOX-Font-Datum)
  (lambda [v]
    (or (string? v)
        (and (pair? v)
             (string? (car v))
             (keyword? (cdr v))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-css-function-filter <mox-system-color> #:-> MOX-System-Color-Datum
  ;;; Fundamentals and Markup Language References, 20.1.2.3.33
  ;;    eg: sysClr(val, lastClr)
  [(sysclr sysClr) #:=> [(cons [val ? string?] [lastClr ? index?])]
   (CSS<&> (CSS:<^> (<css:string>))
           (CSS<?> [(<css-comma>) <:lastClr:>]
                   [else          <:lastClr:>]))]
  #:where
  [(define <:lastClr:>
     (CSS:<^> (CSS:<~> (<css#color> '#:no-alpha)
                       hexa-digits)))])

(define-css-function-filter <mox-panose-font> #:-> MOX-Font-Datum
  ;;; Fundamentals and Markup Language References, 21.1.2.5
  ;;    eg: panose(typeface, value)
  [(panose) #:=> [(cons [typeface ? string?] [number ? keyword?])]
   (CSS<&> (CSS:<^> (<css:string>))
           (CSS<?> [(<css-comma>) <:number:>]
                   [else          <:number:>]))]
  #:where
  [(define <:number:>
     (CSS:<^> <mox-panose>))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-css-disjoint-filter <mox-color> #:-> (U MOX-System-Color-Datum Symbol FlColor CSS-Wide-Keyword)
  (<css-color>)
  (<css:string>)
  (<mox-system-color>))

(define-css-disjoint-filter <mox-font> #:-> (U MOX-Font-Datum CSS-Wide-Keyword)
  (<css:string>)
  (<mox-panose-font>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <mox-panose> : (CSS:Filter Keyword)
  (lambda [t]
    (cond [(not (css:hash? t)) (make-exn:css:type t)]
          [else (let ([number (css:hash-datum t)])
                  (if (not (= (string-length (keyword->immutable-string number)) 20))
                      (make-exn:css:digit t)
                      number))])))
