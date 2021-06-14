#lang typed/racket/base

(provide (all-defined-out))

(require css)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-System-Color-Datum (U String (Pairof String Index)))

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

(define-css-disjoint-filter <mox-color> #:-> (U MOX-System-Color-Datum Symbol FlColor CSS-Wide-Keyword)
  (<css-color>)
  (<css:string>)
  (<mox-system-color>))
