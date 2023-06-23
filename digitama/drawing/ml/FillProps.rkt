#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "main/fill.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->line-fill-property : (-> XML-Element (Option MOX-Line-Fill-Property))
  (lambda [child]
    (case (car child)
      [(a:noFill) 'none]
      [(a:solidFill) (make-mox:solid-fill)]
      [(a:gradFill) (make-mox:gradient-fill)]
      [(a:pattFill) (make-mox:pattern-fill)]
      [else #false])))

(define xml-element->fill-property : (-> XML-Element (Option MOX-Fill-Property))
  (lambda [child]
    (or (xml-element->line-fill-property child)
        (case (car child)
          [(a:grpFill) 'group]
          [(a:blipFill) (make-mox:blip-fill)]
          [else #false]))))
