#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "main/shape.rkt")
(require "main/matrix.rkt")

(require "ColorChoice.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->shape-style : (-> XML-Element MOX:Shape-Style)
  (lambda [style]
    (define-values (styles rest) (xml-children->map* style mox-stype-matrix-reference-map))
    (define fontRef.xml : (Option XML-Element) (xml-elements-ref rest 'a:fontRef))
    (define fontRef : MOX:Font-Reference
      (cond [(and fontRef.xml) (xml-element->font-reference fontRef.xml)]
            [else (raise-xml-missing-element-error (car style) 'a:fontRef)]))
    
    (make-mox:shape-style #:lnRef (hash-ref styles 'a:lnRef
                                            (λ [] (raise-xml-missing-element-error
                                                   (car style) 'a:lnRef)))
                          #:fillRef (hash-ref styles 'a:fillRef
                                              (λ [] (raise-xml-missing-element-error
                                                     (car style) 'a:fillRef)))
                          #:effectRef (hash-ref styles 'a:effectRef
                                                (λ [] (raise-xml-missing-element-error
                                                   (car style) 'a:effectRef)))
                          #:fontRef fontRef)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-stype-matrix-reference-map : (XML-Children-Filter-Map MOX:Style-Matrix-Reference)
  (lambda [child parent]
    (and (not (eq? (car child) 'a:fontRef))
         (let-values ([(attlst rest) (extract-mox#style-matrix-reference (cadr child) (car child))]
                      [(maybe-color) (xml-children-ref child 0)])
           (make-mox:style-matrix-reference #:color (and maybe-color (xml-element->color-choice maybe-color))
                                            #:attlist attlst)))))

(define xml-element->font-reference : (-> XML-Element MOX:Font-Reference)
  (lambda [child]
    (define-values (attlst rest) (extract-mox#font-reference (cadr child) (car child)))
    (define maybe-color (xml-children-ref child 0))

    (make-mox:font-reference #:color (and maybe-color (xml-element->color-choice maybe-color))
                             #:attlist attlst)))
