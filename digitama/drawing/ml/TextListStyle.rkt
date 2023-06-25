#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "main/text.rkt")

(require "Hyperlink.rkt")
(require "FillProps.rkt")
(require "extLst.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-text-paragraph-property : MOX:Text-Paragraph-Property (make-mox:text-paragraph-property))
(define default-text-character-property : MOX:Text-Character-Property (make-mox:text-character-property))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->text-list-style : (-> XML-Element (Option MOX:Text-List-Style))
  (lambda [lstStyle]
    (define-values (pPrs rest) (xml-children->map* lstStyle text-paragraph-property-map))
    (define maybe-extLst : (Option XML-Element) (xml-elements-ref rest 'a:extLst))
    (define pPrs-okay? : Boolean (not (hash-empty? pPrs)))
    
    (and (or pPrs-okay? maybe-extLst)
         (make-mox:text-list-style #:pPrs (and pPrs-okay? pPrs)
                                   #:extLst (and maybe-extLst
                                                 (xml-element->art-extension-list maybe-extLst))))))

(define xml-element->text-paragraph-property : (-> XML-Element (Option MOX:Text-Paragraph-Property))
  (lambda [pPr]
    (define-values (palst rest) (extract-mox#text-paragraph-property (cadr pPr) (car pPr)))
    
    (xml-children-filter-fold pPr mox-text-paragraph-property-fold
                              (and palst (make-mox:text-paragraph-property #:attlist palst)))))

(define xml-element->text-character-property : (-> XML-Element (Option MOX:Text-Character-Property))
  (lambda [rPr]
    (define-values (ralst rest) (extract-mox#text-character-property (cadr rPr) (car rPr)))
    (xml-children-filter-fold rPr mox-text-character-property-fold
                              (and ralst (make-mox:text-character-property #:attlist ralst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define text-paragraph-property-map : (XML-Children-Filter-Map MOX:Text-Paragraph-Property)
  (lambda [child parent]
    (and (not (eq? (car child) 'a:extLst))
         (xml-element->text-paragraph-property child))))

(define mox-text-paragraph-property-fold : (XML-Children-Filter-Fold (Option MOX:Text-Paragraph-Property))
  (lambda [child self parent]
    (case (car child)
      [(a:defRPr)
       (let ([rPr (xml-element->text-character-property child)])
         (and rPr (remake-mox:text-paragraph-property #:defRPr rPr
                                                      (or self default-text-paragraph-property))))]
      [(a:extLst)
       (remake-mox:text-paragraph-property #:extLst (xml-element->art-extension-list child)
                                           (or self default-text-paragraph-property))]
      [else self])))

(define mox-text-character-property-fold : (XML-Children-Filter-Fold (Option MOX:Text-Character-Property))
  (lambda [child self parent]
    (case (car child)
      [(a:hlinkClick)
       (remake-mox:text-character-property #:hlinkClick (xml-element->hyperlink child)
                                           (or self default-text-character-property))]
      [(a:hlinkMouseOver)
       (remake-mox:text-character-property #:hlinkMouseOver (xml-element->hyperlink child)
                                           (or self default-text-character-property))]
      [(a:extLst)
       (remake-mox:text-character-property #:extLst (xml-element->art-extension-list child)
                                           (or self default-text-character-property))]
      [else (cond [(xml-element->fill-property child)
                   => (λ [fill]
                        (remake-mox:text-character-property #:fill fill
                                                            (or self default-text-character-property)))]
                  [else #false])])))
