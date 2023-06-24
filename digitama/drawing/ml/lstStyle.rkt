#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "main/text.rkt")

(require "Hyperlink.rkt")
(require "FillProps.rkt")
(require "extLst.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-text-list-style : MOX:Text-List-Style (make-mox:text-list-style))
(define default-text-paragraph-property : MOX:Text-Paragraph-Property (make-mox:text-paragraph-property))
(define default-text-character-property : MOX:Text-Character-Property (make-mox:text-character-property))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->text-list-style : (-> XML-Element (Option MOX:Text-List-Style))
  (lambda [lstStyle]
    (xml-children-filter-fold lstStyle mox-text-list-style-fold #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-text-list-style-fold : (XML-Children-Filter-Fold (Option MOX:Text-List-Style))
  (lambda [child self parent]
    (if (eq? (car child) 'a:extLst)
        (remake-mox:text-list-style #:extLst (xml-element->art-extension-list child)
                                    (or self default-text-list-style))
        (let ([pPr (xml-element->text-paragraph-property child)])
          (and pPr (lstStyle-set (or self default-text-list-style)
                                 (car child) pPr))))))

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
                   => (λ [fill] (remake-mox:text-character-property #:fill fill
                                                                    (or self default-text-character-property)))]
                  [else #false])])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(define lstStyle-set : (-> MOX:Text-List-Style Symbol MOX:Text-Paragraph-Property MOX:Text-List-Style)
  (lambda [self element pPr]
    (case element
      [(a:defPPr) (remake-mox:text-list-style self #:defPPr pPr)]
      [(a:lvl1pPr) (remake-mox:text-list-style self #:lvl1pPr pPr)]
      [(a:lvl2pPr) (remake-mox:text-list-style self #:lvl2pPr pPr)]
      [(a:lvl3pPr) (remake-mox:text-list-style self #:lvl3pPr pPr)]
      [(a:lvl4pPr) (remake-mox:text-list-style self #:lvl4pPr pPr)]
      [(a:lvl5pPr) (remake-mox:text-list-style self #:lvl5pPr pPr)]
      [(a:lvl6pPr) (remake-mox:text-list-style self #:lvl6pPr pPr)]
      [(a:lvl7pPr) (remake-mox:text-list-style self #:lvl7pPr pPr)]
      [(a:lvl8pPr) (remake-mox:text-list-style self #:lvl8pPr pPr)]
      [(a:lvl9pPr) (remake-mox:text-list-style self #:lvl9pPr pPr)]
      [else self])))
