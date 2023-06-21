#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "main.rkt")
(require "main/ext/office-art.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-text-list-style : MOX:Text-List-Style (make-mox:text-list-style))
(define default-text-paragraph-property : MOX:Text-Paragraph-Property (make-mox:text-paragraph-property))
(define default-text-character-property : MOX:Text-Character-Property (make-mox:text-character-property))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->text-list-style : (-> XML-Element (Option MOX:Text-List-Style))
  (lambda [lstStyle]
    (for/fold ([self : (Option MOX:Text-List-Style) #false])
              ([child (in-list (caddr lstStyle))] #:when (list? child))
      (if (eq? (car child) 'a:extLst)
          (let ([extLst (xml-element->art-extension-list child)])
            (remake-mox:text-list-style #:extension extLst
                                        (or self default-text-list-style)))
          (let ([pPr (xml-element->text-paragraph-property child)])
            (cond [(not pPr) self]
                  [else (lstStyle-set (or self default-text-list-style)
                                      (car child) pPr)]))))))

(define xml-element->text-paragraph-property : (-> XML-Element (Option MOX:Text-Paragraph-Property))
  (lambda [pPr]
    (define-values (palst rest) (extract-mox#text-paragraph-property (cadr pPr) (car pPr)))
    (for/fold ([self : (Option MOX:Text-Paragraph-Property)
                     (and palst (remake-mox:text-paragraph-property #:attlist palst
                                                                    default-text-paragraph-property))])
              ([child (in-list (caddr pPr))] #:when (list? child))
      (case (car child)
        [(a:defRPr)
         (let ([rPr (xml-element->text-character-property child)])
           (cond [(not rPr) self]
                 [else (remake-mox:text-paragraph-property (or self default-text-paragraph-property)
                                                           #:defRPr rPr)]))]
        [(a:extLst)
         (let ([extLst (xml-element->art-extension-list child)])
           (cond [(not extLst) self]
                 [else (remake-mox:text-paragraph-property (or self default-text-paragraph-property)
                                                           #:extension extLst)]))]
        [else self]))))

(define xml-element->text-character-property : (-> XML-Element (Option MOX:Text-Character-Property))
  (lambda [rPr]
    (define-values (ralst rest) (extract-mox#text-character-property (cadr rPr) (car rPr)))
    (and ralst (remake-mox:text-character-property default-text-character-property #:attlist ralst))))

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
