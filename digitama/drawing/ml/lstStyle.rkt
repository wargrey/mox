#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "main.rkt")
(require "main/ext/office-art.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-text-list-style : MOX-Text-List-Style (make-mox-text-list-style))
(define default-text-paragraph-property : MOX-Text-Paragraph-Property (make-mox-text-paragraph-property))
(define default-text-character-property : MOX-Text-Character-Property (make-mox-text-character-property))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->text-list-style : (-> XML-Element (Option MOX-Text-List-Style))
  (lambda [lstStyle]
    (let transform ([children : (Listof XExpr-Element-Children) (caddr lstStyle)]
                    [self : (Option MOX-Text-List-Style) #false])
      (if (pair? children)
          (let-values ([(child rest) (values (car children) (cdr children))])
            (with-asserts ([child list?])
              (define pPr (xml-element->text-paragraph-property child))
              (transform rest
                         (cond [(not pPr) self]
                               [else (lstStyle-set (or self default-text-list-style)
                                                   (car child) pPr)]))))
          self))))

(define xml-element->text-paragraph-property : (-> XML-Element (Option MOX-Text-Paragraph-Property))
  (lambda [pPr]
    (define-values (palst rest) (extract-mox:attr:text-paragraph (cadr pPr) (car pPr)))
    (let transform ([children : (Listof XExpr-Element-Children) (caddr pPr)]
                    [self : (Option MOX-Text-Paragraph-Property) (and palst (remake-mox-text-paragraph-property default-text-paragraph-property #:attlist palst))])
      (cond [(pair? children)
             (let-values ([(child rest) (values (car children) (cdr children))])
               (with-asserts ([child list?])
                 (define rPr (xml-element->text-character-property child))
                 (transform rest
                            (cond [(not rPr) self]
                                  [else (remake-mox-text-paragraph-property #:defRPr rPr
                                                                            (or self default-text-paragraph-property))]))))]
            [else self]))))

(define xml-element->text-character-property : (-> XML-Element (Option MOX-Text-Character-Property))
  (lambda [rPr]
    (define-values (ralst rest) (extract-mox:attr:text-character (cadr rPr) (car rPr)))
    (and ralst (remake-mox-text-character-property default-text-character-property #:attlist ralst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lstStyle-set : (-> MOX-Text-List-Style Symbol MOX-Text-Paragraph-Property MOX-Text-List-Style)
  (lambda [self element pPr]
    (case element
      [(a:defPPr) (remake-mox-text-list-style self #:defPPr pPr)]
      [(a:lvl1pPr) (remake-mox-text-list-style self #:lvl1pPr pPr)]
      [(a:lvl2pPr) (remake-mox-text-list-style self #:lvl2pPr pPr)]
      [(a:lvl3pPr) (remake-mox-text-list-style self #:lvl3pPr pPr)]
      [(a:lvl4pPr) (remake-mox-text-list-style self #:lvl4pPr pPr)]
      [(a:lvl5pPr) (remake-mox-text-list-style self #:lvl5pPr pPr)]
      [(a:lvl6pPr) (remake-mox-text-list-style self #:lvl6pPr pPr)]
      [(a:lvl7pPr) (remake-mox-text-list-style self #:lvl7pPr pPr)]
      [(a:lvl8pPr) (remake-mox-text-list-style self #:lvl8pPr pPr)]
      [(a:lvl9pPr) (remake-mox-text-list-style self #:lvl9pPr pPr)]
      [else self])))
