#lang typed/racket/base

(provide (all-defined-out))

(require digimon/function)
(require sgml/sax)

(require "main.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-text-list-style : MOX-Text-List-Style (make-mox-text-list-style))
(define default-text-paragraph-property : MOX-Text-Paragraph-Property (make-mox-text-paragraph-property))
(define default-text-character-property : MOX-Text-Character-Property (make-mox-text-character-property))

(struct lstStyle mox-text-list-style
  ([master : Symbol]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-list-style-sax-element : (XML-Element-Handler MOX-Text-List-Style)
  (lambda [element xpath attlist ?empty ?preserve self]
    (case element
      [(a:defPPr a:lvl1pPr a:lvl2pPr a:lvl3pPr a:lvl4pPr a:lvl5pPr a:lvl6pPr a:lvl7pPr a:lvl8pPr a:lvl9pPr)
       (if (list? attlist)
           (let-values ([(palst rest) (extract-mox:attr:text-paragraph attlist element)]
                        [(pPr) (lstStyle-ref self element)])
             (cond [(not palst) (mox-text-list-style-apply lstStyle self element)]
                   [(not pPr) (lstStyle-set self element (make-mox-text-paragraph-property #:attlist palst))]
                   [else (lstStyle-set self element (remake-mox-text-paragraph-property pPr #:attlist palst))]))
           (remake-mox-text-list-style self))]
      [else (and (lstStyle? self)
                 (let* ([elem (lstStyle-master self)]
                        [pPr (lstStyle-ref self elem)]
                        [pPr++ (mox-subpara-property-sax-element element xpath attlist ?empty ?preserve pPr)])
                   (and pPr++ (lstStyle-set self elem pPr++))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-subpara-property-sax-element : (XML-Element-Handler (Option MOX-Text-Paragraph-Property))
  (lambda [element xpath attlist ?empty ?preserve self]
    (if (list? attlist)
        (case element
          [(a:defRPr)
           (let-values ([(ralst rest) (extract-mox:attr:text-character attlist element)])
             (cond [(not ralst) self]
                   [else (let ([pPr (or self default-text-paragraph-property)]
                               [rPr (make-mox-text-character-property #:attlist ralst)])
                           (remake-mox-text-paragraph-property pPr #:defRPr rPr))]))]
          [else #false])

        #| ETage |#
        self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lstStyle-ref : (-> MOX-Text-List-Style Symbol (Option MOX-Text-Paragraph-Property))
  (lambda [self element]
    (case element
      [(a:defPPr) (mox-text-list-style-defPPr self)]
      [(a:lvl1pPr) (mox-text-list-style-lvl1pPr self)]
      [(a:lvl2pPr) (mox-text-list-style-lvl2pPr self)]
      [(a:lvl3pPr) (mox-text-list-style-lvl3pPr self)]
      [(a:lvl4pPr) (mox-text-list-style-lvl4pPr self)]
      [(a:lvl5pPr) (mox-text-list-style-lvl5pPr self)]
      [(a:lvl6pPr) (mox-text-list-style-lvl6pPr self)]
      [(a:lvl7pPr) (mox-text-list-style-lvl7pPr self)]
      [(a:lvl8pPr) (mox-text-list-style-lvl8pPr self)]
      [(a:lvl9pPr) (mox-text-list-style-lvl9pPr self)]
      [else #false])))

(define lstStyle-set : (-> MOX-Text-List-Style Symbol (Option MOX-Text-Paragraph-Property) MOX-Text-List-Style)
  (lambda [self element pPr]
    (case element
      [(a:defPPr) (mox-text-list-style-apply lstStyle self #:defPPr pPr element)]
      [(a:lvl1pPr) (mox-text-list-style-apply lstStyle self #:lvl1pPr pPr element)]
      [(a:lvl2pPr) (mox-text-list-style-apply lstStyle self #:lvl2pPr pPr element)]
      [(a:lvl3pPr) (mox-text-list-style-apply lstStyle self #:lvl3pPr pPr element)]
      [(a:lvl4pPr) (mox-text-list-style-apply lstStyle self #:lvl4pPr pPr element)]
      [(a:lvl5pPr) (mox-text-list-style-apply lstStyle self #:lvl5pPr pPr element)]
      [(a:lvl6pPr) (mox-text-list-style-apply lstStyle self #:lvl6pPr pPr element)]
      [(a:lvl7pPr) (mox-text-list-style-apply lstStyle self #:lvl7pPr pPr element)]
      [(a:lvl8pPr) (mox-text-list-style-apply lstStyle self #:lvl8pPr pPr element)]
      [(a:lvl9pPr) (mox-text-list-style-apply lstStyle self #:lvl9pPr pPr element)]
      [else self])))
