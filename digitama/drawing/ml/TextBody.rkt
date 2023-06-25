#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require sgml/xexpr)

(require "main/text.rkt")

(require "TextListStyle.rkt")
(require "extLst.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-text-body-property
  (make-mox:text-body-property #:attlist (make-mox#text-body-property)))

(define default-text-paragraph (make-mox:text-paragraph))

(define default-regular-text-run (make-mox:text-run #:t ""))
(define default-text-line-break (make-mox:text-line-break))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->text-body : (-> XML-Element MOX:Text-Body)
  (lambda [child]
    (define-values (ps rest) (xml-children-filter-map* child mox-text-paragraph-map))

    (let fold ([rest : (Listof XML-Element) rest]
               [bodyPr : (Option MOX:Text-Body-Property) #false]
               [lstStyle : (Option MOX:Text-List-Style) #false])
      (cond [(pair? rest)
             (let ([child (car rest)])
               (case (car child)
                 [(a:bodyPr) (fold (cdr rest) (xml-element->text-body-property child) lstStyle)]
                 [(a:lstStyle) (fold (cdr rest) bodyPr (xml-element->text-list-style child))]
                 [else (fold (cdr rest) bodyPr lstStyle)]))]
            [(not bodyPr) (raise-xml-missing-element-error (car child) 'a:bodyPr)]
            [(null? ps) (raise-xml-missing-element-error (car child) 'a:p)]
            [else (make-mox:text-body #:p+ ps #:bodyPr bodyPr #:lstStyle lstStyle)]))))

(define xml-element->text-body-property : (-> XML-Element MOX:Text-Body-Property)
  (lambda [child]
    (define-values (attlist _) (extract-mox#text-body-property (cadr child) (car child)))

    (xml-children-filter-fold child mox-text-body-property-fold
                              (cond [(not attlist) default-text-body-property]
                                    [else (make-mox:text-body-property #:attlist attlist)]))))

(define xml-element->text-paragraph : (-> XML-Element MOX:Text-Paragraph)
  (lambda [child]
    (define para : MOX:Text-Paragraph
      (xml-children-filter-fold child mox-text-paragraph-fold default-text-paragraph))

    (remake-mox:text-paragraph para #:r* (reverse (mox:text-paragraph-r* para)))))

(define xml-element->text-field : (-> XML-Element MOX:Text-Field)
  (lambda [child]
    (define-values (attlst _) (extract-mox#text-field (cadr child) (car child)))

    (xml-children-filter-fold child mox-text-field-fold
                              (make-mox:text-field #:attlist attlst))))

(define xml-element->text-run : (-> XML-Element (Option MOX-Text-Run))
  (lambda [child]
    (case (car child)
      [(a:r) (xml-children-fold child mox-regular-text-run-fold default-regular-text-run)]
      [(a:br) (xml-children-fold child mox-text-line-break-fold default-text-line-break)]
      [(a:fld) (xml-element->text-field child)]
      [else #false])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-text-body-property-fold : (XML-Children-Filter-Fold MOX:Text-Body-Property)
  (lambda [child self parent]
    (case (car child)
      [(a:extLst) (remake-mox:text-body-property self #:extLst (xml-element->art-extension-list child))]
      [else #false])))

(define mox-text-paragraph-fold : (XML-Children-Filter-Fold MOX:Text-Paragraph)
  (lambda [child self parent]
    (define text-run : (Option MOX-Text-Run) (xml-element->text-run child))

    (if (not text-run)
        (case (car child)
          [(a:pPr)
           (let ([pPr (xml-element->text-paragraph-property child)])
             (and pPr (remake-mox:text-paragraph self #:pPr pPr)))]
          [(a:endParaRPr)
           (let ([rPr (xml-element->text-character-property child)])
             (and rPr (remake-mox:text-paragraph self #:endParaRPr rPr)))]
          [else #false])
        (remake-mox:text-paragraph self #:r* (cons text-run (mox:text-paragraph-r* self))))))

(define mox-text-paragraph-map : (XML-Children-Filter-Map MOX:Text-Paragraph)
  (lambda [child parent]
    (and (eq? (car child) 'a:p)
         (xml-element->text-paragraph child))))

(define mox-regular-text-run-fold : (XML-Children-Fold MOX:Text-Run)
  (lambda [child self parent]
    (case (car child)
      [(a:rPr)
       (let ([rPr (xml-element->text-character-property child)])
         (if (not rPr) self (remake-mox:text-run self #:rPr rPr)))]
      [(a:t) (remake-mox:text-run self #:t (string-join (xml-pcdata-unbox* child)))]
      [else self])))

(define mox-text-line-break-fold : (XML-Children-Fold MOX:Text-Line-Break)
  (lambda [child self parent]
    (case (car child)
      [(a:rPr)
       (let ([rPr (xml-element->text-character-property child)])
         (if (not rPr) self (remake-mox:text-line-break self #:rPr rPr)))]
      [else self])))

(define mox-text-field-fold : (XML-Children-Fold MOX:Text-Field)
  (lambda [child self parent]
    (case (car child)
      [(a:pPr)
       (let ([pPr (xml-element->text-paragraph-property child)])
         (if (not pPr) self (remake-mox:text-field self #:pPr pPr)))]
      [(a:rPr)
       (let ([rPr (xml-element->text-character-property child)])
         (if (not rPr) self (remake-mox:text-field self #:rPr rPr)))]
      [(a:t) (remake-mox:text-field self #:t (xml-pcdata-unbox child))]
      [else self])))
