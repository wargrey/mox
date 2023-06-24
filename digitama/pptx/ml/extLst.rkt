#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "../../dialect.rkt")
(require "extension.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-extension-list (make-pptx:extension-list))
(define default-modify-extension-list (make-pptx:extension-list-modify))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->extension-list : (-> XML-Element PPTX:Extension-List)
  (lambda [extLst]
    default-extension-list))

(define xml-element->extension-list-modify : (-> XML-Element PPTX:Extension-List-Modify)
  (lambda [extLst]
    default-modify-extension-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (A) xml-element->attribute+extension-list
  : (case-> [XML-Element (XML-Attribute-Extract (∩ A MOX-Attribute)) -> (PPTX-Extension-With A)]
            [XML-Element (XML-Attribute-Extract A) -> (Option (PPTX-Extension-With A))])
  (lambda [child extract-attribute]
    (define-values (attlist _) (extract-attribute (cadr child) (car child)))
    (define extLst : (Option PPTX:Extension-List)
      (for/or ([extLst (in-list (caddr child))])
        (and (list? extLst)
             (eq? (car extLst) 'p:extLst)
             (xml-element->extension-list extLst))))

    (and (or attlist extLst)
         (mox+extension attlist extLst))))

(define #:forall (A) xml-element->attribute+extension-list-modify
  : (case-> [XML-Element (XML-Attribute-Extract (∩ A MOX-Attribute)) -> (PPTX-Extension-Modify-With A)]
            [XML-Element (XML-Attribute-Extract A) -> (Option (PPTX-Extension-Modify-With A))])
  (lambda [child extract-attribute]
    (define-values (attlist _) (extract-attribute (cadr child) (car child)))
    (define extLst : (Option PPTX:Extension-List-Modify)
      (for/or ([extLst (in-list (caddr child))])
        (and (list? extLst)
             (eq? (car extLst) 'p:extLst)
             (xml-element->extension-list-modify extLst))))

    (and (or attlist extLst)
         (mox+extension attlist extLst))))
