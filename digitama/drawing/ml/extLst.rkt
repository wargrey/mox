#lang typed/racket/base

(provide (all-defined-out) XML-Attribute-Extract)

(require sgml/xexpr)

(require "../../dialect.rkt")

(require "main/extension.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-art-extension-list : MOX:Office-Art-Extension-List (make-mox:office-art-extension-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->art-extension-list : (-> XML-Element MOX:Office-Art-Extension-List)
  (lambda [extLst]
    default-art-extension-list))

(define #:forall (A) xml-element->attribute+art-extension-list : (-> XML-Element (XML-Attribute-Extract A)
                                                                     (Option (MOX-Art-Extension-With A)))
  (lambda [child extract-attribute]
    (define-values (attlist _) (extract-attribute (cadr child) (car child)))
    (define extLst : (Option MOX:Office-Art-Extension-List)
      (for/or ([extLst (in-list (caddr child))])
        (and (list? extLst)
             (eq? (car extLst) 'a:extLst)
             (xml-element->art-extension-list extLst))))

    (and (or attlist extLst)
         (mox+extension attlist extLst))))
