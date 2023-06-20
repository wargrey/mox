#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "../../drawing/ml/main.rkt")

(require "pml.rkt")
(require "extLst.rkt")

(require "../../drawing/ml/main/clrMap.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-slide : PPTX-Slide (make-pptx-slide))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-document->slide/text : (-> XML-Document PPTX-Slide)
  (lambda [master.xml]
    (define root : XML-Element (assert (xml-root-xexpr master.xml)))
    (define-values (ns rest) (xml-attributes-extract-xmlns (cadr root)))
    (define-values (sattr _) (extract-pptx:attr:slide rest (car root)))

    (let transform ([children : (Listof XExpr-Element-Children) (caddr root)]
                    [self : PPTX-Slide (remake-pptx-slide default-slide #:namespaces ns #:attlist sattr)])
      (cond [(pair? children)
             (let-values ([(child rest) (values (car children) (cdr children))])
               (with-asserts ([child list?])
                 (case (car child)
                   [(p:clrMapOvr)
                    (transform rest
                               (let ([clrMap (xml-element->color-map-override child)])
                                 (if (not clrMap) self (remake-pptx-slide self #:color-map clrMap))))]
                   [(p:extLst) (transform rest (remake-pptx-slide self #:extension (xml-element->modify-extension-list child)))]
                   [else (transform rest self)])))]
            #;[(or (not self) (eq? (pptx-presentation-notes-size self) default-positive-2dsize)) (raise-xml-missing-element-error (car root) 'notesSz)]
            [else self]))))

(define xml-document->slide : (-> XML-Document PPTX-Slide)
  (lambda [master.xml]
    (xml-document->slide/text master.xml)))
