#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "../../../drawing/ml/main.rkt")

(require "../pml.rkt")
(require "../extLst.rkt")

(require "../../../drawing/ml/main/clrMap.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-slide-master : PPTX-Slide-Master (make-pptx-slide-master #:color-map default-mox-color-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-document->slide-master/text : (-> XML-Document PPTX-Slide-Master)
  (lambda [master.xml]
    (define root : XML-Element (assert (xml-root-xexpr master.xml)))
    (define-values (ns rest) (xml-attributes-extract-xmlns (cadr root)))
    (define-values (maybe-prvr _) (xml-attributes-extract rest 'preserve))
    (define prvr : (Option XML-Boolean) (and maybe-prvr (xml:attr-value->boolean maybe-prvr)))

    (let transform ([children : (Listof XExpr-Element-Children) (caddr root)]
                    [self : PPTX-Slide-Master (remake-pptx-slide-master default-slide-master #:namespaces ns #:preserve? (or prvr 'false))])
      (cond [(pair? children)
             (let-values ([(child rest) (values (car children) (cdr children))])
               (with-asserts ([child list?])
                 (case (car child)
                   [(p:sldLayoutIdLst) (transform rest (remake-pptx-slide-master self #:layouts (xml-element->slide-layout-entries child)))]
                   [(p:clrMap) (transform rest (remake-pptx-slide-master self #:color-map (xml-element->color-map child)))]
                   [(p:extLst) (transform rest (remake-pptx-slide-master self #:extension (xml-element->modify-extension-list child)))]
                   [else (transform rest self)])))]
            #;[(or (not self) (eq? (pptx-presentation-notes-size self) default-positive-2dsize)) (raise-xml-missing-element-error (car root) 'notesSz)]
            [else self]))))

(define xml-document->slide-master : (-> XML-Document PPTX-Slide-Master)
  (lambda [master.xml]
    (xml-document->slide-master/text master.xml)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->slide-layout-entries : (-> XML-Element (Listof PPTX:Attr:Slide-Layout-Entry))
  (lambda [sldMasterIdLst]
    (let transform ([children : (Listof XExpr-Element-Children) (caddr sldMasterIdLst)]
                    [seirtne : (Listof PPTX:Attr:Slide-Layout-Entry) null])
      (if (pair? children)
          (let-values ([(child rest) (values (car children) (cdr children))])
            (with-asserts ([child list?]) ; p:sldLayoutId
              (let-values ([(entry _) (extract-pptx:attr:slide-layout-entry (cadr child) (car child))])
                (transform rest (cons entry seirtne)))))
          (reverse seirtne)))))
