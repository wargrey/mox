#lang typed/racket/base

(provide (all-defined-out))

(require "pptx/ml/pml.rkt")
(require "drawing/ml/main/text.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pptx-common-slide-data->paragraphs : (-> PPTX:Common-Slide-Data (Listof MOX:Text-Paragraph))
  (lambda [cSld]
    (define tree (pptx:common-slide-data-spTree cSld))

    (let flattern : (Listof MOX:Text-Paragraph)
      ([ps0 : (Listof MOX:Text-Paragraph) null]
       [grp : PPTX:Group-Shape tree])
      (for/fold ([ps : (Listof MOX:Text-Paragraph) ps0])
                ([sp (in-list (pptx:group-shape-children grp))])
        (cond [(pptx:group-shape? sp) (flattern ps sp)]
              [(pptx:shape? sp)
               (let ([body (pptx:shape-txBody sp)])
                 (cond [(not body) ps]
                       [else (append ps (mox:text-body-p+ body))]))]
              [else ps])))))
