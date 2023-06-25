#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/base.rkt"))
(provide (all-from-out "digitama/pptx/ml/pml.rkt"))

(require "digitama/base.rkt")
(require "digitama/package.rkt")
(require "digitama/pptx/moxml.rkt")

(require "digitama/drawing.rkt")
(require "digitama/pptx.rkt")

(require "digitama/pptx/ml/pml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-PowerPoint-Package (MOX-Packageof MOX-PowerPoint))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-pptx-package : (-> MOX-Stdin MOX-PowerPoint-Package)
  (lambda [/dev/stdin]
    (mox-input-package /dev/stdin moxml-power-point-agent 'full)))

(define read-pptx-package-for-text : (-> MOX-Stdin MOX-PowerPoint-Package)
  (lambda [/dev/stdin]
    (mox-input-package /dev/stdin moxml-power-point-agent 'text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pptx-extract-paragraphs-from-slide-master : (-> (U MOX-PowerPoint-Package MOX-PowerPoint)
                                                        (Listof (Listof String)))
  (lambda [pptx.zip]
    (define powerpoint.ml (if (mox-powerpoint? pptx.zip) pptx.zip (mox-self pptx.zip)))
    (define masters (mox-powerpoint-slide-masters powerpoint.ml))

    (for/list ([master (in-list masters)])
      (map text-paragraph-unbox
           (pptx-common-slide-data->paragraphs
            (pptx:slide-master-cSld master))))))

(define pptx-extract-paragraphs-from-slide : (-> (U MOX-PowerPoint-Package MOX-PowerPoint)
                                                 (Listof (Listof String)))
  (lambda [pptx.zip]
    (define powerpoint.ml (if (mox-powerpoint? pptx.zip) pptx.zip (mox-self pptx.zip)))
    (define slides (mox-powerpoint-slides powerpoint.ml))

    (for/list ([slide (in-list slides)])
      (map text-paragraph-unbox
           (pptx-common-slide-data->paragraphs
            (pptx:slide-cSld slide))))))
