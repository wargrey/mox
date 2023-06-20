#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "pml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-extension-list : PPTX-Extension-List (make-pptx-extension-list))
(define default-modify-extension-list : PPTX-Modify-Extension-List (make-pptx-modify-extension-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->extension-list : (-> XML-Element PPTX-Extension-List)
  (lambda [extLst]
    default-extension-list))

(define xml-element->modify-extension-list : (-> XML-Element PPTX-Modify-Extension-List)
  (lambda [extLst]
    default-modify-extension-list))
