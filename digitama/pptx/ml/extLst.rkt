#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "pml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-extension-list : PPTX:Extension-List (make-pptx:extension-list))
(define default-modify-extension-list : PPTX:Extension-List-Modify (make-pptx:extension-list-modify))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->extension-list : (-> XML-Element PPTX:Extension-List)
  (lambda [extLst]
    default-extension-list))

(define xml-element->extension-list-modify : (-> XML-Element PPTX:Extension-List-Modify)
  (lambda [extLst]
    default-modify-extension-list))
