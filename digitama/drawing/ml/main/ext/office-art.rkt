#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "../extension.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-art-extension-list : MOX-Office-Art-Extension-List (make-mox-office-art-extension-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->art-extension-list : (-> XML-Element MOX-Office-Art-Extension-List)
  (lambda [extLst]
    default-art-extension-list))
