#lang typed/racket/base

(provide (all-defined-out))
(provide MOX-Stdin MOX-Packageof MOX-File-Properties)
(provide (rename-out [mox-document.ml mox-self]
                     [mox-document.ml mox-document]
                     [mox-package-orphans mox-pkg-orphans]))

(require sgml/xml)

(require "package.rkt")
(require "drawing/moxml.rkt")
(require "shared/moxml.rkt")
(require "shared/property.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-pkg-content-types : (-> (MOX-Packageof Any) MOX-Content-Types)
  (lambda [mox.zip]
    (mox.zip-types mox.zip)))

(define mox-pkg-relationships : (-> (MOX-Packageof Any) (Pairof MOX-Relationships (HashTable String MOX-Relationships)))
  (lambda [mox.zip]
    (cons (mox.zip-rels mox.zip)
          (mox.zip-part-rels mox.zip))))

(define mox-pkg-properties : (-> (MOX-Packageof Any) MOX-File-Properties)
  (lambda [mox.zip]
    (mox-sharedml-properties (mox-shared.ml mox.zip))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-drawing.ml : (-> (MOX-Packageof Any) MOX-DrawingML)
  (lambda [mox.zip]
    (mox.ml-drawing mox.zip)))

(define mox-shared.ml : (-> (MOX-Packageof Any) MOX-SharedML)
  (lambda [mox.zip]
    (mox.ml-shared mox.zip)))

(define mox-document.ml : (All (x) (-> (MOX-Packageof x) x))
  (lambda [mox.zip]
    (mox.ml-document mox.zip)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-theme : (-> (MOX-Packageof Any) (Option XML-Document))
  (lambda [mox.zip]
    (mox-drawingml-theme (mox.ml-drawing mox.zip))))
