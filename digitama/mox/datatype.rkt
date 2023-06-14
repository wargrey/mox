#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out sgml/digitama/plain/datatype))
(provide (rename-out [xml:attr-value->symbol mox:attr-value->relationship-id]))

(require sgml/digitama/plain/grammar)
(require sgml/digitama/plain/dialect)
(require sgml/digitama/plain/datatype)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-attributes-extract-positive-2dsize : (-> (Listof XML-Element-Attribute) Symbol Symbol
                                                     (Values (Option (Pairof Index Index))
                                                             (Listof XML-Element-Attribute)))
  (lambda [attrs x y]
    (mox-attributes-extract-pair attrs x y mox:attr-value->coordinate mox:attr-value->coordinate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox:attr-value->coordinate : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v 0 (assert 27273042316900 index?))))
