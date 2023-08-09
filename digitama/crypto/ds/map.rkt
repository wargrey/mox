#lang typed/racket/base

(provide (all-defined-out))

(require digimon/stdio)
(require digimon/enumeration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type DataSpace-Map-Entry (Pairof String (Listof DataSpace-Map-RefCom)))

(define-enumeration* ds-refcom-type #:+> DS-RefCom-Type
  ds-refcom-type->number number->ds-refcom-type
  [0 Stream Storage])

(define-binary-struct ds-map-refcom : DataSpace-Map-RefCom
  ([type : (#:enum LUInt32 ds-refcom-type->number number->ds-refcom-type)]
   [name : (LNLocale 4)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-ds-map-entries : (-> Input-Port (Listof DataSpace-Map-Entry))
  (lambda [/dev/mapin]
    (define map-header-size (read-luint32 /dev/mapin))
    (define entry-count (read-luint32 /dev/mapin))

    (drop-bytes /dev/mapin (- map-header-size 8))
    (for/list ([idx (in-range entry-count)])
      (read-ds-map-entry /dev/mapin))))

(define read-ds-map-entry : (-> Input-Port DataSpace-Map-Entry)
  (lambda [/dev/mapin]
    (define length (read-luint32 /dev/mapin))
    (define count (read-luint32 /dev/mapin))

    (define refcoms : (Listof DataSpace-Map-RefCom)
      (for/list ([idx (in-range count)])
        (read-ds-map-refcom /dev/mapin)))
    
    (cons (read-ln:lcstring /dev/mapin 4)
          refcoms)))
