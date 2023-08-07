#lang typed/racket/base

(provide (all-defined-out))

(require digimon/stdio)

(require "../../ole/cfb.rkt")
(require "../../ole/stream.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-ds-definition : (-> Input-Port MS-CFB String (Listof String)
                                 (Immutable-HashTable String (Listof String)))
  (lambda [/dev/cfbin cfb storage names]
    (for/fold ([refers : (Immutable-HashTable String (Listof String)) (hash)])
              ([name (in-list names)])
      (define self : (Option (Listof String))
        (call-with-input-stream /dev/cfbin cfb
          (string-append storage name)
          read-ds-transform-references))
      (cond [(not self) refers]
            [else (hash-set refers name self)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-ds-transform-references : (-> Input-Port (Listof String))
  (lambda [/dev/defin]
    (define length (read-luint32 /dev/defin))
    (define count (read-luint32 /dev/defin))

    (drop-bytes /dev/defin (- length 8))
    (for/list ([idx (in-range count)])
      (read-ln:lcstring /dev/defin 4))))
