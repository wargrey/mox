#lang typed/racket/base

(require digimon/port)

(require "../ooxml.rkt")

(require "../../digitama/ole/cfb.rkt")
(require "../../digitama/ole/header.rkt")
(require "../../digitama/ole/directory.rkt")
(require "../../digitama/ole/stream.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define file.dat : Path-String
  (or ($1)
      (build-path (#%dir) "cfb.dat")))

(define cfb.dat : Bytes
  (call-with-input-file* file.dat
    (λ [[/dev/datin : Input-Port]]
      (port->bytes (open-input-hexdump /dev/datin #false #:width 16)))))

(define /dev/cfbin (open-input-bytes cfb.dat))
(define /dev/hexout (open-output-hexdump (current-output-port)))
(define cfb (read-compound-file /dev/cfbin))

(display-cfb-header (ms-cfb-header cfb) #:mode #true)

(displayln (ms-cfb-fat cfb))
(displayln (ms-cfb-minifat cfb))

(for ([dir (in-vector (ms-cfb-directories cfb))])
  (display-cfb-directory-entry dir #:mode #true))

(for ([obj (in-list (ms-cfb-objects cfb))])
  (define /dev/objin (open-input-stream /dev/cfbin cfb (car obj)))

  (when (input-port? /dev/objin)
    (writeln obj)
    (copy-port /dev/objin /dev/hexout)))
