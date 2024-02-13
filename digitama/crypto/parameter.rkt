#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type OLE-Ask-Password (-> Any String))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ole-ask-password/cli : OLE-Ask-Password
  (lambda [mox.x]
    (printf "Encrypted file: ~a~nPassword: " mox.x)

    (let ([line (read-line (current-input-port))])
      (if (string? line) line ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-ole-ask-password : (Parameterof OLE-Ask-Password)
  (make-parameter ole-ask-password/cli))
