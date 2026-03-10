#lang typed/racket/base

(provide (all-defined-out))

(require digimon/scribble)
(require digimon/digitama/minimal/dtrace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-dtrace-sender : (Parameterof Scribble-Message) (make-parameter dtrace-message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-dtrace : (-> Symbol String Any * Void)
  (lambda [level fmt . argl]
    (apply (default-dtrace-sender) level fmt argl)))

(define mox-note : (-> String Any * Void)
  (lambda [fmt . argl]
    (apply (default-dtrace-sender) 'note fmt argl)))

(define mox-debug : (-> String Any * Void)
  (lambda [fmt . argl]
    (apply (default-dtrace-sender) 'debug fmt argl)))

