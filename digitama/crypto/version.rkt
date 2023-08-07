#lang typed/racket/base

(provide (all-defined-out))

(require digimon/stdio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-binary-struct ds-version-info : DataSpace-Version-Info
  ([identifier : (LNLocale 4)]
   [reader : LUInt32]
   [updater : LUInt32]
   [writer : LUInt32]))
