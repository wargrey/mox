#lang typed/racket/base

(require "ooxml.rkt")

(require racket/file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define APA.docx : Path (#%docx))

(define APA.zip (time (read-docx-package APA.docx)))

(pretty-display APA.zip)
