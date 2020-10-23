#lang typed/racket/base

(require "ooxml.rkt")

(require racket/file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define StateDepartment.xlsx : Path (#%xlsx))

(define StateDepartment.zip (time (read-xlsx-package StateDepartment.xlsx)))

;(pretty-display StateDepartment.zip)

(hash-ref StateDepartment.zip #"xl/workbook.xml")
