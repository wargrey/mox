#lang typed/racket/base

(require mox/digitama/drawing/theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(read-mox-theme-from-css
 (let ([argv (current-command-line-arguments)])
   (cond [(= (vector-length argv) 1) (vector-ref argv 0)]
         [else (collection-file-path "default.css" "mox" "stone" "theme")])))
