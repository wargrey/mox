#lang typed/racket/base

(require css)
(require css/tamer/recognizer)

(require digimon/spec)

(require mox/digitama/css/datatype)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(spec-begin recognizer #:do
            (context "datatype" #:do
                     (context "color" #:do
                              (it-check "sysClr(windowText #954F72)" (<mox-color>) mox-color-datum?)
                              (it-check "clrTransform(accent1, alpha(mod(61.8%)), tint(2021), shade(0706))" (<mox-color+transform>) mox-color-transform?)
                              (it-check "clrTransform(scrollBar, inverse-gamma, gray, complement)" (<mox-color+transform>) mox-color-transform?)
                              (it-check "keyword-should-be-case-sensitive" (<mox-color>) #false))
                     
                     (context "font" #:do
                              (it-check "\"新細明體\"" (<mox-font>) mox-font-datum?)
                              (it-check "panose(\"Calibri\" #020F0302020204030204)" (<mox-font>) mox-font-datum?)
                              (it-check "panose(\"Calibri\" #020F03020202040302)" (<mox-font>) 'exn:css:digit))))
