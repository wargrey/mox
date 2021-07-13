#lang typed/racket/base

(require css)
(require css/digitama/image)

(require css/tamer/recognizer)

(require digimon/spec)

(require mox/digitama/css/datatype)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(spec-begin recognizer #:do
            (context "datatype" #:do
                     (context "color" #:do
                              (it-check "sysClr(windowText #954F72)" (<mox-color>) mox-color-datum?)
                              (it-check "clrTransform(accent1, alpha(#mod 61.8%), tint(2021), shade(0706))" (<mox-color>) mox-color-transform?)
                              (it-check "clrTransform(scrollBar, inverse-gamma, gray, complement)" (<mox-color>) mox-color-transform?)
                              (it-check "keyword-should-be-case-sensitive" (<mox-color>) #false))

                     (context "gradient" #:do
                              (it-check "lin(5400000, clrTransform(phClr, tint(65000), lum(#mod 110000)), clrTransform(phClr, tint(90000)) 88000)"
                                        (<mox-fill-gradient>) css-gradient?)
                              (it-check "lin(5400000 #scaled, clrTransform(phClr, tint(65000), lum(#mod 110000)), clrTransform(phClr, tint(90000)) 88000)"
                                        (<mox-fill-gradient>) css-gradient?)
                              (it-check "path(circle, clrTransform(phClr, tint(90000), lum(#mod 110000)), clrTransform(phClr, shade(94000), lum(#mod 96000)) 100000)"
                                        (<mox-fill-gradient>) css-gradient?)
                              (it-check "path(circle in 50000 100000 100000 50000, clrTransform(phClr, tint(90000)), clrTransform(phClr, shade(94000)) 100000)"
                                        (<mox-fill-gradient>) css-gradient?))

                     (context "line style" #:do
                              (it-check "arrow med" (<:mox-line-end-shape:>) (list mox-line-end-type? mox-line-end-size-option?))
                              (it-check "lg diamond" (<:mox-line-end-shape:>) (list mox-line-end-size-option? mox-line-end-type?))
                              (it-check "stealth lg sm" (<:mox-line-end-shape:>) (list mox-line-end-type? mox-line-end-size-option? mox-line-end-size-option?))
                              (it-check "med sm triangle" (<:mox-line-end-shape:>) (list mox-line-end-size-option? mox-line-end-size-option? mox-line-end-type?))
                              (it-check "lg oval sm" (<:mox-line-end-shape:>) (list mox-line-end-size-option? mox-line-end-type? mox-line-end-size-option?)))
                     
                     (context "font" #:do
                              (it-check "\"新細明體\"" (<mox-font>) mox-font-datum?)
                              (it-check "panose(\"Calibri\" #020F0302020204030204)" (<mox-font>) mox-font-datum?)
                              (it-check "panose(\"Calibri\" #020F03020202040302)" (<mox-font>) 'exn:css:digit))))
