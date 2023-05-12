#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "standards.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-drawing-xmlns : (-> XExpr-AttList)
  (lambda []
    `([xmlns:cx . ,(assert (opc-xmlns 'Draw:CX))]
      [xmlns:cx1 . ,(assert (opc-xmlns 'Draw:CX1))]
      [xmlns:cx2 . ,(assert (opc-xmlns 'Draw:CX2))]
      [xmlns:cx3 . ,(assert (opc-xmlns 'Draw:CX3))]
      [xmlns:cx4 . ,(assert (opc-xmlns 'Draw:CX4))]
      [xmlns:cx5 . ,(assert (opc-xmlns 'Draw:CX5))]
      [xmlns:cx6 . ,(assert (opc-xmlns 'Draw:CX6))]
      [xmlns:cx7 . ,(assert (opc-xmlns 'Draw:CX7))]
      [xmlns:cx8 . ,(assert (opc-xmlns 'Draw:CX8))]
      [xmlns:aink . ,(assert (opc-xmlns 'Draw:AINK))]
      [xmlns:am3d . ,(assert (opc-xmlns 'Draw:AM3D))]
      
      [xmlns:o . ,(assert (opc-xmlns 'Draw:O))]
      [xmlns:r . ,(assert (opc-xmlns 'Draw:R))]
      [xmlns:m . ,(assert (opc-xmlns 'Draw:M))]
      [xmlns:v . ,(assert (opc-xmlns 'Draw:V))])))

(define mox-compatibility-xmlns : (-> XExpr-AttList)
  (lambda []
    `([xmlns:mc . ,(assert (opc-xmlns 'Compatibility))]
      [mc:Ignorable . "w14 w15 w16se w16cid w16 w16cex w16sdtdh wp14"])))
