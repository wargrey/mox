#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xml)

(require "../moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct mox-diagram
  ([color : XML-Document]
   [data : XML-Document]
   [layout : XML-Document]
   [style : XML-Document])
  #:type-name MOX-Diagram
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct mox-drawingml moxml
  ([charts : (Listof XML-Document)]
   [shapes : (Listof XML-Document)]
   [diagrams : (Listof MOX-Diagram)]
   [theme : (Option XML-Document)]
   [theme-overrides : (Listof XML-Document)]
   [table-style : (Option XML-Document)])
  #:type-name MOX-DrawingML
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define moxml-drawingml-agent : (MOXML-Agentof MOX-DrawingML)
  (lambda [pkg-type]
    (define &chartSpaces : (Boxof (Listof XML-Document)) (box null))
    (define &userShapes : (Boxof (Listof XML-Document)) (box null))
    (define &styleDef : (Boxof (Option XML-Document)) (box #false))
    (define &tblStyleLst : (Boxof (Option XML-Document)) (box #false))
    (define &ossOverride : (Boxof (Listof XML-Document)) (box null))

    (define &colorsDefs : (Boxof (Listof XML-Document)) (box null))
    (define &dataModels : (Boxof (Listof XML-Document)) (box null))
    (define &layoutDefs : (Boxof (Listof XML-Document)) (box null))
    (define &styleDefs : (Boxof (Listof XML-Document)) (box null))
   
    
    (values 'drawingml

            (λ [[entry : String] [type : Symbol] [/dev/pkgin : Input-Port]] : (Option Void)
              (case type
                [(application/vnd.openxmlformats-officedocument.theme+xml)
                 (set-box! &styleDef (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.presentationml.tableStyles+xml)
                 (set-box! &tblStyleLst (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.drawingml.chart+xml)
                 (set-box! &chartSpaces (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &chartSpaces)))]
                [(application/vnd.openxmlformats-officedocument.drawingml.chartshapes+xml)
                 (set-box! &userShapes (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &userShapes)))]
                [(application/vnd.openxmlformats-officedocument.drawingml.diagramColors+xml)
                 (set-box! &colorsDefs (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &colorsDefs)))]
                [(application/vnd.openxmlformats-officedocument.drawingml.diagramData+xml)
                 (set-box! &dataModels (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &dataModels)))]
                [(application/vnd.openxmlformats-officedocument.drawingml.diagramLayout+xml)
                 (set-box! &layoutDefs (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &layoutDefs)))]
                [(application/vnd.openxmlformats-officedocument.drawingml.diagramStyle+xml)
                 (set-box! &styleDefs (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &styleDefs)))]
                [(application/vnd.openxmlformats-officedocument.themeOverride+xml) ; for PresentationML
                 (set-box! &ossOverride (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &ossOverride)))]
                [else #false]))

            (λ [] : MOX-DrawingML
              (mox-drawingml (reverse (unbox &chartSpaces)) (reverse (unbox &userShapes))
                             (for/list : (Listof MOX-Diagram) ([c (in-list (reverse (unbox &colorsDefs)))]
                                                               [d (in-list (reverse (unbox &dataModels)))]
                                                               [l (in-list (reverse (unbox &layoutDefs)))]
                                                               [s (in-list (reverse (unbox &styleDefs)))])
                               (mox-diagram c d l s))
                             (unbox &styleDef) (reverse (unbox &ossOverride)) (unbox &tblStyleLst))))))

