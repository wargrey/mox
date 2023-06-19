#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xml)

(require "../moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xlsx-name : Symbol 'xlsx)

(struct excel-pivot-table
  ([self : XML-Document]
   [cache-definition : XML-Document]
   [cache-record : XML-Document])
  #:type-name Excel-Pivot-Table
  #:transparent)

(struct excel-revision
  ([header : XML-Document]
   [logs : (Listof XML-Document)])
  #:type-name Excel-Revision
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Missing Parts
; custom property part, custom XML mappings, dialogsheet

(struct mox-excel moxml
  ([workbook : XML-Document]
   [worksheets : (Listof XML-Document)]
   [strings : XML-Document]
   [chain : (Option XML-Document)]
   [style : (Option XML-Document)]
   [volatile : (Option XML-Document)]
   
   [tables : (Listof XML-Document)]
   [single-cell-table : (Option XML-Document)]
   [chartsheets : (Listof XML-Document)]
   [drawings : (Listof XML-Document)]
   
   [comments : (Listof XML-Document)]
   [revision : (Option Excel-Revision)]
   [users : (Option XML-Document)]

   ; external and remote
   [externals : (Listof XML-Document)]
   [connections : (Option XML-Document)]
   [metadata : (Option XML-Document)]
   [query-tables : (Listof XML-Document)]
   [pivot-tables : (Listof Excel-Pivot-Table)])
  #:type-name MOX-Excel
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define moxml-excel-agent : (MOXML-Agentof MOX-Excel)
  (lambda [pkg-type]
    (define &workbook : (Boxof XML-Document) (box empty-workbook))
    (define &worksheets : (Boxof (Listof XML-Document)) (box null))
    (define &sst : (Boxof XML-Document) (box empty-shared-string-table))
    (define &calcChain : (Boxof (Option XML-Document)) (box #false))
    (define &styleSheet : (Boxof (Option XML-Document)) (box #false))
    (define &volTypes : (Boxof (Option XML-Document)) (box #false))

    (define &tables : (Boxof (Listof XML-Document)) (box null))
    (define &singleCells : (Boxof (Option XML-Document)) (box #false))
    (define &chartsheets : (Boxof (Listof XML-Document)) (box null))
    (define &wsDrs : (Boxof (Listof XML-Document)) (box null))

    (define &comments : (Boxof (Listof XML-Document)) (box null))
    (define &headers : (Boxof (Option XML-Document)) (box #false))
    (define &revisions : (Boxof (Listof XML-Document)) (box null))
    (define &users : (Boxof (Option XML-Document)) (box #false))
    
    (define &externalLinks : (Boxof (Listof XML-Document)) (box null))
    (define &queryTables : (Boxof (Listof XML-Document)) (box null))
    (define &connections : (Boxof (Option XML-Document)) (box #false))
    (define &metadata : (Boxof (Option XML-Document)) (box #false))

    (define &pivotTableDefinitions : (Boxof (Listof XML-Document)) (box null))
    (define &pivotCacheDefinitions : (Boxof (Listof XML-Document)) (box null))
    (define &pivotCacheRecords : (Boxof (Listof XML-Document)) (box null))
    
    (values xlsx-name

            (λ [[entry : String] [type : Symbol] [/dev/pkgin : Input-Port]] : (Option Void)
              (case type
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.table+xml)
                 (set-box! &tables (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &tables)))]
                [(application/vnd.openxmlformats-officedocument.drawing+xml)
                 (set-box! &wsDrs (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &wsDrs)))]
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.chartsheet+xml)
                 (set-box! &chartsheets (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &chartsheets)))]
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml)
                 (set-box! &worksheets (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &worksheets)))]
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml
                  application/vnd.openxmlformats-officedocument.spreadsheetml.template.main+xml)
                 (set-box! &workbook (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.calcChain+xml)
                 (set-box! &calcChain (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.revisionLog+xml)
                 (set-box! &revisions (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &revisions)))]
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.comments+xml)
                 (set-box! &comments (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &comments)))]
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.volatileDependencies+xml)
                 (set-box! &volTypes (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml)
                 (set-box! &styleSheet (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.revisionHeaders+xml)
                 (set-box! &headers (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.userNames+xml)
                 (set-box! &users (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.tableSingleCells+xml)
                 (set-box! &singleCells (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.externalLink+xml)
                 (set-box! &externalLinks (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &externalLinks)))]
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.queryTable+xml)
                 (set-box! &queryTables (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &queryTables)))]
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.connections+xml)
                 (set-box! &connections (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.sheetMetadata+xml)
                 (set-box! &metadata (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.pivotTable+xml)
                 (set-box! &pivotTableDefinitions (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &pivotTableDefinitions)))]
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.pivotCacheDefinition+xml)
                 (set-box! &pivotCacheDefinitions (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &pivotCacheDefinitions)))]
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.pivotCacheRecords+xml)
                 (set-box! &pivotCacheRecords (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &pivotCacheRecords)))]
                [else #false]))

            (λ [] : MOX-Excel
              (mox-excel (unbox &workbook) (unbox &worksheets)
                         (unbox &sst) (unbox &calcChain)
                         (unbox &styleSheet)
                         (unbox &volTypes)

                         (reverse (unbox &tables))
                         (unbox &singleCells)
                         (reverse (unbox &chartsheets)) (reverse (unbox &wsDrs))

                         (reverse (unbox &comments))
                         (let ([header (unbox &headers)])
                           (and header
                                (excel-revision header (reverse (unbox &revisions)))))
                         (unbox &users)
                         
                         (reverse (unbox &externalLinks))
                         (unbox &connections) (unbox &metadata) (reverse (unbox &queryTables))
                         (for/list : (Listof Excel-Pivot-Table) ([pd (in-list (reverse (unbox &pivotTableDefinitions)))]
                                                                 [pcd (in-list (reverse (unbox &pivotCacheDefinitions)))]
                                                                 [pcr (in-list (reverse (unbox &pivotCacheRecords)))])
                           (excel-pivot-table pd pcd pcr)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define empty-workbook : XML-Document (xml-blank 'workbook))
(define empty-shared-string-table : XML-Document (xml-blank 'sst))
