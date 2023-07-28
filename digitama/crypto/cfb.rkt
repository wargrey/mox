#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/vector)

(require digimon/enumeration)
(require digimon/stdio)
(require digimon/port)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #%cfb-identifier : Natural #xD0CF11E0A1B11AE1)
(define #%cfb-max-sector-id : Natural #xFFFFFFFA)
#;(define #%cfb-reserved : Natural #xFFFFFFFB)
(define #%cfb-difat : Natural #xFFFFFFFC)
(define #%cfb-fat : Natural #xFFFFFFFD)
(define #%cfb-end-of-chain : Natural #xFFFFFFFE)
(define #%cfb-free : Natural #xFFFFFFFF)

(define-enumeration* cfb-directory-type #:+> CFB-Directory-Type
  directory-type->number number->directory-type
  [0 Empty User-Storage User-Stream Lock-Bytes Property Root-Storage])

(define-enumeration* cfb-directory-color #:+> CFB-Directory-Color
  directory-color->number number->directory-color
  [0 Red Black])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-binary-struct cfb-header : CFB-Header
  ([identifier : MUInt64 #:signature #%cfb-identifier]
   [clsid : (#:raw 16) #:default #""]
   [revision : LUInt16 #:radix 16 #:default #x003E]
   [version : LUInt16 #:radix 16 #:default #x0003]
   [byte-order : MUInt16 #:radix 16 #:default #xFEFF]
   [lnsector-size : LUInt16 #:default 9]
   [lnshort-sector-size : LUInt16 #:default 6]
   [0x22 : (#:reserved 6)]
   [directory-count : LUInt32 #:default 0]
   [fat-count : LUInt32]
   [directory0-secid : LUInt32 #:radix 16]
   [transaction : (#:unused 4)]
   [stream-minsize : LUInt32 #:default 4096]
   [minifat0-secid : LUInt32 #:radix 16]
   [minifat-count : LUInt32]
   [ext-difat0-secid : LUInt32 #:radix 16]
   [ext-difat-count : LUInt32]))

(define-binary-struct cfb-directory-entry : CFB-Directory-Entry
  ([name : (#:locale 64)]
   [name+size : LUInt16]
   [type : (#:enum Byte directory-type->number number->directory-type)]
   [color : (#:enum Byte directory-color->number number->directory-color)]
   [left-sibling : LUInt32 #:radix 16]
   [right-sibling : LUInt32 #:radix 16]
   [child : LUInt32 #:radix 16]
   [clsid : (#:raw 16) #:default #""]
   [user-flags : LUInt32 #:radix 2 #:default 0]
   [creation-time : LUInt64 #:default 0]
   [last-modification-time : LUInt64 #:default 0]
   [sector-id : LUInt32]

   ;;; NOTE
   ; For version 3, the type should be of `LUInt32`,
   ;   with another not used field of 4 bytes.
   ; In this implementation, the most signifciant 32bit
   ;   of `stream-size` is ignored.
   [stream-size : LUInt64]))

(struct ms-cfb
  ([header : CFB-Header]
   [difat : (Vectorof Index)]
   [fat : (Vectorof Index)]
   [minifat : (Vectorof Index)]
   [directories : (Listof CFB-Directory-Entry)])
  #:transparent
  #:type-name MS-CFB)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cfb-identifier-okay? : (-> Input-Port Boolean)
  (lambda [/dev/stdin]
    (equal? (peek-muint64 /dev/stdin)
            #%cfb-identifier)))

(define read-compound-file : (-> Input-Port MS-CFB)
  (lambda [/dev/cfbin]
    (define header (read-cfb-header /dev/cfbin))
    (define sector-size (cfb-sector-size (cfb-header-lnsector-size header)))
    (define short-sector-size (cfb-sector-size (cfb-header-lnshort-sector-size header)))
    (define difat (read-difat-array /dev/cfbin sector-size header))

    (define fat : (Vectorof Index)
      (read-fat-array /dev/cfbin sector-size
                      (vector-filter cfb-regular-sector-id? difat)))
    
    (define minifat : (Vectorof Index)
      (read-fat-array /dev/cfbin sector-size
                      (cfb-sector-chain (cfb-header-minifat0-secid header) fat)))

    (define dirs : (Listof CFB-Directory-Entry)
      (read-directory-entries /dev/cfbin sector-size
                              (cfb-sector-chain (cfb-header-directory0-secid header) fat)))

    (ms-cfb header difat fat minifat dirs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-directory-entries : (-> Input-Port Index (Listof Index) (Listof CFB-Directory-Entry))
  (lambda [/dev/cfbin sector-size secids]
    (define count (quotient sector-size (sizeof-cfb-directory-entry)))

    (parameterize ([default-stdin-locale "UTF-16LE"])
      (apply append
             (for/list : (Listof (Listof CFB-Directory-Entry)) ([secid (in-list secids)])
               (port-seek /dev/cfbin (cfb-sector-position secid sector-size))
               
               (for/list : (Listof CFB-Directory-Entry) ([i (in-range count)])
                 (read-cfb-directory-entry /dev/cfbin)))))))

(define read-fat-array : (-> Input-Port Index (U (Listof Index) (Vectorof Index)) (Vectorof Index))
  (lambda [/dev/cfbin sector-size secids]
    (define count (quotient sector-size 4))
    (define sec-seq : (Sequenceof Index)
      (if (list? secids)
          (in-list secids)
          (in-vector secids)))

    (apply vector-append
           (for/list : (Listof (Vectorof Index)) ([secid : Index sec-seq])
             (port-seek /dev/cfbin (cfb-sector-position secid sector-size))

             (let ([fats : (Vectorof Index) (make-vector count)])
               (for ([i (in-range count)])
                 (vector-set! fats i (read-luint32 /dev/cfbin)))
               fats)))))

(define read-difat-array : (-> Input-Port Index CFB-Header (Vectorof Index))
  (lambda [/dev/cfbin sector-size cfb]
    (define leading-difats : (Vectorof Index) (make-vector 109))
    (define next-sector-id : Index (cfb-header-ext-difat0-secid cfb))

    (for ([i (in-range (vector-length leading-difats))])
      (vector-set! leading-difats i
                   (read-luint32 /dev/cfbin)))

    (case (cfb-header-version cfb)
      [(#x04) (drop-bytes /dev/cfbin 3584)])

    (if (< next-sector-id #%cfb-max-sector-id)
        (vector-append leading-difats
                       (read-extended-difat-array /dev/cfbin sector-size
                                                  next-sector-id))
        leading-difats)))

(define read-extended-difat-array : (-> Input-Port Index Index (Vectorof Index))
  (lambda [/dev/cfbin sector-size next-secid]
    (define count (sub1 (quotient sector-size 4)))
    (define chained-difats : (Vectorof Index) (make-vector count))

    (port-seek /dev/cfbin (cfb-sector-position next-secid sector-size))
    
    (for ([i (in-range count)])
      (vector-set! chained-difats i
                   (read-luint32 /dev/cfbin)))

    (let ([next-sid (read-luint32 /dev/cfbin)])
      (if (< next-sid #%cfb-max-sector-id)
          (vector-append chained-difats
                         (read-extended-difat-array /dev/cfbin sector-size
                                                    next-sid))
          chained-difats))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cfb-sector-chain : (-> Index (Vectorof Index) (Listof Index))
  (lambda [sid0 ids]
    (define maxidx (vector-length ids))

    (let chain ([cid : Index sid0])
      (if (< cid maxidx)
          (cons cid (chain (vector-ref ids cid)))
          null))))

(define cfb-regular-sector-id? : (-> Index Boolean)
  (lambda [secid]
    (< secid #%cfb-max-sector-id)))

(define cfb-sector-size : (-> Index Index)
  (lambda [lnsize]
    (assert (expt 2 lnsize) index?)))

(define cfb-sector-position : (-> Natural Index Natural)
  (lambda [secid sector-size]
    (+ 512 (* secid sector-size))))
