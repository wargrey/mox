#lang typed/racket/base

(provide (all-defined-out))

(require racket/vector)

(require digimon/stdio)
(require digimon/port)

(require "header.rkt")
(require "directory.rkt")
(require "tree.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct ms-cfb
  ([header : CFB-Header]
   [difat : (Vectorof Index)]
   [fat : (Vectorof Index)]
   [minifat : (Vectorof Index)]
   [directories : (Vectorof CFB-Directory-Entry)]
   [objects : (Listof CFB-Object-Info)]
   [stream-index->block : (-> Natural (Values Natural Natural))]
   [ministream-index->block : (-> Natural (Values Natural Natural))])
  #:transparent
  #:type-name MS-CFB)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-compound-file : (-> Input-Port MS-CFB)
  (lambda [/dev/cfbin]
    (define header (read-cfb-header /dev/cfbin))
    (define sector-size (cfb-sector-size (cfb-header-lnsector-size header)))
    (define short-sector-size (cfb-sector-size (cfb-header-lnshort-sector-size header)))
    (define difat (read-difat-array /dev/cfbin sector-size header))

    (define fat : (Vectorof Index)
      (read-fat-array /dev/cfbin sector-size
                      (vector-filter cfb-regular-id? difat)))
    
    (define minifat : (Vectorof Index)
      (read-fat-array /dev/cfbin sector-size
                      (cfb-sector-chain (cfb-header-minifat-chain-head header) fat)))

    (define dirs : (Vectorof CFB-Directory-Entry)
      (read-directory-entries /dev/cfbin sector-size
                              (cfb-sector-chain (cfb-header-directory-chain-head header) fat)))

    (define-values (ministream-container-size sector-chain-header)
      (if (> (vector-length dirs) 0)
          (let ([root (vector-ref dirs 0)])
            (values (cfb-directory-entry-stream-size root)
                    (cfb-directory-entry-sector-chain-head root)))
          (values 0 (assert #%cfb-end-of-chain index?))))

    (ms-cfb header difat fat minifat dirs
            (cfb-directory-tree-restore dirs)
            (cfb-make-stream-index->block sector-size)
            (cfb-make-minstream-index->block sector-chain-header fat
                                             sector-size short-sector-size
                                             ministream-container-size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-directory-entries : (-> Input-Port Index (Listof Index) (Vectorof CFB-Directory-Entry))
  (lambda [/dev/cfbin sector-size secids]
    (define count (quotient sector-size (sizeof-cfb-directory-entry)))

    (parameterize ([default-stdin-locale "UTF-16LE"])
      (apply vector-append
             (for/list : (Listof (Vectorof CFB-Directory-Entry)) ([secid (in-list secids)])
               (port-seek /dev/cfbin (cfb-sector-position secid sector-size))

               (for/vector : (Vectorof CFB-Directory-Entry) ([i (in-range count)])
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
    (define next-sector-id : Index (cfb-header-ext-difat-chain-head cfb))

    (for ([i (in-range (vector-length leading-difats))])
      (vector-set! leading-difats i
                   (read-luint32 /dev/cfbin)))

    (case (cfb-header-version cfb)
      [(#x04) (drop-bytes /dev/cfbin 3584)])

    (if (< next-sector-id #%cfb-max-id)
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
      (if (< next-sid #%cfb-max-id)
          (vector-append chained-difats
                         (read-extended-difat-array /dev/cfbin sector-size
                                                    next-sid))
          chained-difats))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cfb-make-stream-index->block : (-> Index (-> Natural (Values Natural Natural)))
  (lambda [sector-size]
    (λ [[sid : Natural]] : (Values Natural Natural)
      (values (cfb-sector-position sid sector-size)
              sector-size))))

(define cfb-make-minstream-index->block : (-> Index (Vectorof Index) Index Index Natural
                                          (-> Natural (Values Natural Natural)))
  (lambda [sid0 ids sector-size mini-sector-size total]
    (define chains (cfb-sector-chain sid0 ids))
    (define n/sector (quotient sector-size mini-sector-size))
    
    (λ [[ssid0 : Natural]] : (Values Natural Natural)
      (define-values (sidx ssid) (quotient/remainder ssid0 n/sector))
      
      (values (+ (cfb-sector-position (list-ref chains sidx) sector-size)
                 (* ssid mini-sector-size))
              mini-sector-size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cfb-sector-chain : (-> Index (Vectorof Index) (Listof Index))
  (lambda [sid0 ids]
    (define maxidx (vector-length ids))

    (let chain ([cid : Index sid0])
      (if (< cid maxidx)
          (cons cid (chain (vector-ref ids cid)))
          null))))

(define cfb-sector-size : (-> Index Index)
  (lambda [shift]
    (assert (expt 2 shift) index?)))

(define cfb-sector-position : (-> Natural Index Natural)
  (lambda [secid sector-size]
    (+ 512 (* secid sector-size))))
