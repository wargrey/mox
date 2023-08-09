#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "header.rkt")
(require "directory.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CFB-Object-Info (List String Symbol Natural Index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct cfb-tree-node
  ([entry : Index]
   [children : (Listof CFB-Tree-Node)])
  #:type-name CFB-Tree-Node
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cfb-directory-tree-restore : (-> (Vectorof CFB-Directory-Entry) (Listof CFB-Object-Info))
  (lambda [entries]
    (define (cfb-directory-index->tree-nodes [idx : Index]) : (Listof CFB-Tree-Node)
      (define entry (vector-ref entries idx))
      (define left (cfb-directory-entry-left-sibling entry))
      (define right (cfb-directory-entry-right-sibling entry))
      
      ((inst sort CFB-Tree-Node CFB-Directory-Entry)
       (cons (cfb-directory-index->tree-node idx)
             (apply append
                    (map cfb-directory-index->tree-nodes
                         (filter index?
                                 (list (and (cfb-regular-id? left) left)
                                       (and (cfb-regular-id? right) right))))))
       cfb-directory-ci<?
       #:key (λ [[tn : CFB-Tree-Node]] : CFB-Directory-Entry
               (vector-ref entries (cfb-tree-node-entry tn)))
       #:cache-keys? #true))

    (define (cfb-directory-index->tree-node [idx : Index]) : CFB-Tree-Node
      (define child (cfb-directory-entry-child (vector-ref entries idx)))

      (if (cfb-regular-id? child)
          (cfb-tree-node idx (cfb-directory-index->tree-nodes child))
          (cfb-tree-node idx null)))

    (define tree : CFB-Tree-Node (cfb-directory-index->tree-node 0))

    (for/list ([pidx (in-list (cfb-tree-node->path-element-indices tree))])
      (define entry (vector-ref entries (last pidx)))
      (list (cfb-path-element-indices->path entries pidx)
            (cfb-directory-entry-type entry)
            (bitwise-and (cfb-directory-entry-stream-size entry) #xFFFFFFFFFF)
            (cfb-directory-entry-sector-chain-head entry)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cfb-tree-node->path-element-indices : (-> CFB-Tree-Node (Listof (Listof Index)))
  (lambda [node]
    (define children : (Listof CFB-Tree-Node) (cfb-tree-node-children node))
    (define index : Index (cfb-tree-node-entry node))
    
    (cond [(null? children) (list (list index))]
          [else (for/list ([subpaths (in-list (apply append (map cfb-tree-node->path-element-indices children)))])
                  (cons index subpaths))])))

(define cfb-path-element-indices->size : (-> (Vectorof CFB-Directory-Entry) (Listof Index) String)
  (lambda [entries indices]
    (for/fold ([pstr : String ""])
              ([pidx (in-list indices)])
      (define entry (vector-ref entries pidx))
      (define name (cfb-directory-name entry))
      
      (case (cfb-directory-entry-type entry)
        [(Root) (if (string=? pstr "") "/" pstr)]
        [(User-Storage) (string-append pstr name "/")]
        [(User-Stream) (string-append pstr name)]
        [else pstr]))))

(define cfb-path-element-indices->path : (-> (Vectorof CFB-Directory-Entry) (Listof Index) String)
  (lambda [entries indices]
    (for/fold ([pstr : String ""])
              ([pidx (in-list indices)])
      (define entry (vector-ref entries pidx))
      (define name (cfb-directory-name entry))
      
      (case (cfb-directory-entry-type entry)
        [(Root) (if (string=? pstr "") "/" pstr)]
        [(User-Storage) (string-append pstr name "/")]
        [(User-Stream) (string-append pstr name)]
        [else pstr]))))
