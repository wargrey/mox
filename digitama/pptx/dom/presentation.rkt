#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require sgml/sax)

(require "../../mox/dialect.rkt")
(require "../../mox/datatype.rkt")
(require "datatype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-attribute presentation #:for pptx
  ([serverZoom : Flonum #:= [#false 0.5] #:<-> xml:attr-value->flonum]
   [firstSlideNum : Index #:= [#false 1] #:<-> xml:attr-value->index]
   [showSpecialPlsOnTitleSld : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean]
   [rtl : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [removePersonalInfoOnSave : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [compatMode : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [strictFirstAndLastChars : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean]
   [embedTrueTypeFonts : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [saveSubsetFonts : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [autoCompressPictures : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean]
   [bookmarkIdSeed : Index #:= [#false 1] #:<-> xml:attr-value->index]
   [conformance : PPTX-Conformance-Class #:= #false #:<-> xml:attr-value->pptx-conformance-class]))

(define-mox-attribute slide-master-entry #:for pptx
  ([id : Natural #:= [] #:<-> mox:attr-value->slide-master-id]
   [r:id : Symbol #:= [] #:<-> mox:attr-value->relationship-id]))

(define-mox-attribute slide-entry #:for pptx
  ([id : Natural #:= [] #:<-> mox:attr-value->slide-id]
   [r:id : Symbol #:= [] #:<-> mox:attr-value->relationship-id]))

(define-mox-attribute slide-size #:for pptx
  ([cx : Index #:<-> mox:attr-value->slide-coordinate]
   [cy : Index #:<-> mox:attr-value->slide-coordinate]
   [type : PPTX-Slide-Size-Type #:= [#false 'custom] #:<-> xml:attr-value->pptx-slide-size-type]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct pptx-presentation : PPTX-Presentation
  ([namespaces : MOX-Namespaces null]
   [attribute : (Option PPTX:Attr:Presentation) #false]
   [slide-masters : (Listof PPTX:Attr:Slide-Master-Entry) null]
   [slides : (Listof PPTX:Attr:Slide-Entry) null]
   [slide-size : (Option PPTX:Attr:Slide-Size) #false]
   [notes-size : (Pairof Index Index) (cons 0 0)])
  #:transparent)

(define empty-presentation : PPTX-Presentation
  (make-pptx-presentation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fundamentals And Markup Language References 19.2
(define presentation-sax-element : (XML-Element-Handler PPTX-Presentation)
  (λ [element depth attrs ?empty ?preserve self]
    (if (and attrs)
        (case element
          [(p:presentation)
           (let*-values ([(ns rest) (mox-attributes-extract-xmlns attrs)]
                         [(pattr rest) (extract-pptx:attr:presentation rest)])
             (remake-pptx-presentation self #:namespaces ns #:attribute pattr))]
          [(p:sldMasterId)
           (let-values ([(mid rest) (extract-pptx:attr:slide-master-entry attrs)])
             (remake-pptx-presentation self #:slide-masters (cons (assert mid) (pptx-presentation-slide-masters self))))]
          [(p:sldId)
           (let-values ([(sid rest) (extract-pptx:attr:slide-entry attrs)])
             (remake-pptx-presentation self #:slides (cons (assert sid) (pptx-presentation-slides self))))]
          [(p:sldSz)
           (let-values ([(sldsz rest) (extract-pptx:attr:slide-size attrs)])
             (remake-pptx-presentation self #:slide-size sldsz))]
          [(p:notesSz)
           (let-values ([(ntsz rest) (mox-attributes-extract-positive-2dsize attrs 'cx 'cy)])
             (remake-pptx-presentation self #:notes-size (or ntsz (pptx-presentation-notes-size self))))]
          [else #| TODO: p:defaultTextStyle |# self])

        #| ETag |#
        (case element
          [(p:sldMasterIdLst) (remake-pptx-presentation self #:slide-masters (reverse (pptx-presentation-slide-masters self)))]
          [(p:sldIdLst) (remake-pptx-presentation self #:slides (reverse (pptx-presentation-slides self)))]
          [else self]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pptx-presentation-sax-handler : (XML-Event-Handlerof PPTX-Presentation)
  ((inst make-xml-event-handler PPTX-Presentation)
   #:element presentation-sax-element))
