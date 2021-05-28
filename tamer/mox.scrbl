#lang scribble/manual

@(require mox/scribble)

@mox-handbook-title/pkg-desc[#:category "Handbook" #:status "Draft" #:type "Manual"]

@author{Gyoudmon}

@itemlist[
 #:style 'compact

 @item{@tech{story}: each unique region of content within a document into which the user can type.

  @itemlist[#:style 'compact

            @item{@tech{main document}: containing the primart contents of the document.}]}]

@section{Heading 1}

@subsection{Heading 2}

@handbook-smart-table[]

@handbook-appendix[#:index-section? #true
 (url-bib-entry "OpenXML" "Standard ECMA-376: Office Open XML File Formats"
                "http://www.ecma-international.org/publications/standards/Ecma-376.htm"
                #:author "ECMA International"
                #:date 2008)]
