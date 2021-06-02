#lang scribble/manual

@(require mox/scribble)

@mox-handbook-title/pkg-desc[#:category "Handbook" #:status "Draft" #:type "Manual"]

There is a hspace(@elem[#:style 'hspace]{hspace}) element here,
followed by a newline @elem[#:style 'newline]{hspace} element.

x@subscript{1}@superscript{2} @emph{emph} @tt{tt}.

@itemlist[
 #:style 'compact

 @item{@deftech{story}: each unique region of content within a document into which the user can type.

  @itemlist[#:style 'compact

            @item{@deftech{main document}: containing the primary contents of the document.}]}]

@section{Heading 1}

this content refers to the @tech{story}.

@subsection{Heading 2}

this content refers to the @tech{main document}.

@handbook-smart-table[]

@handbook-appendix[#:index-section? #true
 (url-bib-entry "OpenXML" "Standard ECMA-376: Office Open XML File Formats"
                "http://www.ecma-international.org/publications/standards/Ecma-376.htm"
                #:author "ECMA International"
                #:date 2008)]
