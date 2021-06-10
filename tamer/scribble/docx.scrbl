#lang scribble/manual

@(require mox/scribble)
@(require mox/docx-render)

@(require (for-label scribble/core))
@(require (for-label scribble/base-render))

@handbook-story{Scribble Renders}
@defmodule[mox/docx-render]

@defmixin[render-mixin (render<%>) ()]{Specializes a @racket[render<%>] class for generating docx output.}

@handbook-scenario{Gallery}

@centered{The rest demonstrates all supported @racket[element]s.}

@nested[#:style 'inset]{There is a hspace(@elem[#:style 'hspace]{hspace}) element here,
 followed by a newline @elem[#:style 'newline]{hspace} element.

 x@subscript{1}@superscript{2} @emph{emph} @tt{tt}.}

@itemlist[
 #:style 'compact

 @item{@deftech{story}: each unique region of content within a document into which the user can type.

  @itemlist[#:style 'compact

            @item{@deftech{main document}: containing the primary contents of the document.}]}]
