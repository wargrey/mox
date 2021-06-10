#lang scribble/manual

@(require mox/scribble)

@mox-handbook-title/pkg-desc[#:category "Handbook" #:status "Draft" #:type "Manual"]

@handbook-smart-table[]

@include-section{scribble/docx.scrbl}

@handbook-appendix[#:index-section? #true
 (url-bib-entry "OpenXML" "Standard ECMA-376: Office Open XML File Formats"
                "http://www.ecma-international.org/publications/standards/Ecma-376.htm"
                #:author "ECMA International"
                #:date 2008)]
