#lang scribble/manual

@(require digimon/tamer)
@(require mox/digitama/scribble/package/core)

@handbook-title/pkg-desc[
 #:properties (make-mox-metainfo #:category "category"
                                 #:status "status" #:type "type"
                                 #:created "created" #:creator "creator"
                                 #:description "description"
                                 #:identifier "identifier"
                                 #:keywords (list 'key 'words)
                                 #:langauge 'en)
   [language : (Option Symbol) #false]
   [last-modifier : (Option String) #false]
   [last-printed : (Option String) #false]
   [modified : (Option String) #false]
   [revision : (Option Natural) #false]
   [subject : (Option String) #false]
   [title : (Option String) #false]
   [version : (Option String) #false])]

@handbook-smart-table[]

@handbook-appendix[#:index-section? #true
 (url-bib-entry "OpenXML" "Standard ECMA-376: Office Open XML File Formats"
                "http://www.ecma-international.org/publications/standards/Ecma-376.htm"
                #:author "ECMA International"
                #:date 2008)]
