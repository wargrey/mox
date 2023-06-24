#lang typed/racket/base

(provide (all-defined-out))

(require sgml/digitama/xexpr/grammar)

(require "../../../dialect.rkt")
(require "../../../shared/ml/common-simple-types.rkt")

(require "base.rkt")
(require "line.rkt")
(require "fill.rkt")
(require "hyperlink.rkt")
(require "format.rkt")
(require "extension.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-xml-enumeration black-white-mode : Black-White-Mode #:for mox
  [clr auto gray ltGray invGray grayWhite blackGray blackWhite black white hidden])

(define-xml-enumeration shape-type : Shape-Type #:for mox
  [line lineInv triangle rtTriangle rect diamond parallelogram trapezoid nonIsoscelesTrapezoid
        pentagon hexagon heptagon octagon decagon dodecagon star4 star5 star6 star7 star8 star10
        star12 star16 star24 star32 roundRect round1Rect round2SameRect round2DiagRect snipRoundRect
        snip1Rect snip2SameRect snip2DiagRect plaque ellipse teardrop homePlate chevron pieWedge
        pie blockArc donut noSmoking rightArrow leftArrow upArrow downArrow stripedRightArrow
        notchedRightArrow bentUpArrow leftRightArrow upDownArrow leftUpArrow leftRightUpArrow
        quadArrow leftArrowCallout rightArrowCallout upArrowCallout downArrowCallout
        leftRightArrowCallout upDownArrowCallout quadArrowCallout bentArrow uturnArrow
        circularArrow leftCircularArrow leftRightCircularArrow curvedRightArrow curvedLeftArrow
        curvedUpArrow curvedDownArrow swooshArrow cube can lightningBolt heart sun moon smileyFace
        irregularSeal1 irregularSeal2 foldedCorner bevel frame halfFrame corner diagStripe chord
        arc leftBracket rightBracket leftBrace rightBrace bracketPair bracePair straightConnector1
        bentConnector2 bentConnector3 bentConnector4 bentConnector5 curvedConnector2 curvedConnector3
        curvedConnector4 curvedConnector5 callout1 callout2 callout3 accentCallout1 accentCallout2
        accentCallout3 borderCallout1 borderCallout2 borderCallout3 accentBorderCallout1
        accentBorderCallout2 accentBorderCallout3 wedgeRectCallout wedgeRoundRectCallout
        wedgeEllipseCallout cloudCallout cloud ribbon ribbon2 ellipseRibbon ellipseRibbon2
        leftRightRibbon verticalScroll horizontalScroll wave doubleWave plus flowChartProcess
        flowChartDecision flowChartInputOutput flowChartPredefinedProcess flowChartInternalStorage
        flowChartDocument flowChartMultidocument flowChartTerminator flowChartPreparation
        flowChartManualInput flowChartManualOperation flowChartConnector flowChartPunchedCard
        flowChartPunchedTape flowChartSummingJunction flowChartOr flowChartCollate flowChartSort
        flowChartExtract flowChartMerge flowChartOfflineStorage flowChartOnlineStorage
        flowChartMagneticTape flowChartMagneticDisk flowChartMagneticDrum flowChartDisplay
        flowChartDelay flowChartAlternateProcess flowChartOffpageConnector actionButtonBlank
        actionButtonHome actionButtonHelp actionButtonInformation actionButtonForwardNext
        actionButtonBackPrevious actionButtonEnd actionButtonBeginning actionButtonReturn
        actionButtonDocument actionButtonSound actionButtonMovie gear6 gear9 funnel mathPlus
        mathMinus mathMultiply mathDivide mathEqual mathNotEqual cornerTabs squareTabs plaqueTabs
        chartX chartStar chartPlus])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-attribute connection #:for mox
  ([id : Index #:<-> xml:attr-value->drawing-element-id]
   [idx : Index #:<-> xml:attr-value->index]))

(define-mox-attribute shape-locking #:for mox
  ([noGrp : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noSelect : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noRot : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noChangeAspect : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noMove : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noResize : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noEditPoints : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noAdjustHandles : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noChangeArrowheads : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noChangeShapeType : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noTextEdit : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]))

(define-mox-attribute group-shape-locking #:for mox
  ([noGrp : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noUngrp : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noSelect : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noRot : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noChangeAspect : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noMove : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noResize : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]))

(define-mox-attribute connector-locking #:for mox
  ([noGrp : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noSelect : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noRot : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noChangeAspect : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noMove : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noResize : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noEditPoints : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noAdjustHandles : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noChangeArrowheads : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noChangeShapeType : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]))

(define-mox-attribute graphical-object-frame-locking #:for mox
  ([noGrp : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noDrilldown : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noSelect : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noChangeAspect : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noMove : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noResize : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]))

(define-mox-attribute picture-locking #:for mox
  ([noGrp : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noSelect : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noRot : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noChangeAspect : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noMove : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noResize : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noEditPoints : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noAdjustHandles : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noChangeArrowheads : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noChangeShapeType : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [noCrop : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-element transform2d #:for mox
  #:attlist
  ([rot : Fixnum #:= [#false 0] #:<-> mox:attr-value->angle]
   [flipH : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [flipV : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean])
  ([off : (Option MOX#Point2d) #false]
   [ext : (Option MOX#Positive-Size2d) #false]))

(define-mox-element group-transform2d #:for mox
  #:attlist
  ([rot : Fixnum #:= [#false 0] #:<-> mox:attr-value->angle]
   [flipH : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [flipV : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean])
  ([off : (Option MOX#Point2d) #false]
   [ext : (Option MOX#Positive-Size2d) #false]
   [chOff : (Option MOX#Point2d) #false]
   [chExt : (Option MOX#Positive-Size2d) #false]))

(define-mox-element shape-style #:for mox
  ([line : MOX#Style-Matrix-Reference]
   [fill : MOX#Style-Matrix-Reference]
   [effect : MOX#Style-Matrix-Reference]
   [font : MOX#Font-Reference]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-element group-shape-property #:for mox
  #:attlist
  ([bwMode : Black-White-Mode #:= [#false 'white] #:<-> mox:attr-value->black-white-mode])
  ([xfrm : (Option MOX:Group-Transform2d) #false]
   [fill : (Option MOX-Fill-Property) #false]
   [extLst : (Option MOX:Office-Art-Extension-List) #false]))

(define-mox-element shape-property #:for mox
  #:attlist
  ([bwMode : Black-White-Mode #:= [#false 'white] #:<-> mox:attr-value->black-white-mode])
  ([xfrm : (Option MOX:Group-Transform2d) #false]
   [fill : (Option MOX-Fill-Property) #false]
   [ln : (Option MOX:Line-Property) #false]
   [extLst : (Option MOX:Office-Art-Extension-List) #false]))

(define-mox-element nvisual-canvas-property #:for mox
  #:attlist
  ([id : Index #:<-> xml:attr-value->drawing-element-id]
   [name : String #:<-> xml:attr-value->string]
   [descr : String #:= #false #:<-> xml:attr-value->string]
   [hidden : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [title : String #:= #false #:<-> xml:attr-value->string])
  ([hlinkClick : (Option MOX:Hyperlink) #false]
   [hlinkHover : (Option MOX:Hyperlink) #false]
   [extLst : (Option MOX:Office-Art-Extension-List) #false]))

(define-mox-element nvisual-shape-property #:for mox
  #:attlist
  ([txBox : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean])
  ([spLocks : (Option (MOX-Art-Extension-With (Option MOX#Shape-Locking))) #false]
   [extLst : (Option MOX:Office-Art-Extension-List) #false]))

(define-mox-element nvisual-group-shape-property #:for mox
  ([grpSpLocks : (Option (MOX-Art-Extension-With (Option MOX#Group-Shape-Locking))) #false]
   [extLst : (Option MOX:Office-Art-Extension-List) #false]))

(define-mox-element nvisual-graphic-frame-property #:for mox
  ([graphicFrameLocks : (Option (MOX-Art-Extension-With (Option MOX#Graphical-Object-Frame-Locking))) #false]
   [extLst : (Option MOX:Office-Art-Extension-List) #false]))

(define-mox-element nvisual-picture-property #:for mox
  #:attlist
  ([preferRelativeResize : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean])
  ([picLocks : (Option (MOX-Art-Extension-With (Option MOX#Graphical-Object-Frame-Locking))) #false]
   [extLst : (Option MOX:Office-Art-Extension-List) #false]))

(define-mox-element nvisual-connector-property #:for mox
  ([cxnSpLocks : (Option (MOX-Art-Extension-With (Option MOX#Connector-Locking))) #false]
   [stCxn : (Option MOX#Connection) #false]
   [endCxn : (Option MOX#Connection) #false]
   [extLst : (Option MOX:Office-Art-Extension-List) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-element graphical-object-data #:for mox
  #:attlist
  ([uri : String #:<-> xml:attr-value->token])
  ([body : XML-Element]))
