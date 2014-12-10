; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: RouterWidth
; Parameter: TargetCellView (CVId)
; Return: TargetCellView (CVId)
;
; Calculates Router region width
 
defun( RouterWidth ( TargetCellView  )

;get old cell region data from cellview
(defvar CellRegionData 
  ( PlacementRegionsScrapeCellRegionDataFromCellView
    TargetCellView
    ( getq TargetCellView instances )
    NChainLibCellPairRegExs
    PChainLibCellPairRegExs
    GateLibCellPairRegExs
    NPlugLibCellPairRegExs
    PPlugLibCellPairRegExs
    ( times UserUnitsPerMeter EvilGatePRegionOffset )
    ( times UserUnitsPerMeter MinWellSpacing )
    TargetCellView->prBoundary
    ( times LeafCellMinHorizontalSpacingToBoundary UserUnitsPerMeter )
    ( times LeafCellMinVerticalSpacingToBoundary UserUnitsPerMeter )
    ) )

;the minimum of the metal wire pitches
   RouterWidthPitch = DefaultWirePitch * UserUnitsPerMeter

;setup the initial gap search table
(unless RouterGapWidthInitialSearchTable
  (defvar RouterGapWidthInitialSearchTable 
    ( BinarySearchMakeTable 
      0 
      ( min 
        16
        ( floor
          ( quotient
            ( difference
              BestRoutedViewArea
              ( RectGetArea 
                ( PinUtilFindPRBoundBBox
                  BoundaryLPP
                  TargetCellView ) ) )
            ( times 
              YPitch
              RouterWidthPitch 
              ( plus 0.999 
; use plus 0.999 instead of plus 1, due to unpredictable behavior of 
; floor(2.0) might give 1 or 2 depending on the machine rounding.
                     ( length ( PlacementRegionsGetCellRegionDataColumnList CellRegionData ) ) )
              ) ) ) )
        0 ) )
    (defvar RouterGapWidthSearchTableList
    ( ListFillList
      (lambda ( I )
        RouterGapWidthInitialSearchTable )
      ( plus 1 ( length ( PlacementRegionsGetCellRegionDataColumnList CellRegionData ) ) ) ) )
  (defvar CurrentSearchTable RouterGapWidthInitialSearchTable ) )

;setup the individual gap search table
(when ( and InitialRoutingSearchDone
            ( equal CurrentSearchTable RouterGapWidthInitialSearchTable ) )
  (let (
        ( Max ( arrayref CurrentSearchTable "trial" ) ) )
    (defvar RouterGapWidthSearchTableList
      ( ListFillList
        (lambda ( I )
          ( BinarySearchMakeTable 0 Max 0 ?MaxTested t ) )
        ( plus 1
               ( length ( PlacementRegionsGetCellRegionDataColumnList CellRegionData ) ) ) ) )
    (defvar CurrentSearchTable ( car RouterGapWidthSearchTableList ) ) ) )

;setup the current gap width list
(defvar RouterGapWidthList 
  ( mapcar 
    ( lambda ( RouterGapWidthTable )
      ( times 
        (if ( equal CurrentSearchTable RouterGapWidthTable )
            ( arrayref RouterGapWidthTable "trial" )
          ( arrayref RouterGapWidthTable "max" ) )
        RouterWidthPitch ) )
    RouterGapWidthSearchTableList ) )

(defvar CellRegionData 
  ( SquishSquishCellRegionDataAndComponents 
    TargetCellView
    CellRegionData
    ( times UserUnitsPerMeter MinRegionWidth )
    ( times UserUnitsPerMeter MinWellSpacing )
    ( times UserUnitsPerMeter 
            ( CellInfoGetLayerWirePitchInMeters
              DirectiveTable
              Metal3LPP
              DirectiveUnitsPerMeter ) )
    LeftBuffer
    RightBuffer
    RouterGapWidthList
    GateLibCellPairRegExs
    ( cons 
      ( list WellPlugLibrary
             PWellPlugCellName )
      NChainLibCellPairRegExs  )
    ( cons 
      ( list WellPlugLibrary
             NWellPlugCellName )
      PChainLibCellPairRegExs  )
    WellPlugLibrary
    PWellPlugCellName 
    NWellPlugCellName 
    WellPlugViewName 
    NImplantLPP
    PImplantLPP
    ( times EvilGatePRegionOffset UserUnitsPerMeter )
    ( times ExtraGateWidth UserUnitsPerMeter )
    ContactLPP ) )

( dbSetq 
  ( PinUtilFindPRBoundShape
    BoundaryLPP
    TargetCellView )
  ( PlacementRegionsGetCellRegionDataBBox
    CellRegionData )
  bBox )

( dbSave TargetCellView )
 
TargetCellView
)

(defun RouterWidthFuncDefault ( RegionRect
                                Components
                                Gates
                                RouterWidth
                                RouterWidthPitch
                                RouterWidthSigmoidFactor
                                NumInstTermsAtHalfRouterWidth )
  (let (
        ( NumInstTerms 0 ) )
    ( foreach Component Components 
              ( setq NumInstTerms ( plus NumInstTerms ( length ( getq Component conns ) ) ) ) )
    ( foreach Gate Gates
              ( setq NumInstTerms 
                     ( plus NumInstTerms ( length 
                                           ( setof InstTerm ( getq Gate conns )
                                                   ( exists Pin ( getq ( getq InstTerm term ) pins )
                                                            ( RectDoRectsOverlap
                                                              RegionRect
                                                              ( dbTransformBBox ( getq ( getq Pin fig ) bBox ) 
                                                                                ( getq ( getq InstTerm inst ) transform ) ) ) ) ) ) ) ) )
    (let (
          ( Alpha ( quotient 
                    1.0
                    ( plus 
                      1.0
                      ( exp
                        ( times
                          RouterWidthSigmoidFactor
                          ( difference  
                            ( float NumInstTermsAtHalfRouterWidth )
                            ( float NumInstTerms ) ) ) ) ) ) ) )
      (let (
            ( Ret ( times Alpha RouterWidth RouterWidthPitch ) ) )
        ( printf "# instance terminals in column: %d -> %f\n" NumInstTerms Ret )
        Ret ) ) ) )


