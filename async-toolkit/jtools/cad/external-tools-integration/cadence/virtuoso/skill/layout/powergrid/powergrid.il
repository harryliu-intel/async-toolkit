; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$



(defun PowerGridFlattenOverlap ( Overlap )
  (let (
        ( Stack nil )
        ( CurrOverlap Overlap ) 
        ( Result nil ) )
    (while ( listp CurrOverlap )
      ( setq Stack ( cons ( car CurrOverlap ) Stack ) )
      ( setq CurrOverlap ( cadr CurrOverlap ) ) )
    (when CurrOverlap
      ( setq Result ( getq CurrOverlap bBox ) )
      (while Stack
        ( setq 
          Result
          ( dbTransformBBox Result ( getq ( car Stack ) transform ) ) )
        ( setq Stack ( cdr Stack ) ) ) )
    Result ) )




(defun PowerGridFlattenOverlaps ( Overlaps )
  ( ListApplyFuncToListAndAccumulateResults
    Overlaps
    (lambda
      ( Overlap )
      ( PowerGridFlattenOverlap Overlap ) )
    nil ) )

(defun PowerGridGetFlatListOfRectsOnLayer ( CellView LayerPP Levels )
  ( dbComputeBBox CellView )
  ( PowerGridFlattenOverlaps
    ( dbGetTrueOverlaps
      CellView
      ( getq CellView bBox )
      LayerPP
      Levels ) ) )

(defun PowerGridIsHorizontalWire ( WireRect )
  ( lessp
    ( RectGetHeight WireRect )
    ( RectGetWidth WireRect ) ) )


(defun PowerGridGetPitchOffset ( EdgeOfWire WireSpacing WirePitch )
  (let (
        ( HalfWireSpacing ( quotient WireSpacing 2.0 ) ) )
    (let (
          ( EdgeOfPitch ( difference EdgeOfWire HalfWireSpacing ) ) )
      ( difference
        ( times
          ( round
            ( quotient
              EdgeOfPitch
              WirePitch ) )
          WirePitch )
        EdgeOfPitch ) ) ) )



(defun PowerGridTranslatePositionToPitch ( Position
                                           WirePitch
                                           PitchOffset )
  ( floor
    ( quotient
      ( float
        ( plus
          Position
          PitchOffset ) )
      ( float WirePitch ) ) ) )

(defun PowerGridTranslatePitchToEdges ( Pitch WirePitch PitchOffset )
  (let (
        ( FirstEdge ( difference ( times Pitch WirePitch ) PitchOffset ) )
        ( SecondEdge ( difference ( times ( plus Pitch 1 ) WirePitch ) PitchOffset ) ) )
    ( list FirstEdge SecondEdge ) ) )


(defun PowerGridGetPitchesUsed ( LayerRects
                                 PitchOffset
                                 WirePitch
                                 WireSpacing
                                 PowerGridWidth
                                 PowerGridHeight
                                 LeftEdgeOfPowerGrid
                                 BottomEdgeOfPowerGrid )
  (let (
        ( HalfWireSpacing ( difference ( quotient WireSpacing 2.0 ) 0.000001 ) )
        ( LeftmostPitch ( PowerGridTranslatePositionToPitch LeftEdgeOfPowerGrid WirePitch PitchOffset ) )
        ( BottommostPitch ( PowerGridTranslatePositionToPitch BottomEdgeOfPowerGrid WirePitch PitchOffset ) )
        ( IsHorizontal ( PowerGridIsHorizontalWire ( car LayerRects ) ) ) )
    (let (
          ( TotalNumPitches ( quotient 
                              (if IsHorizontal
                                  PowerGridHeight
                                PowerGridWidth ) 
                              WirePitch ) )
          ( PitchTable ( makeTable "foo" nil ) ) )
      ( foreach
        LayerRect
        LayerRects
        (let (
              ( RectStartPitch ( PowerGridTranslatePositionToPitch
                                 ( difference
                                   (if IsHorizontal
                                       ( RectGetBottom
                                         LayerRect )
                                       ( RectGetLeft
                                         LayerRect ) )
                                   HalfWireSpacing )
                                 WirePitch
                                 PitchOffset ) )
              ( RectEndPitch ( PowerGridTranslatePositionToPitch
                               ( plus
                                 (if IsHorizontal
                                     ( RectGetTop
                                       LayerRect )
                                   ( RectGetRight
                                     LayerRect ) )
                                 HalfWireSpacing )
                               WirePitch
                               PitchOffset ) ) )
          ( printf 
            "%L %L %L\n"
            LayerRect
            RectStartPitch
            RectEndPitch )
          ( for
            Pitch
            RectStartPitch
            RectEndPitch
            ( setarray PitchTable Pitch t ) ) ) )
      ( car
        ( CounterCountAndAccumulateResult
          (if IsHorizontal
              BottommostPitch
            LeftmostPitch )
          ( plus 
            ( difference TotalNumPitches 1 )
            (if IsHorizontal
                BottommostPitch
              LeftmostPitch ) )
          1
          (lambda
            ( Pitch CurrResult PitchTable )
            ( tconc
              CurrResult
              ( arrayref PitchTable Pitch ) ) )
          ( list PitchTable )
          nil ) ) ) ) )
  
(defun PowerGridGetHorizontalPitchRects ( BottomEdge
                                          WirePitch
                                          Height
                                          PitchOffset )
  (let (
        ( NumPitches ( floor
                       ( quotient
                         Height
                         WirePitch ) ) )
        ( FirstPitch ( PowerGridTranslatePositionToPitch
                       BottomEdge
                       WirePitch
                       PitchOffset ) ) )
    ( CounterCountAndAccumulateResult
      FirstPitch
      ( plus
        FirstPitch
        ( difference NumPitches 1 ) )
      1
      (lambda
        ( Pitch CurrResult WirePitch PitchOffset )
        (let (
              ( PitchEdges ( PowerGridTranslatePitchToEdges Pitch WirePitch PitchOffset ) ) )
          (let (
                ( FirstEdge ( car  PitchEdges ) )
                ( SecondEdge ( cadr PitchEdges ) ) )
            ( cons
              ( list
                ( list 0 FirstEdge )
                ( list WirePitch SecondEdge ) )
              CurrResult ) ) ) )
      ( list WirePitch PitchOffset ) 
      nil ) ) )

(defun PowerGridGetVerticalPitchRects ( LeftEdge
                                        WirePitch
                                        Width
                                        PitchOffset )
  (let (
        ( NumPitches ( floor
                       ( quotient
                         Width
                         WirePitch ) ) )
        ( FirstPitch ( PowerGridTranslatePositionToPitch
                       LeftEdge
                       WirePitch
                       PitchOffset ) ) )
    ( CounterCountAndAccumulateResult
      FirstPitch
      ( plus
        FirstPitch
        ( difference NumPitches 1 ) )
      1
      (lambda
        ( Pitch CurrResult WirePitch PitchOffset )
        (let (
              ( FirstEdge ( difference ( times Pitch WirePitch ) PitchOffset ) )
              ( SecondEdge ( difference ( times ( plus Pitch 1 ) WirePitch ) PitchOffset ) ) )
          ( cons
            ( list
              ( list FirstEdge 0 )
              ( list SecondEdge WirePitch ) )
            CurrResult ) ) )
      ( list WirePitch PitchOffset ) 
      nil ) ) )
        
(defun PowerGridAnalyzePowerGridLayer ( CellView 
                                        LayerPP 
                                        HierarchyDepth
                                        WirePitch
                                        WireSpacing
                                        PowerGridWidth
                                        PowerGridHeight
                                        LeftEdgeOfPowerGrid
                                        BottomEdgeOfPowerGrid )
  (let (
        ( LayerRects ( sort
                       ( PowerGridGetFlatListOfRectsOnLayer
                         CellView 
                         LayerPP
                         HierarchyDepth )
                       `RectCompareRects ) ) )
    (when LayerRects
      (let (
            ( IsHorizontal ( PowerGridIsHorizontalWire ( car LayerRects ) ) ) )
        (let (
              ( PitchOffset (if IsHorizontal
                                ( PowerGridGetPitchOffset
                                  ( RectGetBottom ( cadr LayerRects ) )
                                  WireSpacing
                                  WirePitch )
                              ( PowerGridGetPitchOffset
                                ( RectGetLeft ( cadr LayerRects ) )
                                WireSpacing
                                WirePitch ) ) ) )
          ( list
            LayerPP
            IsHorizontal
            PitchOffset
            ( PowerGridGetPitchesUsed
              LayerRects
              PitchOffset
              WirePitch
              WireSpacing
              PowerGridWidth
              PowerGridHeight
              LeftEdgeOfPowerGrid
              BottomEdgeOfPowerGrid ) 
            (if IsHorizontal
                ( PowerGridGetHorizontalPitchRects
                  BottomEdgeOfPowerGrid
                  WirePitch
                  PowerGridHeight
                  PitchOffset )
              ( PowerGridGetVerticalPitchRects
                LeftEdgeOfPowerGrid
                WirePitch
                PowerGridWidth
                PitchOffset ) ) ) ) ) ) ) )
              
(defun PowerGridGetPowerGridInfo ( PowerGridLibName
                                   PowerGridCellName
                                   PowerGridViewName
                                   MetalLayerPurposePairs
                                   DirectivesTable
                                   UserUnitsPerMeter
                                   DirectivesUnitsPerMeter
                                   HierarchyDepth )
  (let (
        ( PowerGridCellViewDDObj ( ddGetObj PowerGridLibName PowerGridCellName PowerGridViewName ) ) )
    (when ( and PowerGridCellViewDDObj ( getq PowerGridCellViewDDObj files ) )
      (let (
            ( PowerGridCellView ( dbOpenCellViewByType
                                  PowerGridLibName
                                  PowerGridCellName
                                  PowerGridViewName
                                  nil
                                  "r" ) ) )
        (when PowerGridCellView
          ( dbComputeBBox PowerGridCellView )
          (let (
                ( PowerGridBBox ( getq PowerGridCellView bBox ) ) )
            (let (
                  ( PowerGridWidth ( RectGetWidth PowerGridBBox ) )
                  ( PowerGridHeight ( RectGetHeight PowerGridBBox ) ) )
              ( list
                ( list
                  PowerGridWidth
                  PowerGridHeight )
                ( ListApplyFuncToListAndAccumulateNonNilResults
                  MetalLayerPurposePairs
                  (lambda
                    ( MetalLayerPurposePair
                      PowerGridCellView
                      DirectivesTable
                      UserUnitsPerMeter
                      DirectivesUnitsPerMeter
                      PowerGridWidth
                      PowerGridHeight
                      LeftEdgeOfPowerGrid
                      BottomEdgeOfPowerGrid
                      HierarchyDepth )
                    (let (
                          ( WirePitch ( times
                                        UserUnitsPerMeter
                                        ( CellInfoGetLayerWirePitchInMeters
                                          DirectivesTable
                                          MetalLayerPurposePair
                                          DirectivesUnitsPerMeter ) ) )
                          ( WireSpacing ( times
                                          UserUnitsPerMeter
                                          ( CellInfoGetLayerWireSpacingInMeters
                                            DirectivesTable
                                            MetalLayerPurposePair
                                            DirectivesUnitsPerMeter ) ) ) )
                          (when ( and WirePitch WireSpacing )
                        ( PowerGridAnalyzePowerGridLayer
                          PowerGridCellView
                          MetalLayerPurposePair
                          HierarchyDepth
                          WirePitch
                          WireSpacing
                          PowerGridWidth
                          PowerGridHeight
                          LeftEdgeOfPowerGrid
                          BottomEdgeOfPowerGrid ) ) ) )
                  ( list
                    PowerGridCellView
                    DirectivesTable
                    UserUnitsPerMeter
                    DirectivesUnitsPerMeter
                    PowerGridWidth
                    PowerGridHeight
                    ( RectGetLeft PowerGridBBox )
                    ( RectGetBottom PowerGridBBox )
                    HierarchyDepth ) ) ) ) ) ) ) ) ) )
                    


(defun PowerGridGetPowerGridPatternForMetalLPP ( PowerGridInfo 
                                                 MetalLPP )
  (let (
        ( MetalInfo ( car ( exists SomeMetalInfo ( cadr PowerGridInfo )
                                   ( equal ( nth 0 SomeMetalInfo ) MetalLPP ) ) ) ) )
    ( nth 3 MetalInfo ) ) )



(defun PowerGridCopyAndAlignInstances ( SrcCellView
                                        TargetCellView
                                        PowerGridLibName
                                        PowerGridCellName
                                        PowerGridAlignmentInUserUnits )
  (let (
        ( FilterRet ( NameFilterInstances
                      ( getq SrcCellView instances )
                      ( list ( list PowerGridLibName PowerGridCellName ) ) ) ) )
    (let (
          ( CurrCount ( stringToTime ( getCurrentTime ) ) )
          ( PowerGridInstances ( cadr 
                                 FilterRet ) ) )
      ( foreach
        Instance
        PowerGridInstances
        (let (
              ( CurrInstanceName ( sprintf nil "power_grid_%d" CurrCount ) ) )
          ( setq CurrCount ( plus CurrCount 1 ) )
          ( dbSetq Instance CurrInstanceName name )
          ( InstanceAlignInstance Instance PowerGridAlignmentInUserUnits )
          (let (
                ( ObjToCopy (if ( equal ( getq Instance objType ) "mosaicInst" )
                                ( getq Instance mosaic )
                              Instance ) ) )
            ( dbSetq ObjToCopy CurrInstanceName name )
            ( dbCopyFig ObjToCopy TargetCellView ) ) ) ) ) ) )
        

(defun PowerGridDrawPowerGridOnCellView ( CellView
                                          TempDir
                                          PDKPackageRoot
                                          PowerGridLibName
                                          PowerGridCellName
                                          PowerGridViewName
                                          PowerGridAlignmentInUserUnits
                                          PowerGridExclusionLPP )
  (let (
        ( RuleFile ( sprintf nil "%s/share/Fulcrum/powergrid/powergrid.assura.rules" PDKPackageRoot ) )
        ( TempLibName ( LibCreateTempLibraryFromCellView CellView "POWERGRID" TempDir ) )
        ( ErrorStr nil ) )
    ( setq
      ErrorStr
      ( AssuraRunAssuraLayerProcessor
        CellView
        TempLibName
        ( getq CellView cellName )
        ( getq CellView viewName )
        RuleFile
        TempDir
        ( list ( list "powergrid_fill" PowerGridExclusionLPP ) )
        ( list
          ( list
            ( list
              PowerGridLibName
              PowerGridCellName
              PowerGridViewName )
            "grid" ) ) ) )
    (unless ErrorStr
      (let (
            ( OutputCellDDObj ( ddGetObj TempLibName ( getq CellView cellName ) ( getq CellView viewName ) ) ) )
        (if ( and OutputCellDDObj ( getq OutputCellDDObj files ) )
            (let (
                  ( CellWithGrid ( dbOpenCellViewByType
                                   TempLibName
                                   ( getq CellView cellName )
                                   ( getq CellView viewName )
                                   nil
                                   "a" ) ) )
              (if CellWithGrid
                  (let ()
                    ( PowerGridCopyAndAlignInstances 
                      CellWithGrid
                      CellView
                      PowerGridLibName
                      PowerGridCellName
                      PowerGridAlignmentInUserUnits )
                    ( dbSave CellWithGrid )
                    ( dbPurge CellWithGrid ) )
                ( setq 
                  ErrorStr
                  ( sprintf 
                    nil
                    "Unable to open cell view %L %L %L, even though DDObj exists."
                    TempLibName
                    ( getq CellView cellName )
                    ( getq CellView viewName ) ) ) ) )
          ( setq 
            ErrorStr
            ( sprintf
              nil
              "%L %L %L does not exist."
              TempLibName
              ( getq CellView cellName )
              ( getq CellView viewName ) ) ) ) ) )
    ( ddDeleteObj ( ddGetObj TempLibName ) )
    ErrorStr ) )


(defun PowerGridCreatePowerGridPatternFromDirectives ( DirectiveTable LPP )
  (cond (
         ( and 
           ( CellInfoLookupParametrized DirectiveTable "layer_wirepitch" "layer" LPP )
           ( CellInfoLookupParametrized DirectiveTable "layer_wirespacing" "layer" LPP )
           ( CellInfoLookupParametrized DirectiveTable "layer_powergrid_wirewidth" "layer" LPP )
           ( CellInfoLookupParametrized DirectiveTable "layer_powergrid_offset" "layer" LPP )
           ( CellInfoLookupParametrized DirectiveTable "layer_powergrid_spacing" "layer" LPP )
           )
         (let (
               ( LayerWirePitch
                 ( CellInfoLookupParametrized 
                   DirectiveTable "layer_wirepitch" "layer" LPP ) )
               ( LayerWireSpacing
                 ( CellInfoLookupParametrized
                   DirectiveTable "layer_wirespacing" "layer" LPP ) )
               ( LayerPowerGridWireWidth
                 ( CellInfoLookupParametrized
                   DirectiveTable "layer_powergrid_wirewidth" "layer" LPP ) )
               ( LayerPowerGridOffset
                 ( CellInfoLookupParametrized
                   DirectiveTable "layer_powergrid_offset" "layer" LPP ) )
               ( LayerPowerGridSpacing
                 ( quotient
                   ( CellInfoLookupParametrized 
                     DirectiveTable "layer_powergrid_spacing" "layer" LPP )
                   2  ) )
               )
           (let (
                 ( PowerGridPitches 
                   ( ceiling 
                     ( quotient 
                       ( plus LayerPowerGridWireWidth LayerWireSpacing )
                       LayerWirePitch ) ) ) )
             (let (
                   ( PitchVector ( makeVector LayerPowerGridSpacing nil ) ) )
               ( for I LayerPowerGridOffset 
                     ( plus LayerPowerGridOffset 
                            ( difference PowerGridPitches 1 ) )
                     ( setarray PitchVector I t ) )
               ( vectorToList PitchVector ) ) ) ) )
        (
         ( list nil ) ) ) )
          
        
  
; Reports types that are not layout views or any power grid instances
(defun ReportPowerGridsInSubcells (@key (CV (geGetEditCellView)))
  (let (done)
    done = (makeTable "done" nil)
    (rexCompile "POWER_GRID")
    (ReportPowerGridsInSubcellsRecurse CV t)
    )
  t
  )

; Reports types that are not layout views or any power grid instances
(defun ReportPowerGridsInSubcellsRecurse (CV top)
  (cond ((and !top CV->viewName!="layout")
         (printf "Cell %s found with view %s\n" CV->cellName CV->viewName)
         )
        ((and !top (setof inst CV->instances (rexExecute inst->cellName)))
         (printf "Cell %s layout view has POWER_GRID instance\n" CV->cellName)
         )
        (t
         (foreach inst CV->instances
           (when (and inst->libName!=TechLibName
                      inst->libName!="stack"
                      inst->libName!="gate"
                      !done[inst->master])
             (ReportPowerGridsInSubcellsRecurse inst->master nil)
             )
           )
         )
        )
  done[CV]=t
  t
  )
