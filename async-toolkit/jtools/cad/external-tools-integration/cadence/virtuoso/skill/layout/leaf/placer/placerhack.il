; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$
(defun PlacerHackFindInstancesInRect ( Instances Rect )
  (setof
   Instance
   Instances
   ( RectDoRectsOverlap
     ( getq Instance bBox ) 
     Rect ) ) )

(defun PlacerHackInstancesSortFunc ( Instance0 Instance1 )
  (let (
        ( Rect0 ( getq Instance0 bBox ) )
        ( Rect1 ( getq Instance1 bBox ) ) )
    (let (
          ( Top0 ( RectGetBottom Rect0 ) )
          ( Top1 ( RectGetBottom Rect1 ) ) )
      ( lessp Top0 Top1 ) ) ) )


(defun PlacerHackCreateTransitorToGroupTable ( TransistorGroups )
  (let (
        ( Ret ( makeTable "foo" ) ) )
    ( foreach
      Group
      TransistorGroups
      ( foreach
        Transistor
        Group
        ( setarray
          Ret
          ( getq Transistor name )
          Group ) ) ) 
    Ret ) )


(defun PlacerHackGetBottomOfTransistorGateFromInstance ( Instance GateLengthParamName UserUnitsPerMeter )
  (let (
        ( RotationStr ( getq Instance orient ) )
        ( GateLengthProp ( dbSearchPropByName Instance GateLengthParamName ) ) )
    (let (
          ( GateLengthPropValue ( getq GateLengthProp value ) ) )
      (let (
            ( GateLength
              (if ( stringp GateLengthPropValue )
                  ( StringUtilParseLengthString GateLengthPropValue )
                GateLengthPropValue ) ) )
        (when GateLength
          (cond
           (
            ( equal RotationStr "R90" )
            ( cadr ( getq Instance xy ) ) )
           (
            ( equal RotationStr "R270" )
            ( difference 
              ( cadr ( getq Instance xy ) )
              ( times GateLength UserUnitsPerMeter ) ) )
           ( t
             nil ) ) ) ) ) ) )


(defun PlacerHackIsGateInstance ( Instance GateLibName )
  ( equal GateLibName ( getq Instance libName ) ) )


(defun PlacerHackGetAdjacentTransistorPairInfos ( SortedInstancesList
                                                  TransistorLibName
                                                  TransistorCellName
                                                  TransistorViewName 
                                                  TransistorToGroupTable )
  (let (
        ( LastTransistorInstance nil )
        ( BelowInstances nil )
        ( DuringInstances nil )
        ( CurrInstancesList SortedInstancesList ) 
        ( Infos nil ) )
    (while CurrInstancesList
      (let (
            ( CurrInstance ( car CurrInstancesList ) ) )
        ( setq CurrInstancesList ( cdr CurrInstancesList ) )
        (if ( and
              ( equal
                TransistorLibName
                ( getq CurrInstance libName ) )
              ( equal
                TransistorCellName
                ( getq CurrInstance cellName ) )
              ( equal
                TransistorViewName
                ( getq CurrInstance viewName ) ) )
            (let (
                  ( CurrTransistorInstance CurrInstance ) )
              (when LastTransistorInstance
                ( setq 
                  Infos
                  ( cons
                    ( list
                      LastTransistorInstance
                      CurrTransistorInstance
                      BelowInstances
                      ( cons CurrTransistorInstance CurrInstancesList )
                      ( ListRemoveDuplicates
                        ( ListApplyFuncToListAndAccumulateResult
                          DuringInstances
                          (lambda
                            ( DuringInstance CurrResult TransistorToGroupTable ) 
                            (let (
                                  ( Group ( arrayref 
                                            TransistorToGroupTable
                                            ( getq DuringInstance name ) ) ) )
                              (if Group
                                  (let (
                                        ( Ret CurrResult ) )
                                    ( foreach
                                      GroupMember
                                      Group
                                      ( setq Ret ( cons GroupMember Ret ) ) )
                                    Ret )
                                ( cons
                                  DuringInstance
                                  CurrResult ) ) ) )
                          ( list TransistorToGroupTable )
                          nil )
                        (lambda
                          ( Inst0 Inst1 )
                          ( equal ( getq Inst0 name ) ( getq Inst1 name ) ) )
                        nil ) )
                    Infos ) ) )
                ( setq DuringInstances nil )
                ( setq LastTransistorInstance CurrTransistorInstance ) )
            ( setq DuringInstances ( cons CurrInstance DuringInstances ) ) )
        ( setq BelowInstances ( cons CurrInstance BelowInstances ) ) ) ) 
    Infos ) )

(defun PlacerHackGetDiffusionRectsFromInstance ( TransistorInstance
                                                 DiffusionLPP )
  (let (
        ( DiffusionLayerName ( car DiffusionLPP ) )
        ( DiffusionPurposeName ( cadr DiffusionLPP ) ) 
        ( Master ( getq TransistorInstance master ) ) )
    ( dbComputeBBox Master )
    (let (
          ( DiffusionShapesInMaster ( dbProduceOverlap
                                      Master
                                      ( getq Master bBox )
                                      ( list 0 0 )
                                      DiffusionLayerName ) ) )
      (let (
            ( DiffusionRectsInMaster ( setof
                                       Shape
                                       DiffusionShapesInMaster
                                       ( equal
                                         "rect"
                                         ( getq Shape objType ) ) ) ) )
                                        
        ( ListApplyFuncToListAndAccumulateResults
          DiffusionRectsInMaster
          (lambda
            ( Rect Transform )
            ( dbTransformBBox ( getq Rect bBox ) Transform ) )
          ( list
            ( getq TransistorInstance transform ) ) ) ) ) ) )

(defun PlacerHackGetTopOfDiffusionFromInstance ( TransistorInstance
                                                 DiffusionLPP )
  (let (
        ( DiffusionRects ( PlacerHackGetDiffusionRectsFromInstance
                           TransistorInstance
                           DiffusionLPP ) )
        ( CurrTop nil ) )
    ( foreach
      Rect
      DiffusionRects
      (when ( or
              ( null CurrTop )
              ( greaterp
                ( RectGetTop Rect )
                CurrTop ) )
        ( setq CurrTop ( RectGetTop Rect ) ) ) )
    CurrTop ) )

(defun PlacerHackGetBottomOfDiffusionFromInstance ( TransistorInstance
                                                    DiffusionLPP )
  (let (
        ( DiffusionRects ( PlacerHackGetDiffusionRectsFromInstance
                           TransistorInstance
                           DiffusionLPP ) )
        ( CurrBottom nil ) )
    ( foreach
      Rect
      DiffusionRects
      (when ( or
              ( null CurrBottom )
              ( lessp
                ( RectGetBottom Rect )
                CurrBottom ) )
        ( setq CurrBottom ( RectGetBottom Rect ) ) ) ) 
    CurrBottom ) )


(defun PlacerHackFindInstancePairsWithIllegalDiffusionSpacings ( InstancePairInfos 
                                                                 DiffusionLPP
                                                                 MinDiffusionSpacing )
  ( ListApplyFuncToListAndAccumulateNonNilResults
    InstancePairInfos
    (lambda
      ( InstancePairInfo DiffusionLPP MinDiffusionSpacing )
      (let (
            ( TopInstance ( cadr InstancePairInfo ) )
            ( BottomInstance ( car InstancePairInfo ) ) 
            ( BelowInstances ( caddr InstancePairInfo ) ) 
            ( AboveInstances ( cadddr InstancePairInfo ) ) 
            ( DuringInstances ( nth 4 InstancePairInfo ) ) )
        (let (
              ( BottomOfTopInstanceDiffusion
                ( PlacerHackGetBottomOfDiffusionFromInstance
                  TopInstance
                  DiffusionLPP ) )
              ( TopOfBottomInstanceDiffusion
                ( PlacerHackGetTopOfDiffusionFromInstance
                  BottomInstance
                  DiffusionLPP ) ) )
          (let (
                ( CurrDiffusionSpacing 
                  ( difference 
                    BottomOfTopInstanceDiffusion
                    TopOfBottomInstanceDiffusion ) ) )
            (unless ( or
                      ( lessp CurrDiffusionSpacing 0.0001 )
                      ( equal CurrDiffusionSpacing MinDiffusionSpacing )
                      ( greaterp CurrDiffusionSpacing MinDiffusionSpacing ) )
              ( list
                BottomInstance
                TopInstance
                BelowInstances
                AboveInstances
                DuringInstances
                CurrDiffusionSpacing ) ) ) ) ) )
    ( list
      DiffusionLPP
      MinDiffusionSpacing ) ) )

(defun PlacerHackFixIllegalSpacings ( IllegalSpacingInfos
                                      TopInstance
                                      BottomInstance 
                                      RegionTop
                                      RegionBottom
                                      CorrectSpacing )
  ( foreach
    IllegalSpacingInfo
    IllegalSpacingInfos
    (let (
          ( RequiredDisplacement ( difference 
                                   ( float CorrectSpacing )
                                   ( float 
                                     ( nth 5 IllegalSpacingInfo ) ) ) )
          ( TopDistance ( abs 
                          ( difference
                            RegionTop
                            ( RectGetTop
                              ( getq TopInstance bBox ) ) ) ) )
          ( BottomDistance ( abs
                             ( difference
                               RegionBottom
                               ( RectGetBottom
                                 ( getq BottomInstance bBox ) ) ) ) )
          ( BelowInstances ( caddr IllegalSpacingInfo ) )
          ( AboveInstances ( cadddr IllegalSpacingInfo ) ) 
          ( DuringInstances ( nth 4 IllegalSpacingInfo ) ) )
      ( printf
        "Top: %L %L Bottom:%L %L %L\n"
        ( getq ( nth 1 IllegalSpacingInfo ) name )
        ( getq ( nth 1 IllegalSpacingInfo ) xy )
        ( getq ( nth 0 IllegalSpacingInfo ) name )
        ( getq ( nth 0 IllegalSpacingInfo ) xy )
        ( nth 5 IllegalSpacingInfo ) )
      (let (
            ( DirectedDisplacement (if ( greaterp BottomDistance TopDistance )
                                       ( difference 0.0 RequiredDisplacement )
                                     RequiredDisplacement ) )
            ( InstancesToMove ( ListRemoveDuplicates
                                ( append
                                  (if DuringInstances 
                                      ( reverse DuringInstances )
                                    ( list ) )
                                  (if ( greaterp BottomDistance TopDistance )
                                      ( reverse BelowInstances )
                                    AboveInstances ) )
                                (lambda
                                  ( Inst0 Inst1 )
                                  ( equal ( getq Inst0 name ) ( getq Inst1 name ) ) )
                                nil ) ) )
        ( foreach
          Instance
          InstancesToMove
          (let ()
            ( dbMoveFig
              Instance
              nil
              ( list
                ( list 0 DirectedDisplacement )
                "R0" ) )
            ( printf
              "Moved %L by %L\n"
              ( getq Instance name ) 
              DirectedDisplacement ) ) ) ) ) ) )
          
              

(defun PlacerHackFixCell ( CellView 
                           CellRegionData
                           LibsToIgnore
                           CellsToIgnore
                           TransistorLibName
                           NTransistorCellName
                           PTransistorCellName
                           TransistorViewName
                           DiffusionLPP
                           MinDiffusionSpacing )
  (let (
        ( CurrCol 0 )
        ( FilteredInstances ( UpdateNetlistFilterInstances
                              CellView
                              LibsToIgnore
                              CellsToIgnore ) ) )
    ( foreach
      Column
      ( PlacementRegionsGetCellRegionDataColumnList
        CellRegionData )
      (let (
            ( ColumnRect ( PlacementRegionGetColumnBBox Column ) ) )
        ( setq CurrCol ( plus CurrCol 1 ) )
        (let (
              ( ColumnInstances ( PlacerHackFindInstancesInRect
                                  FilteredInstances
                                  ColumnRect ) ) )
          (when ColumnInstances
            (let (
                  ( SortedColumnInstances ( sort
                                            ColumnInstances
                                            `PlacerHackInstancesSortFunc ) ) )
                  
              (let (
                    ( InstanceGroupsTable 
                      ( PlacerHackCreateTransitorToGroupTable 
                        ( GuessPlacementRegionsGroupTransistorInstances 
                          SortedColumnInstances ) ) )
                    ( TopInstance ( nth 
                                    ( difference
                                      ( length SortedColumnInstances )
                                      1 )
                                    SortedColumnInstances ) )
                    ( BottomInstance ( car SortedColumnInstances ) ) 
                    ( ColumnTop ( RectGetTop ColumnRect ) )
                    ( ColumnBottom ( RectGetBottom ColumnRect ) ) )
                ( PlacerHackFixIllegalSpacings
                  ( PlacerHackFindInstancePairsWithIllegalDiffusionSpacings
                    ( PlacerHackGetAdjacentTransistorPairInfos
                      SortedColumnInstances
                      TransistorLibName
                      NTransistorCellName
                      TransistorViewName
                      InstanceGroupsTable )
                    DiffusionLPP
                    MinDiffusionSpacing )
                  TopInstance
                  BottomInstance
                  ColumnTop
                  ColumnBottom
                  MinDiffusionSpacing )

                ( PlacerHackFixIllegalSpacings
                  ( PlacerHackFindInstancePairsWithIllegalDiffusionSpacings
                    ( PlacerHackGetAdjacentTransistorPairInfos
                      SortedColumnInstances
                      TransistorLibName
                      PTransistorCellName
                      TransistorViewName
                      InstanceGroupsTable )
                    DiffusionLPP
                    MinDiffusionSpacing )
                  TopInstance
                  BottomInstance
                  ColumnTop
                  ColumnBottom
                  MinDiffusionSpacing ) ) ) ) ) ) ) ) 
  t )


(defun PlacerHackFixCellWithTechnologyInfo ( CellView
                                             CellRegionData
                                             LibsToIgnore
                                             CellsToIgnore
                                             TransistorLibName
                                             NTransistorCellName
                                             PTransistorCellName
                                             TransistorViewName
                                             DiffusionLPP )
  (let (
        ( MinDiffusionSpacing
          ( SpacingGetSameLayerSpacingForLayer
            CellView
            DiffusionLPP ) ) )
    (if MinDiffusionSpacing
        ( PlacerHackFixCell
          CellView
          CellRegionData
          LibsToIgnore
          CellsToIgnore
          TransistorLibName
          NTransistorCellName
          PTransistorCellName
          TransistorViewName
          DiffusionLPP
          MinDiffusionSpacing )
      ( printf "\nERROR: Unable to get minimum diffusion spacing.\n" ) ) ) )

;( PlacerHackFindInstancePairsWithIllegalDiffusionSpacings ( sort ( PlacerHackFindInstancesInRect ( UpdateNetlistFilterInstances ( geGetWindowCellView ) ( list ) ( list "M3_M2_min" ) ) ( PlacementRegionGetColumnNRegionRect ( car ( PlacementRegionsGetCellRegionDataColumnList CellRegionData ) ) ) ) `PlacerHackInstancesSortFunc ) "gate" ( list "DIFF" "drawing" ) 0.21 )

;( sort ( PlacerHackFindInstancesInRect ( UpdateNetlistFilterInstances ( getq ( geGetWindowCellView ) instances ) ( list ) ( list "M3_M2_min" ) ) ( PlacementRegionGetColumnNRegionRect ( car ( PlacementRegionsGetCellRegionDataColumnList CellRegionData ) ) ) ) `PlacerHackInstancesSortFunc )



;( PlacerHackFindInstancePairsWithIllegalDiffusionSpacings (let ( ( Instances ( sort ( PlacerHackFindInstancesInRect ( UpdateNetlistFilterInstances ( geGetWindowCellView ) ( list ) ( list "M3_M2_min" ) ) ( list ( list 0.105 0.910 ) ( list 14.105 37.490 ) ) ) `PlacerHackInstancesSortFunc ) ) ) ( PlacerHackGetAdjacentTransistorPairInfos Instances "tsmc13lg" "nmos1v" "layout" ( PlacerHackCreateTransitorToGroupTable ( GuessPlacementRegionsGroupTransistorInstances Instances ) ) ) ) ( list "DIFF" "drawing" ) 0.21 )
