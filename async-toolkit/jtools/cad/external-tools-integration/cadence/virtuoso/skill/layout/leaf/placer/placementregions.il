 ; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 ; $Id$
 ; $DateTime$
 ; $Author$

; Function: PlacementRegions
; Parameter: CellView (CVId)
; Return: CellView (CVId)
;
; Defines how large an region to contain all the components.
;
;
;

defun( PlacementRegions ( CellView )

  (defvar CellRegionData
    ( PlacementRegionsCalculateCellRegionData 
      YPitch
      ( max NWidth ( NPSearchGetMinFitWidthFromWidthData NWidthData ) )
      ( max PWidth ( NPSearchGetMinFitWidthFromWidthData PWidthData ) )
      ColumnCount
    ( times LeafCellMinHorizontalSpacingToBoundary UserUnitsPerMeter )
    ( times LeafCellMinVerticalSpacingToBoundary UserUnitsPerMeter )
    0.0
    ( times ( car BoundaryPosition )  UserUnitsPerMeter )
    ( times ( cadr BoundaryPosition ) UserUnitsPerMeter )
 ) )
  ( printf "CellRegionData= " )
  ( println CellRegionData )

  ( dbCreateRect
    CellView
    BoundaryLPP
    ( PlacementRegionsGetCellRegionDataBBox
      CellRegionData ) )

  prBound=( PlacementRegionsGetCellRegionDataBBox CellRegionData )
  dbCreatePRBoundary( CellView 
    list( leftEdge(prBound):bottomEdge(prBound)
          rightEdge(prBound):bottomEdge(prBound)
          rightEdge(prBound):topEdge(prBound)
          leftEdge(prBound):topEdge(prBound)
        )
  )

  ( dbSave CellView )
  CellView
 )

(defun PlacementRegionsHasTransistors ( CellView TransistorLibraryName NTransistorName PTransistorName )
  ( not 
    ( null
      ( exists
        Instance
        ( getq CellView instances )
        ( and
          ( equal
            ( getq Instance libName )
            TransistorLibraryName )
          ( or
            ( equal
              ( getq Instance cellName )
              NTransistorName )
            ( equal
              ( getq Instance cellName )
              PTransistorName ) ) ) ) ) ) )


(defun PlacementRegionsCalculateColumnData ( ColLeftEdge 
                                             YPitch
                                             NWidth
                                             PWidth
                                             NPRegionParity
                                             TopOfTopGateRegion
                                             BottomOfTopGateRegion
                                             TopOfBottomGateRegion
                                             BottomOfBottomGateRegion )
  (let (
        ( TransistorRegionJunction ( plus
                                     ColLeftEdge
                                     (if NPRegionParity PWidth NWidth ) ) )
        ( ColRightEdge ( plus
                         ColLeftEdge
                         NWidth
                         PWidth ) ) )
    ( PlacementRegionMakeColumnData
      (if NPRegionParity 
          ( list
            ( list TransistorRegionJunction BottomOfBottomGateRegion )
            ( list ColRightEdge TopOfTopGateRegion ) )
          ( list
            ( list ColLeftEdge BottomOfBottomGateRegion )
            ( list TransistorRegionJunction TopOfTopGateRegion ) ) )
      (if NPRegionParity 
          ( list
            ( list ColLeftEdge BottomOfBottomGateRegion )
            ( list TransistorRegionJunction TopOfTopGateRegion ) )
        ( list
          ( list TransistorRegionJunction BottomOfBottomGateRegion )
          ( list ColRightEdge TopOfTopGateRegion ) ) )
      NPRegionParity         
      ( list
        ( list ColLeftEdge BottomOfBottomGateRegion )
        ( list ColRightEdge BottomOfBottomGateRegion ) )
      ( list
        ( list ColLeftEdge BottomOfBottomGateRegion )
        ( list ColRightEdge TopOfTopGateRegion ) ) ) ) )


(defun PlacementRegionsGetRealRegions ( CellRegionData )
  (let (
        ( Regions nil )
        ( LastRightRegion nil )
        ( LastParity 1 )
        ( IsFirst t )
        ( Columns ( PlacementRegionsGetCellRegionDataColumnList CellRegionData )  )
        )
    (while Columns
      (let (
            ( Column ( car Columns ) )
            ( IsLast ( not ( cdr Columns ) ) ) )
        (let (
              ( Parity ( PlacementRegionGetNPRegionParity Column ) )
              ( LeftRegion ( PlacementRegionGetColumnLeftRegionRect Column ) )
              ( RightRegion ( PlacementRegionGetColumnRightRegionRect Column ) ) )
              (when IsFirst
                ( setq Regions 
                       ( tconc Regions
                               ( PlacementRegionMakeRealRegion 
                                 LeftRegion 
                                 Parity
                                 ) ) ) )
              (when ( not IsFirst )
                (cond (
                       ( not ( equal Parity LastParity ) )
                       ( setq Regions 
                              ( tconc Regions 
                                      ( PlacementRegionMakeRealRegion 
                                        ( RectGetBoundRect LastRightRegion LeftRegion )
                                        Parity ) ) ) )
                      (
                       t
                       ( setq Regions 
                              ( tconc Regions 
                                      ( PlacementRegionMakeRealRegion 
                                        LeftRegion
                                        Parity ) ) )
                       ( setq Regions 
                              ( tconc Regions 
                                      ( PlacementRegionMakeRealRegion 
                                        LastRightRegion
                                        ( null Parity ) ) ) ) ) ) )
              (when IsLast 
                ( setq Regions 
                       ( tconc Regions
                               ( PlacementRegionMakeRealRegion 
                                 RightRegion 
                                 ( null Parity )
                                 ) ) ) )

              ( setq LastParity Parity )
              ( setq LastRightRegion RightRegion )
              ( setq Columns ( cdr Columns ) )
              ( setq IsFirst nil ) ) ) )
    
    ( car Regions )
) )

(defun PlacementRegionMakeRealRegion ( Rect IsP )
  ( list Rect IsP ) )

(defun PlacementRegionGetRealRegionBBox ( RealRegion )
  ( nth 0 RealRegion ) )

(defun PlacementRegionGetRealRegionIsP ( RealRegion )
  ( nth 1 RealRegion ) )

(defun PlacementRegionsMakeCellRegionData ( ColumnDataList BBox )
  ( list ColumnDataList BBox ) )

(defun PlacementRegionsGetCellRegionDataBBox ( CellRegionData )
  ( nth 1 CellRegionData ) )

(defun PlacementRegionsGetCellRegionDataColumnList ( CellRegionData )
  ( nth 0 CellRegionData ) )

(defun PlacementRegionMakeColumnData ( NRegionRect PRegionRect NPRegionParity BottomGateRegionRect TopGateRegionRect )
  ( list ( list NRegionRect PRegionRect NPRegionParity ) ( list BottomGateRegionRect TopGateRegionRect ) ) )

(defun PlacementRegionGetColumnNRegionRect ( ColumnData )
  ( nth 0 ( nth 0 ColumnData ) ) )

(defun PlacementRegionGetColumnPRegionRect ( ColumnData )
  ( nth 1 ( nth 0 ColumnData ) ) )

(defun PlacementRegionGetColumnLeftRegionRect ( ColumnData )
  (if ( PlacementRegionGetNPRegionParity ColumnData )
      ( PlacementRegionGetColumnPRegionRect ColumnData )
    ( PlacementRegionGetColumnNRegionRect ColumnData ) ) )

(defun PlacementRegionGetColumnRightRegionRect ( ColumnData )
  (if ( PlacementRegionGetNPRegionParity ColumnData )
      ( PlacementRegionGetColumnNRegionRect ColumnData )
    ( PlacementRegionGetColumnPRegionRect ColumnData ) ) )

(defun PlacementRegionGetNPRegionParity ( ColumnData )
  ( nth 2 ( nth 0 ColumnData ) ) )

(defun PlacementRegionGetColumnBottomGateRegionRect ( ColumnData )
  ( nth 0 ( nth 1 ColumnData ) ) )

(defun PlacementRegionGetColumnTopGateRegionRect ( ColumnData )
  ( nth 1 ( nth 1 ColumnData ) ) )

(defun PlacementRegionGetColumnNPJunction ( ColumnData )
  (cond (
         ( PlacementRegionGetColumnLeftRegionRect ColumnData )
         ( RectGetRight
           ( PlacementRegionGetColumnLeftRegionRect ColumnData ) ) )
        (
         ( RectGetLeft
          ( PlacementRegionGetColumnRightRegionRect ColumnData ) ) ) ) )


(defun PlacementRegionGetColumnBBox ( ColumnData )
  ( RectGetBoundRect
    ( RectGetBoundRect
      ( PlacementRegionGetColumnNRegionRect
        ColumnData )
      ( PlacementRegionGetColumnPRegionRect
        ColumnData ) )
    ( RectGetBoundRect
      ( PlacementRegionGetColumnTopGateRegionRect
        ColumnData )
      ( PlacementRegionGetColumnBottomGateRegionRect
        ColumnData ) ) ) )

(defun PlacementRegionGetColumnRightEdge ( ColumnData )
  ( RectGetRight ( PlacementRegionGetColumnBBox ColumnData ) ) )


(defun PlacementRegionsCalculateBottomOfBottomGateRegion ( YPitch
                                                           MinVerticalSpacingToBoundary 
                                                           BottomOfCellBoundary )
  ( plus 
    ( float MinVerticalSpacingToBoundary )
    ( float BottomOfCellBoundary ) ) )

(defun PlacementRegionsCalculateTopOfBottomGateRegion ( YPitch
                                                        MinVerticalSpacingToBoundary 
                                                        BottomOfCellBoundary )
  ( plus 
    ( float MinVerticalSpacingToBoundary )
    ( float BottomOfCellBoundary ) ) )

(defun PlacementRegionsCalculateBottomOfTopGateRegion ( YPitch
                                                        MinVerticalSpacingToBoundary
                                                        BottomOfCellBoundary )
  ( plus 
    ( float MinVerticalSpacingToBoundary )
    ( float BottomOfCellBoundary ) ) )
  

(defun PlacementRegionsCalculateTopOfTopGateRegion ( YPitch
                                                     MinVerticalSpacingToBoundary
                                                     BottomOfCellBoundary )
  ( plus
    ( difference
      ( float YPitch )
      ( float MinVerticalSpacingToBoundary ) )
    BottomOfCellBoundary ) )


(defun PlacementRegionsCalculateTotalGateAndTransistorRegionsHeight ( YPitch
                                                                      MinVerticalSpacingToBoundary )
  ( difference
    YPitch
    MinVerticalSpacingToBoundary ) )

(defun PlacementRegionsCalculateMaxSpacing ( LeafCellMetalSpacings
                                             WellSpacing )
  ( max
    ( PlacementRegionsCalculateMaxWireSpacing LeafCellMetalSpacings )
    ( float WellSpacing ) ) )

(defun PlacementRegionsCalculateMaxWireSpacing ( LeafCellMetalSpacings )
  ( ListApplyFuncToListAndAccumulateResult
    LeafCellMetalSpacings
    (lambda
      ( CurrSpacing MaxSpacing )
      ( max CurrSpacing MaxSpacing ) )
    nil
    0.0 ) )

(defun PlacementRegionsCalculateCellRegionData ( YPitch
                                                 NWidth
                                                 PWidth
                                                 NumberOfColumns
                                                 MinHorizontalSpacingToBoundary
                                                 MinVerticalSpacingToBoundary
                                                 ColumnSpacing
                                                 LeftOfCellBoundary
                                                 BottomOfCellBoundary )
                                        ;Check arguments and return nil if arguments don't make sense.
  (when ( and
                                        ;Maximum Spacing requirement had better be positive
          ( geqp
            MinHorizontalSpacingToBoundary
            0 )
                                        ;Maximum Spacing requirement had better be positive
          ( geqp
            MinVerticalSpacingToBoundary
           0 )
                                        ;Have to have at least one column
          ( greaterp
            NumberOfColumns
            0 ) )
    (let (
          ( BottomOfBottomGateRegion ( PlacementRegionsCalculateBottomOfBottomGateRegion
                                       YPitch
                                       MinVerticalSpacingToBoundary
                                       BottomOfCellBoundary ) )
          ( TopOfBottomGateRegion ( PlacementRegionsCalculateTopOfBottomGateRegion
                                    YPitch
                                    MinVerticalSpacingToBoundary
                                    BottomOfCellBoundary ) )
          ( BottomOfTopGateRegion ( PlacementRegionsCalculateBottomOfTopGateRegion
                                    YPitch
                                    MinVerticalSpacingToBoundary  
                                    BottomOfCellBoundary ) )
          ( TopOfTopGateRegion ( PlacementRegionsCalculateTopOfTopGateRegion
                                 YPitch
                                 MinVerticalSpacingToBoundary
                                 BottomOfCellBoundary ) )
          ( ColumnList nil )
          ( CurrX ( plus LeftOfCellBoundary MinHorizontalSpacingToBoundary ) ) )
      ( for 
        ColNum 
        1 
        NumberOfColumns
        (let (
              ( CurrCol ( PlacementRegionsCalculateColumnData
                          CurrX
                          YPitch
                          NWidth
                          PWidth
                          ( equal ( mod ColNum 2 ) 0 )
                          TopOfTopGateRegion
                          BottomOfTopGateRegion
                          TopOfBottomGateRegion
                          BottomOfBottomGateRegion ) ) )
          ( setq ColumnList ( tconc ColumnList CurrCol ) )
          ( setq CurrX ( plus ( PlacementRegionGetColumnRightEdge CurrCol ) ColumnSpacing ) ) ) )
      (let (
            ( RightMostCol ( car ( last ColumnList ) ) ) )
        (let (
              ( CellBound ( list
                            ( list LeftOfCellBoundary BottomOfCellBoundary )
                            ( list
                              ( plus
                                ( PlacementRegionGetColumnRightEdge
                                  RightMostCol )
                                MinHorizontalSpacingToBoundary )
                              ( plus 
                                YPitch
                                BottomOfCellBoundary ) ) ) ) )
                                        ;Result is a list whose first entry is
                                        ;a list of column datas and whose
                                        ;second entry is the bounding box
                                        ;of the cell
          ( PlacementRegionsMakeCellRegionData 
            ( car ColumnList )
            CellBound  ) ) ) ) ) )

(defun PlacementRegionsGetTotalGateRegionHeight ( CellRegionData )
  (let ( 
        ( Height 0 )
        ( ColumnDataList ( PlacementRegionsGetCellRegionDataColumnList CellRegionData ) ) )
    ( foreach ColumnData ColumnDataList             
              (let (
                    ( TopGateRegionRect ( PlacementRegionGetColumnTopGateRegionRect ColumnData ) )              
                    ( BottomGateRegionRect ( PlacementRegionGetColumnBottomGateRegionRect ColumnData ) ) )
                ( setq Height ( plus Height ( BBoxGetHeight TopGateRegionRect ) ( BBoxGetHeight BottomGateRegionRect ) ) ) ) )
    Height ) )

(defun PlacementRegionsGetTotalNRegionHeight ( CellRegionData )
  (let ( 
        ( Height 0 )
        ( ColumnDataList ( PlacementRegionsGetCellRegionDataColumnList CellRegionData ) ) )
    ( foreach ColumnData ColumnDataList             
              (let (
                    ( NRegionRect ( PlacementRegionGetColumnNRegionRect ColumnData ) ) )                  
                ( setq Height ( plus Height ( BBoxGetHeight NRegionRect ) ) ) ) )
    Height ) )

(defun PlacementRegionsGetTotalPRegionHeight ( CellRegionData )
  (let ( 
        ( Height 0 )
        ( ColumnDataList ( PlacementRegionsGetCellRegionDataColumnList CellRegionData ) ) )
    ( foreach ColumnData ColumnDataList             
              (let (
                    ( PRegionRect ( PlacementRegionGetColumnPRegionRect ColumnData ) ) )                  
                ( setq Height ( plus Height ( BBoxGetHeight PRegionRect ) ) ) ) )
    Height ) )


(defun PlacementRegionsGateRegionToPlacementRowRect ( GateRegionRect Offset )
  (let (
        ( LeftEdge ( plus ( RectGetLeft GateRegionRect ) Offset ) )
        ( TopEdge ( RectGetTop GateRegionRect ) )
        ( BottomEdge ( RectGetBottom GateRegionRect ) ) )
    (let (
          ( RightEdge ( plus LeftEdge 0.2 ) ) )
      ( list
        ( list
          LeftEdge
          BottomEdge )
        ( list
          RightEdge
          TopEdge ) ) ) ) )

(defun PlacementRegionsDrawCellRegionData ( CellView
                                            CellRegionData
                                            NRegionLPP
                                            PRegionLPP )
  ( ListNonDestructiveMapCan
    (lambda ( ColumnData )
      ( append
        (when NRegionLPP
          ( list 
            ( dbCreateRect
              CellView
              NRegionLPP
              ( PlacementRegionGetColumnNRegionRect ColumnData ) ) ) )
        (when PRegionLPP
          ( list
            ( dbCreateRect
              CellView
              PRegionLPP
              ( PlacementRegionGetColumnPRegionRect ColumnData ) ) ) ) ) )
    ( PlacementRegionsGetCellRegionDataColumnList CellRegionData ) ) )


(defun PlacementRegionsLeftRegionToPlacementRowRect ( LeftRegionRect
                                                      LeftRegionOffsetFromColumnCenter )
  ( RectMakeRect 
    ( RectGetLeft LeftRegionRect )
    ( RectGetBottom LeftRegionRect )
    ( difference ( RectGetWidth LeftRegionRect ) LeftRegionOffsetFromColumnCenter )
    ( RectGetHeight LeftRegionRect ) ) )


(defun PlacementRegionsRightRegionToPlacementRowRect ( RightRegionRect
                                                       RightRegionOffsetFromColumnCenter )
  ( RectMakeRect 
    ( plus ( RectGetLeft RightRegionRect ) RightRegionOffsetFromColumnCenter )
    ( RectGetBottom RightRegionRect )
    ( difference ( RectGetWidth RightRegionRect ) RightRegionOffsetFromColumnCenter )
    ( RectGetHeight RightRegionRect ) ) )


(defun PlacementRegionsScrapeCellRegionDataFromCellView ( CellView
                                                          Instances
                                                          NChainLibCellPairRegExs
                                                          PChainLibCellPairRegExs
                                                          GateLibCellPairRegExs
                                                          NPlugLibCellPairRegExs
                                                          PPlugLibCellPairRegExs
                                                          EvilGatePRegionOffset
                                                          MinWellSpacing
                                                          Boundary
                                                          HorizontalSpacing
                                                          VerticalSpacing
                                                          )
  (let (
        ( BBox
          Boundary->bBox ) )
    ( PlacementRegionsMakeCellRegionData
      ( mapcar
        `car
        ( PlacementRegionsScrapeColumnDataClumpPairs
          ( cadr ( NameFilterInstances
                   Instances
                   NChainLibCellPairRegExs ) )
          ( cadr ( NameFilterInstances
                   Instances
                   PChainLibCellPairRegExs ) )
          ( cadr ( NameFilterInstances
                   Instances
                   GateLibCellPairRegExs ) )
                                        ;note nplug is in pregion
          ( cadr ( NameFilterInstances
                   Instances
                   PPlugLibCellPairRegExs ) )
          ( cadr ( NameFilterInstances
                   Instances
                   NPlugLibCellPairRegExs ) )
          EvilGatePRegionOffset
          ( append
            NPlugLibCellPairRegExs
            PPlugLibCellPairRegExs )
          MinWellSpacing
          ( BBoxExpandHorizontal
            ( BBoxExpandVertical BBox -VerticalSpacing )
            -HorizontalSpacing )
          ) )
      BBox ) ) )


(defun PlacementRegionsScrapeColumnDataClumpPairs ( NComponents
                                                    PComponents
                                                    Gates
                                                    NPlugs
                                                    PPlugs
                                                    EvilGatePRegionOffset
                                                    PlugLibCellPairRegExs
                                                    MinWellSpacing 
                                                    BBox )
  (let (
        ( InstanceTable ( makeTable `foo nil ) )
        ( PlugPredicate
          (lambda ( PlugRegion Region )
            (let (
                  ( F
                    (cond (
                           ( and
                             ( car PlugRegion )
                             ( car Region ) )
                           `car )
                          (
                           ( and
                             ( cadr PlugRegion )
                             ( cadr Region ) )
                           `cadr ) ) ) )
              (when F
                  (let (
                        ( Range
                          ( apply F ( list Region ) ) )
                        ( PlugRange
                          ( apply F ( list PlugRegion ) ) ) )
                    (when ( and Range PlugRange )
                      ( RangeIsRangeInRangeClose
                        ( ListFindMinimumElement 
                          ( setof Region
                                  ( mapcar F RegionsBeforePlugs )
                                  ( apply F ( list Region ) ) )
                          (lambda ( Range )
                            ( RangeGetDistance
                              Range
                              PlugRange
                              ) ) )
                        Range
                        1e-9 ) ) ) ) ) ) )
        ( GateRegionsFunc
          (lambda ( Gate )
            ( list
              ( NPSearchGateGetNRange 
                Gate
                EvilGatePRegionOffset )
              ( NPSearchGateGetPRange 
                Gate
                EvilGatePRegionOffset ) ) ) )
        ( NRegionsFunc
          (lambda ( Component )
            ( list
              ( RectGetXRange ( getq Component bBox ) )
              nil ) ) )
        ( PRegionsFunc
          (lambda ( Component )
            ( list
              nil
              ( RectGetXRange ( getq Component bBox ) ) ) ) ) )

    ( FigGetClumpTable
      Gates
      ?Clumpify GateRegionsFunc 
      ?ClumpPredicate `PlacementRegionsSameColumn
      ?ClumpUnion `PlacementRegionsUnion 
      ?InstanceTable InstanceTable )

    ( FigGetClumpTable
      PComponents
      ?Clumpify PRegionsFunc 
      ?ClumpPredicate `PlacementRegionsSameColumn
      ?ClumpUnion `PlacementRegionsUnion 
      ?InstanceTable InstanceTable )
    

    ( FigGetClumpTable
      NComponents
      ?Clumpify NRegionsFunc 
      ?ClumpPredicate `PlacementRegionsSameColumn
      ?ClumpUnion `PlacementRegionsUnion 
      ?InstanceTable InstanceTable )

    ;plug is in some region if it contains the closest region
    (let (
          ( RegionsBeforePlugs
            ( mapcar `car ( tableToList InstanceTable ) ) ) )
      ( FigGetClumpTable
        NPlugs
        ?Clumpify NRegionsFunc
        ?ClumpPredicate PlugPredicate
        ?ClumpUnion `PlacementRegionsUnion
        ?InstanceTable InstanceTable )
      ( FigGetClumpTable
        PPlugs
        ?Clumpify PRegionsFunc
        ?ClumpPredicate PlugPredicate
        ?ClumpUnion `PlacementRegionsUnion
        ?InstanceTable InstanceTable )

;      ( println ( mapcar `car ( tableToList InstanceTable  ) ) )

    ( mapcar 
      (lambda ( Entry )
        (let (
              ( Region ( car Entry ) )
              ( Instances ( cadr Entry ) ) )
          (let (
                ( NPRegionParity
                  ( leqp ( caadr Region )
                         ( caar Region ) ) )
                ( YRange
                  ( RectGetYRange 
                    BBox ) )
                )
            (let (
                  ( NRegionRect 
                    ( RectMakeFromRanges 
                      ( car Region )
                      YRange ) )
                  ( PRegionRect 
                    ( RectMakeFromRanges 
                      ( cadr Region )
                      YRange ) )
                  ( BottomGateRegionRect
                    ( RectMakeFromRanges 
                      ( RangeUnion ( car Region )
                                   ( cadr Region ) )
                      ( list ( car YRange )
                             ( car YRange ) ) ) )
                  ( TopGateRegionRect
                    ( RectMakeFromRanges 
                      ( RangeUnion ( car Region )
                                   ( cadr Region ) )
                      YRange ) )
                  )
              ( list
                ( PlacementRegionMakeColumnData 
                  NRegionRect
                  PRegionRect
                  NPRegionParity
                  BottomGateRegionRect
                  TopGateRegionRect )
                ( cadr Entry ) ) ) ) ) )
      (let (
            ( Entrys
              ( PlacementRegionsMergeEntrys
                ( sort
                  ( tableToList InstanceTable )
                  (lambda ( E1 E2 )
                    ( apply
                      `PlacementRegionsSortFunc
                      ( mapcar `car ( list E1 E2 ) ) ) ) ) ) ) )
;        ( println ( mapcar `car Entrys ) )

        ( mapcar
          (lambda ( Region Instances )
            ( list Region Instances ) )
          ( PlacementRegionsAbutRegions
            ( PlacementRegionsFixRegions
              ( mapcar `car Entrys ) )
            ( RectGetXRange BBox ) )
          ( mapcar `cadr Entrys ) ) ) ) ) ) )

(defun PlacementRegionsFixRegions ( Regions )
  (let (
        ( LastParity t ) )
    ( mapcar
      (lambda ( Region )
        (let (
              ( Parity
                ( null LastParity ) ) )
          (let (
                ( Ret
                  (cond (
                         ( and ( car Region )
                               ( cadr Region ) )
                         ( setq Parity ( leqp ( caadr Region ) ( caar Region ) ) )
                         Region )
                        (
                         ( null ( cadr Region ) )
                         ( list 
                           ( car Region )
                           (cond (
                                  Parity
                                  ( list 
                                    ( caar Region )
                                    ( caar Region ) ) )
                                 (
                                  ( list
                                    ( cadar Region )
                                    ( cadar Region ) ) ) ) ) )
                        (
                         ( null ( car Region ) )
                         ( list 
                           (cond (
                                  Parity
                                  ( list 
                                    ( cadadr Region )
                                    ( cadadr Region ) ) )
                                 (
                                  ( list ( caadr Region )
                                         ( caadr Region ) ) ) )
                           ( cadr Region ) ) ) ) ) )
            ( setq LastParity Parity )
            Ret ) ) )
      Regions ) ) )

;merges an n-only and a p-only entry            
(defun PlacementRegionsMergeEntry ( NEntry PEntry )
  ( list
    ( list ( car ( car NEntry ) ) ( cadr ( car PEntry ) ) )
    ( append ( cadr NEntry ) ( cadr PEntry ) ) ) )

;geiven a list of range/instances entrys, merge together entrys with nil n or p ranges
(defun PlacementRegionsMergeEntrys ( Entrys )
  (let (
        ( LastEntry nil ) )
  ( setof 
    Entry
    ( maplist
      (lambda ( Rest )
        (let (
              ( Entry ( car Rest ) )
              ( NextEntry ( cadr Rest ) ) )
          (let ( 
                ( Ret
                  (cond (
                         ( and LastEntry
                               ( null ( caar LastEntry ) )
                               ( null ( cadar Entry ) ) )
                         ( PlacementRegionsMergeEntry 
                           Entry
                           LastEntry ) )
                        ( 
                         ( and LastEntry
                               ( null ( caar Entry ) )
                               ( null ( cadar LastEntry ) ) )
                         ( PlacementRegionsMergeEntry 
                           LastEntry
                           Entry ) )
                        (
                        ;return as is if it has both n and p, or its the last, or it doesn't match with next 
                         ( or
                           ( and ( caar Entry ) ( cadar Entry ) )
                           ( null NextEntry )
                           ( not
                             ( or
                               ( and 
                                 ( cadar Entry )
                                 ( null ( caar Entry ) )
                                 ( caar NextEntry )
                                 ( null ( cadar NextEntry ) ) )
                               ( and 
                                 ( caar Entry )
                                 ( null ( cadar Entry ) )
                                 ( cadar NextEntry )
                                 ( null ( caar NextEntry ) ) ) ) ) )
                         Entry )
                        ) ) )
            ( setq LastEntry Entry )
            Ret ) ) )
        Entrys )
      Entry ) ) )
                       
(defun PlacementRegionsAbutRegions ( Regions XRange )
  (let (
        ( LeftEdge 
          ( car XRange ) )
        ( Last nil ) )
    ( ListMap
      (lambda ( Region )
        (let (
              ( Parity
                ( leqp ( caadr Region )
                       ( caar Region ) ) )
              ( RightEdge
                (cond (
                       Last
                       ( cadr XRange ) )
                      (
                       ( max ( cadadr Region )
                             ( cadar Region ) ) ) ) )
              )
          (let (
                ( Junction
                  (if Parity
                      ( cadadr Region )
                    ( cadar Region ) ) ) )
            (let (
                  ( Ret
                    ( list
                      ( list LeftEdge Junction )
                      ( list Junction RightEdge ) ) ) )
              ( setq LeftEdge RightEdge )
              (if Parity
                  ( reverse Ret )
                Ret ) ) ) ) )
      Regions
      ?PreLast (lambda ( x ) ( setq Last t ) )
      ) ) )


(defun PlacementRegionsSortFunc ( Region1 Region2 )
  ( lessp
    ( ListFindMinimum
      ( ListNonDestructiveMapCan
        (lambda ( Range ) Range )
        Region1 )
      (lambda ( x ) x ) )
    ( ListFindMinimum
      ( ListNonDestructiveMapCan
        (lambda ( Range ) Range )
        Region2 )
      (lambda ( x ) x ) ) ) )

(defun PlacementRegionsSameColumn ( Regions1 Regions2 )
  ( or
    ( and ( car Regions1 ) ( car Regions2 )
          ( RangeDoIntersect ( car Regions1 ) ( car Regions2 ) 1e-9  ) )
    ( and ( cadr Regions1 ) ( cadr Regions2 )
          ( RangeDoIntersect ( cadr Regions1 ) ( cadr Regions2 ) 1e-9  ) ) ) )

(defun PlacementRegionsUnion ( Regions1 Regions2 )
  ( mapcar
    (lambda ( Region1 Region2 )
      ( RangeUnion Region1 Region2 ) )
    Regions1
    Regions2 ) )
              
