; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: Squish
; Parameter: CellView (CVId)
; Return: CellView (CVId)
;
; squish the cells in x direction to make it narrower.x

defun( Squish ( CellView )

                                      ;get new regions 
  (defvar CellRegionData
    ( SquishSquishCellRegionDataAndComponentsNoRouterWidth
      CellView 
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
      CellView )
    ( PlacementRegionsGetCellRegionDataBBox
      CellRegionData )
    bBox )
  
  ( dbSave CellView )
 CellView
 )

(defun SquishSquishCellRegionDataAndComponentsNoRouterWidth ( CellView 
                                                              CellRegionData 
                                                              MinRegionWidth
                                                              MinWellSpacing
                                                              GridWidth
                                                              LeftBuffer
                                                              RightBuffer
                                                              GateLibCellPairRegExs
                                                              NComponentLibCellPairRegExs
                                                              PComponentLibCellPairRegExs
                                                              PlugLibName 
                                                              NRegionPlugCellName 
                                                              PRegionPlugCellName 
                                                              PlugViewName 
                                                              NImplantLPP
                                                              PImplantLPP
                                                              EvilGatePRegionOffset
                                                              ExtraGateWidth
                                                              ContactLPP )
  ( SquishSquishCellRegionDataAndComponents
    CellView 
    CellRegionData 
    MinRegionWidth
    MinWellSpacing
    GridWidth
    LeftBuffer
    RightBuffer
    ( ListFillList 
      (lambda ( I ) 0.0 )
      ( plus
        ( length ( PlacementRegionsGetCellRegionDataColumnList 
                   CellRegionData ) ) 1 ) )
    GateLibCellPairRegExs
    NComponentLibCellPairRegExs
    PComponentLibCellPairRegExs
    PlugLibName 
    NRegionPlugCellName 
    PRegionPlugCellName 
    PlugViewName 
    NImplantLPP
    PImplantLPP
    EvilGatePRegionOffset
    ExtraGateWidth
    ContactLPP ) )

(defun SquishSquishCellRegionDataAndComponents ( CellView 
                                                 CellRegionData 
                                                 MinRegionWidth
                                                 MinWellSpacing
                                                 GridWidth
                                                 LeftBuffer
                                                 RightBuffer
                                                 GapWidthList
                                                 GateLibCellPairRegExs
                                                 NComponentLibCellPairRegExs
                                                 PComponentLibCellPairRegExs
                                                 PlugLibName 
                                                 NRegionPlugCellName 
                                                 PRegionPlugCellName 
                                                 PlugViewName 
                                                 NImplantLPP
                                                 PImplantLPP
                                                 EvilGatePRegionOffset
                                                 ExtraGateWidth
                                                 ContactLPP )
  (let (
        ( NRegionPlugRects
          ( DrawImplantFindPlugs 
            CellView
            PlugLibName
            NRegionPlugCellName
            PlugViewName
            PImplantLPP
            EvilGatePRegionOffset
            NImplantLPP
            ContactLPP ) )
        ( PRegionPlugRects
          ( DrawImplantFindPlugs 
            CellView
            PlugLibName
            PRegionPlugCellName
            PlugViewName
            NImplantLPP
            EvilGatePRegionOffset
            NImplantLPP
            ContactLPP ) )
        ( Gates ( cadr ( NameFilterInstances 
                         ( getq CellView instances )
                         GateLibCellPairRegExs ) ) )
        ( NComponents ( cadr ( NameFilterInstances 
                               ( getq CellView instances )
                               NComponentLibCellPairRegExs ) ) )
        ( PComponents ( cadr ( NameFilterInstances 
                               ( getq CellView instances )
                               PComponentLibCellPairRegExs ) ) )
        ( ColumnDataList ( PlacementRegionsGetCellRegionDataColumnList CellRegionData ) )
        ( NewColumnDataList nil )
        ( IsFirstColumn t )
        ( BoundaryBox ( PlacementRegionsGetCellRegionDataBBox CellRegionData ) ) )
    (let (
          ( PlugRects ( append NRegionPlugRects PRegionPlugRects ) )
          ( Components ( append ( append NComponents PComponents ) Gates ) )
          ( CurrColumnLeft ( RectGetLeft ( PlacementRegionGetColumnLeftRegionRect ( car ColumnDataList ) ) ) ) )
      (let ( 
            ( HorizontalDistanceToBoundary ( difference CurrColumnLeft ( caar BoundaryBox ) ) ) )
        (while ColumnDataList         
          (let ( 
                ( ColumnData ( car ColumnDataList ) )
                ( IsLastColumn ( null ( cdr ColumnDataList ) ) ) )
          (let ( 
                ( NPRegionParity ( PlacementRegionGetNPRegionParity ColumnData ) )
                ( ColumnBBox ( PlacementRegionGetColumnBBox ColumnData ) )
                ( LeftRegionRect ( PlacementRegionGetColumnLeftRegionRect ColumnData ) )
                ( RightRegionRect ( PlacementRegionGetColumnRightRegionRect ColumnData ) )
                ( TopGateRegionRect ( PlacementRegionGetColumnTopGateRegionRect ColumnData ) )
                ( BottomGateRegionRect ( PlacementRegionGetColumnBottomGateRegionRect ColumnData ) ) )
            (let (
                  ( PlugRectsInColumn 
                    ( setof PlugRect PlugRects
                            ( RectDoRectsOverlap PlugRect ColumnBBox ) ) )
                  ( ComponentsInColumn 
                    ( setof Component Components
                            ( RectDoRectsOverlap ( getq Component bBox ) ColumnBBox ) ) ) )
              
              ( setq PlugRects
                     ( ListStringListDifference PlugRects PlugRectsInColumn ) )
              ( setq Components
                     ( ListStringListDifference Components ComponentsInColumn ) )
              
              (let (
                    ( MinLeftXMaxRightXPair
                      ( SquishGetMinLeftXMaxRightXPair 
                        ( append 
                          ( mapcar 
                            (lambda ( C ) 
                              ( RectShrinkInX
                                ( getq C bBox )
                                -ExtraGateWidth ) )
                            ComponentsInColumn )
                          PlugRectsInColumn )
                        ( PlacementRegionGetColumnNPJunction ColumnData ) ) ) )
                (let (
                      ( LeftWidthNeededForPlugClearance
                        (if IsFirstColumn
                            ( difference 
                              MinWellSpacing
                              ( difference 
                                (cond (
                                       ( ListFindMinimum 
                                         PlugRectsInColumn
                                         (lambda ( Rect )
                                           ( RectGetLeft Rect ) ) ) )
                                      (
                                       1e99 ) )
                                ( car MinLeftXMaxRightXPair ) ) )
                          0.0
                          ) )
                      ( RightWidthNeededForPlugClearance
                        (if IsLastColumn
                            ( difference 
                              MinWellSpacing
                              ( difference
                                ( cadr MinLeftXMaxRightXPair )
                                (cond (
                                       ( ListFindMaximum 
                                         PlugRectsInColumn
                                         (lambda ( Rect )
                                           ( RectGetRight Rect ) ) ) )
                                      (
                                       ( difference 0 1e99 ) ) ) ) )
                          0.0 ) )
                      ( LeftRouterWidthRequested
                        (if IsFirstColumn 
                            ( car GapWidthList )
                          ( quotient ( car GapWidthList ) 2.0 ) ) )
                      ( RightRouterWidthRequested 
                        (if IsLastColumn ( cadr GapWidthList ) 
                          ( quotient ( cadr GapWidthList ) 2.0 ) ) )
                      )
                  (let (
                        ( NewLeftRegionWidth 
                          ( NPSearchSnapToGrid GridWidth
                                ( difference 
                                  ( max 
                                    ( plus ( difference ( RectGetRight LeftRegionRect ) ( car MinLeftXMaxRightXPair ) )
                                           ( max LeftWidthNeededForPlugClearance LeftRouterWidthRequested ) )
                                    MinRegionWidth )
                                  1e-9 )
                                 ) )
                        ( NewRightRegionWidth 
                          ( NPSearchSnapToGrid GridWidth
                                ( difference 
                                  ( max 
                                    ( plus ( difference 
                                             ( cadr MinLeftXMaxRightXPair )
                                             ( RectGetLeft RightRegionRect ) )
                                           ( max RightWidthNeededForPlugClearance 
                                                 RightRouterWidthRequested ) )
                                    MinRegionWidth )
                                  1e-9 )
                                ) ) )
                    (when IsFirstColumn
                      ( setq NewLeftRegionWidth NewLeftRegionWidth+LeftBuffer ) ) 
                    (when IsLastColumn
                      ( setq NewRightRegionWidth NewRightRegionWidth+RightBuffer ) ) 
                    (let (
                          ( NewGateRegionWidth ( plus NewLeftRegionWidth NewRightRegionWidth ) ) )
                      (let (
                            ( NewLeftRegionRect ( RectMakeRect 
                                                  CurrColumnLeft
                                                  ( cadar LeftRegionRect )
                                                  NewLeftRegionWidth
                                                  ( RectGetHeight LeftRegionRect ) ) )
                            ( NewRightRegionRect ( RectMakeRect 
                                                   ( plus CurrColumnLeft NewLeftRegionWidth )
                                                   ( cadar RightRegionRect )
                                                   NewRightRegionWidth                                      
                                                   ( RectGetHeight RightRegionRect ) ) )
                            ( NewTopGateRegionRect ( RectMakeRect 
                                                     CurrColumnLeft
                                                     ( cadar TopGateRegionRect )                                            
                                                     NewGateRegionWidth
                                                     ( RectGetHeight TopGateRegionRect ) ) )
                            ( NewBottomGateRegionRect ( RectMakeRect 
                                                        CurrColumnLeft
                                                        ( cadar BottomGateRegionRect )
                                                        NewGateRegionWidth
                                                        ( RectGetHeight BottomGateRegionRect ) ) ) )
                        ( setq NewColumnDataList ( tconc 
                                                   NewColumnDataList
                                                   ( PlacementRegionMakeColumnData 
                                                     (if NPRegionParity NewRightRegionRect NewLeftRegionRect )
                                                     (if NPRegionParity NewLeftRegionRect NewRightRegionRect )
                                                     NPRegionParity
                                                     NewBottomGateRegionRect
                                                     NewTopGateRegionRect ) ) )
                        ( foreach Component 
                                  ComponentsInColumn
                                  ( dbMoveFig 
                                    Component 
                                    nil 
                                    ( list 
                                      ( list ( difference ( RectGetRight NewLeftRegionRect ) 
                                                          ( RectGetRight LeftRegionRect ) )
                                             0.0 )
                                      "R0" ) ) )
                        ( setq GapWidthList ( cdr GapWidthList ) )
                        ( setq ColumnDataList ( cdr ColumnDataList ) )
                        ( setq IsFirstColumn nil )
                        ( setq CurrColumnLeft ( plus CurrColumnLeft NewGateRegionWidth ) ) ) ) ) ) ) ) ) ) )
          (let (
                ( NewBoundaryBox
                  ( RectMakeRect 
                    ( caar BoundaryBox ) 
                    ( cadar BoundaryBox )
                    ( plus ( difference CurrColumnLeft ( caar BoundaryBox ) ) 
                           HorizontalDistanceToBoundary )
                    ( RectGetHeight BoundaryBox ) ) ) )
            (let (
                  ( NewCellRegionData ( PlacementRegionsMakeCellRegionData ( car NewColumnDataList ) NewBoundaryBox ) ) )
              NewCellRegionData ) ) ) ) ) )


(defun SquishGetMinLeftXMaxRightXPair ( Rects
                                        MiddleX )
  (let (
        ( MinLeftX MiddleX )
        ( MaxRightX MiddleX ) )
    ( foreach Rect Rects
              (let (
                    ( Right ( RectGetRight Rect ) )
                    ( Left ( RectGetLeft Rect ) ) )
                (when ( lessp Left MinLeftX )
                  ( setq MinLeftX Left ) )
                (when ( greaterp Right MaxRightX )
                  ( setq MaxRightX Right ) ) ) )
    ( list MinLeftX MaxRightX ) ) )

