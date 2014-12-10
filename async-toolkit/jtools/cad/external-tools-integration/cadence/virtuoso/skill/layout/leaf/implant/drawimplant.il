; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: DrawImplant
; Parameter: PlacedView (CVId)
; Return: CellView (CVId)
;
; Draw the implant boxes
;
;
defun( DrawImplant ( PlacedView )

  ( DrawImplantOnCellView 
    PlacedView
    CellRegionData
    NImplantLPP 
    PImplantLPP
    WellPlugLibrary
    NWellPlugCellName
    PWellPlugCellName
    WellPlugViewName
    ( times EvilGatePRegionOffset UserUnitsPerMeter )
    ContactLPP
    )
  ( dbSave PlacedView )
  PlacedView
  )


(defun DrawImplantCompareRects ( R1 R2 )
  ( lessp
    ( RectGetBottom
      R1 )
    ( RectGetBottom
      R2 ) ) )

(defun DrawImplantCutRectsOutOfRect ( RectToCutOutFrom RectsToCutOut )
  (let (
        ( LeftEdge ( RectGetLeft RectToCutOutFrom ) )
        ( RightEdge ( RectGetRight RectToCutOutFrom ) )
        ( BottomEdge ( RectGetBottom RectToCutOutFrom ) )
        ( TopEdge ( RectGetTop RectToCutOutFrom ) )
        ( SortedRectsToCutOut ( sort ( copy RectsToCutOut ) `DrawImplantCompareRects ) ) )
    (let (
          ( CurrY BottomEdge )
          ( RemainingRectsToCutOut SortedRectsToCutOut )
          ( Result nil ) )
      (while ( lessp CurrY TopEdge  )
        (let (
              ( CurrRectToCutOut ( car RemainingRectsToCutOut ) ) )
          (if CurrRectToCutOut
              (let (
                    ( RectLeft ( RectGetLeft CurrRectToCutOut ) )
                    ( RectRight ( RectGetRight CurrRectToCutOut ) )
                    ( RectBottom ( RectGetBottom CurrRectToCutOut ) )
                    ( NextY ( min 
                              ( RectGetTop CurrRectToCutOut )
                              TopEdge ) ) )
                ( setq RemainingRectsToCutOut ( cdr RemainingRectsToCutOut ) )
                (let (
                      ( Rect0 ( list
                                ( list LeftEdge CurrY )
                                ( list RightEdge RectBottom ) ) )
                      ( Rect1 ( list
                                ( list LeftEdge RectBottom )
                                ( list RectLeft NextY ) ) )
                      ( Rect2 ( list
                                ( list RectRight RectBottom )
                                ( list RightEdge NextY ) ) ) )
                  ( setq 
                    Result
                    ( cons
                      Rect0
                      ( cons
                        Rect1
                        ( cons Rect2 Result ) ) ) )
                  ( setq CurrY NextY ) ) )
            (let ()
              ( setq
                Result
                ( cons
                  ( list
                    ( list LeftEdge CurrY )
                    ( list RightEdge TopEdge ) )
                  Result ) )
              ( setq CurrY TopEdge ) ) ) ) )
      Result ) ) )

(defun DrawImplantFindPlugs ( CellView
                              PlugLibName
                              PlugCellName
                              PlugViewName
                              ImplantLPP
                              EvilGatePRegionOffset
                              NImplantLPP
                              ContactLPP )
  (let (
        ( PlugFilter 
          ( NameFilterInstances
            ( getq CellView instances )
            ( list ( list PlugLibName PlugCellName ) ) ) ) )
    (let (
          ( Plugs ( cadr PlugFilter ) )
          ( Gates ( car PlugFilter ) ) )
  ( append 
    ( ListApplyFuncToListAndAccumulateNonNilResults
      Gates
      (lambda
        ( Instance LibName CellName ViewName )
        ( FindPlugRectInGateInstance
          Instance
          LibName
          CellName
          ViewName
          ImplantLPP
          EvilGatePRegionOffset
          NImplantLPP
          ContactLPP ) )
      ( list
        PlugLibName
        PlugCellName
        PlugViewName ) )
    ( mapcar 
      (lambda ( PlugInstance )
        ( FindPlugRectInPlugInstance PlugInstance ImplantLPP ) )
      Plugs
      ) ) ) ) )

(defun DrawImplantOnCellView ( CellView 
                               CellRegionData
                               NImplantLPP
                               PImplantLPP
                               PlugLibName
                               NPlugCellName
                               PPlugCellName
                               PlugViewName
                               EvilGatePRegionOffset
                               ContactLPP
                               )
  (let (
        ( NWellPlugList ( DrawImplantFindPlugs 
                          CellView
                          PlugLibName
                          NPlugCellName
                          PlugViewName 
                          NImplantLPP
                          EvilGatePRegionOffset
                          NImplantLPP
                          ContactLPP ) )
        ( PWellPlugList ( DrawImplantFindPlugs
                          CellView 
                          PlugLibName
                          PPlugCellName
                          PlugViewName
                          PImplantLPP
                          EvilGatePRegionOffset
                          NImplantLPP
                          ContactLPP ) ) )
    ;( printf "%L\n" NWellPlugList )
    ( foreach
      Column
      ( PlacementRegionsGetCellRegionDataColumnList CellRegionData )
      (let (
            ( NTransistorRegionRect ( PlacementRegionGetColumnNRegionRect
                                      Column ) )
            ( PTransistorRegionRect ( PlacementRegionGetColumnPRegionRect
                                      Column ) )
            ( BottomGateRegionRect ( PlacementRegionGetColumnBottomGateRegionRect
                                     Column ) )
            ( TopGateRegionRect ( PlacementRegionGetColumnTopGateRegionRect
                                  Column ) ) )
        (let (
              ( BottomOfBottomGateRegion ( RectGetBottom BottomGateRegionRect ) )
              ( TopOfBottomGateRegion ( RectGetTop BottomGateRegionRect ) )
              ( BottomOfTopGateRegion ( RectGetBottom TopGateRegionRect ) )
              ( TopOfTopGateRegion (  RectGetTop TopGateRegionRect ) )
              ( NImplantLeftEdge ( RectGetLeft NTransistorRegionRect ) )
              ( NImplantRightEdge ( RectGetRight NTransistorRegionRect ) )
              ( PImplantLeftEdge ( RectGetLeft PTransistorRegionRect ) )
              ( PImplantRightEdge ( RectGetRight PTransistorRegionRect ) ) )
          (let (
                ( ColumnNImplantRect ( list
                                       ( list NImplantLeftEdge BottomOfBottomGateRegion )
                                       ( list NImplantRightEdge TopOfTopGateRegion ) ) )
                ( ColumnPImplantRect ( list
                                       ( list PImplantLeftEdge BottomOfBottomGateRegion )
                                       ( list PImplantRightEdge TopOfTopGateRegion ) ) ) )
            (let (
                  ( ColumnNPlugs ( setof
                                   PlugRect
                                   NWellPlugList
                                   ( RectDoRectsOverlapOpenRects ColumnPImplantRect PlugRect ) ) )
                  ( ColumnPPlugs ( setof
                                   PlugRect
                                   PWellPlugList
                                   ( RectDoRectsOverlapOpenRects ColumnNImplantRect PlugRect ) ) ) )
              
              (let (
                    ( ColumnNImplantRects ( DrawImplantCutRectsOutOfRect
                                            ColumnNImplantRect
                                            ColumnPPlugs ) )
                    ( ColumnPImplantRects ( DrawImplantCutRectsOutOfRect
                                            ColumnPImplantRect
                                            ColumnNPlugs ) ) )
                ( foreach
                  ImplantRect
                  ColumnNImplantRects
                  (if ( greaterp
                          ( RectGetArea
                            ImplantRect )
                          0.0576 )
                    ( dbCreateRect
                      CellView
                      NImplantLPP
                      ImplantRect ) 
                    ( dbCreateRect
                      CellView
                      PImplantLPP
                      ImplantRect ) ) )
                ( foreach
                  ImplantRect
                  ColumnPImplantRects
                  (if ( greaterp
                          ( RectGetArea
                            ImplantRect )
                          0.0576 ) then
                    ( dbCreateRect
                      CellView
                      PImplantLPP
                      ImplantRect ) 
                    ( dbCreateRect
                      CellView
                      list("PM" "drawing1")
                      ImplantRect ) 
                   else
                    ( dbCreateRect
                      CellView
                      NImplantLPP
                      ImplantRect ) ) ) ) ) ) ) ) )
                      
    t ) )

