; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: AddPlugs
; Parameter: CellView (CVId)
; Return: CellView (CVId)
;
; Force connect the well plugs
;
;
;


defun( AddPlugs ( CellView )

  (let (
        ( WeGoodWithWellPlugs t )
        ( NRegionGatePlugRects
          ( DrawImplantFindPlugs 
            CellView 
            WellPlugLibrary
            PWellPlugCellName
            WellPlugViewName
            PImplantLPP
            ( times EvilGatePRegionOffset UserUnitsPerMeter )
            NImplantLPP
            ContactLPP ) )
        ( PRegionGatePlugRects
          ( DrawImplantFindPlugs 
            CellView 
            WellPlugLibrary
            NWellPlugCellName
            WellPlugViewName
            NImplantLPP
            ( times EvilGatePRegionOffset UserUnitsPerMeter )
            NImplantLPP
            ContactLPP ) ) )

    ( foreach Column ( PlacementRegionsGetCellRegionDataColumnList CellRegionData )
              (let (                
                    ( ColumnBBox ( PlacementRegionGetColumnBBox Column ) )
                    NPRegionParity 
                   )
                   NPRegionParity= ( PlacementRegionGetNPRegionParity Column ) 
                (let (
                      ( NRegionRects
                        ( setof Rect NRegionGatePlugRects
                                ( RectIsRectInRectClose 
                                  Rect
                                  ColumnBBox 
                                  1e-9 ) ) )
                      ( PRegionRects 
                        ( setof Rect PRegionGatePlugRects
                                ( RectIsRectInRectClose 
                                  Rect
                                  ColumnBBox 
                                  1e-9 ) ) ) )             
                  (if ( or ( RangeGetAllRangesNotCovered
                             ( RectGetYRange ColumnBBox )
                             ( mapcar 
                               (lambda ( Rect )
                                 ( RangeMakeRangeFromCenter
                                   ( cadr ( RectGetCenter Rect ) )
                                   ( times UserUnitsPerMeter 
                                           WellPlugEffectiveRadius ) ) )
                               NRegionRects ) )
                           ( RangeGetAllRangesNotCovered
                             ( RectGetYRange ColumnBBox )
                             ( mapcar 
                               (lambda ( Rect )
                                 ( RangeMakeRangeFromCenter
                                   ( cadr ( RectGetCenter Rect ) )
                                   ( times UserUnitsPerMeter 
                                           WellPlugEffectiveRadius ) ) )
                               PRegionRects ) ) )
                      ( setq WeGoodWithWellPlugs nil ) ) ) ) )

    (when ( not WeGoodWithWellPlugs )
      ( println "Drawing Plugs" )
      
      (let (
            ( RegionShapes
              ( PlacementRegionsDrawCellRegionData
                CellView
                CellRegionData
                PWellLPP
                NWellLPP ) ) )
      ( PlugsDrawPlugsOnCellView  
        CellView
        CellRegionData
        WorkingDir
        PlugDRCRuleFile
        GNDNetName
        VddNetName
        NRegionGatePlugRects
        PRegionGatePlugRects
        WellPlugLibrary  
        PWellPlugCellName ;M1_SUB
        NWellPlugCellName ;M1_NWELL
        WellPlugViewName
        WellPlugLPP
        NImplantLPP
        PImplantLPP
        ( times UserUnitsPerMeter WellPlugEffectiveRadius )
        ( times UserUnitsPerMeter MinWellSpacing )
        ?LeaveMess t
        )
      ( foreach Shape RegionShapes
                  ( dbDeleteObject Shape ) )
      ) )
    ; rotate plus when neccessary.
    foreach( inst CellView->instances
      if( inst->cellName==PWellPlugCellName then
         if( dbGetTrueOverlaps( CellView inst->bBox "NW" ) then
           inst->orient="MY"
         )
      )
      if( inst->cellName==NWellPlugCellName then
         overlapShapes=dbGetTrueOverlaps( CellView inst->bBox "NW" )
         foreach( shape overlapShapes
           if( rightEdge( shape ) < rightEdge( inst->bBox) then
             inst->orient="MY"
           )
         )
      )
    )
    ( dbSave CellView )
 ) 
CellView
)

