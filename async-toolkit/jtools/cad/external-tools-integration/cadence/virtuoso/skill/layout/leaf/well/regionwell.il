; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun RegionWellDrawWellInTransistorRegions ( CellView 
                                               CellRegionData
                                               NWellLPP
                                               PWellLPP
                                               NImplantLPP
                                               PImplantLPP )

  ( foreach
    Column
    ( PlacementRegionsGetCellRegionDataColumnList
      CellRegionData )
    (let (
          ( NTransistorRegionRect ( PlacementRegionGetColumnNRegionRect
                                    Column ) )
          ( PTransistorRegionRect ( PlacementRegionGetColumnPRegionRect
                                    Column ) )
          ( BottomGateRegionRect ( PlacementRegionGetColumnBottomGateRegionRect
                                   Column ) )
          ( TopGateRegionRect ( PlacementRegionGetColumnTopGateRegionRect
                                Column ) )
          ( ImplantMinWidth 0.0 ) )
      (let (
            ( PWellLeftEdge ( RectGetLeft NTransistorRegionRect ) )
            ( PWellRightEdge ( RectGetRight NTransistorRegionRect ) )
            ( NWellLeftEdge ( RectGetLeft PTransistorRegionRect ) )
            ( NWellRightEdge ( RectGetRight PTransistorRegionRect ) ) )
        (let (
            ;N Transistors go in a P-Well
            ( TopPWellRect ( list 
                             ( list 
                               PWellLeftEdge
                               ( RectGetBottom TopGateRegionRect ) )
                             ( list
                               PWellRightEdge
                               ( plus 
                                 ( RectGetTop TopGateRegionRect ) 
                                 ImplantMinWidth ) ) ) )
            ;N Transistors go in a P-Well
            ( BottomPWellRect ( list 
                                ( list 
                                  PWellLeftEdge
                                  ( difference
                                    ( RectGetBottom BottomGateRegionRect ) 
                                    ImplantMinWidth ) )
                                ( list
                                  PWellRightEdge
                                  ( RectGetTop BottomGateRegionRect ) ) ) )

            ;P Transistors go in a N-Well
            ( TopNWellRect ( list 
                             ( list 
                               NWellLeftEdge
                               ( RectGetBottom TopGateRegionRect ) )
                             ( list
                               NWellRightEdge
                               ( plus
                                 ( RectGetTop TopGateRegionRect ) 
                                 ImplantMinWidth ) ) ) )
            ;P Transistors go in a N-Well
            ( BottomNWellRect ( list 
                                ( list 
                                  NWellLeftEdge
                                  ( difference
                                    ( RectGetBottom BottomGateRegionRect ) 
                                    ImplantMinWidth ) )
                                ( list
                                  NWellRightEdge
                                  ( RectGetTop BottomGateRegionRect ) ) ) ) )

        (when ( and 
                PWellLPP
                ( greaterp ( RectGetArea NTransistorRegionRect ) 0.1 ) )
          ( dbCreateRect
            CellView
            PWellLPP
            NTransistorRegionRect ) )
        
        (when ( and 
                NWellLPP
                ( greaterp ( RectGetArea PTransistorRegionRect ) 0.1 ) )
          ( dbCreateRect
            CellView
            NWellLPP
            PTransistorRegionRect ) )

        (when ( and 
                PWellLPP
                ( greaterp ( RectGetArea TopPWellRect ) 0.1 )
                )
          ( dbCreateRect
            CellView
            PWellLPP
            TopPWellRect ) )

        (when ( and 
                NWellLPP
                ( greaterp ( RectGetArea TopNWellRect ) 0.1 ) )
          ( dbCreateRect
            CellView
            NWellLPP
            TopNWellRect ) )

        (when ( and 
                PWellLPP
                ( greaterp ( RectGetArea BottomPWellRect ) 0.1 ) )
          ( dbCreateRect
            CellView
            PWellLPP
            BottomPWellRect ) )

        (when ( and 
                NWellLPP
                ( greaterp ( RectGetArea BottomNWellRect ) 0.1 )
                )
          ( dbCreateRect
            CellView
            NWellLPP
            BottomNWellRect ) ) ) ) ) ) )
      
