; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: SpaceStacks
; Parameter: CellView (CVId)
;            ColumnStackSpaceThreshold (float)
;            ColumnStackSpaceMinSpacing (float)
;            ColumnStackSpaceMaxShift (float)
; Return: CellView (CVId)
;
; Spacing the stacks
;
;
defun( SpaceStacks ( CellView 
                     ColumnStackSpaceThreshold 
                     ColumnStackSpaceMinSpacing 
                     ColumnStackSpaceMaxShift)
  ColumnStackSpaceThreshold = ColumnStackSpaceThreshold * UserUnitsPerMeter
  ColumnStackSpaceMinSpacing = ColumnStackSpaceMinSpacing * UserUnitsPerMeter
  ColumnStackSpaceMaxShift = ColumnStackSpaceMaxShift * UserUnitsPerMeter

    (let (
          ( Components
            ( cadr
              ( NameFilterInstances
                ( getq CellView instances )
                ( append
                  ( append NSuperStackLibCellPairRegExs 
                           PSuperStackLibCellPairRegExs  )
                  PlugLibCellPairRegExs ) ) ) )

          ( MoveTest
            (lambda ( Component ColumnStackSpaceThreshold Region )
              ( and
                ( RangeIsNumberInRangeClose
                  ( car ( RectGetCenter ( getq Component bBox ) ) )
                  ( RectGetXRange Region )
                  1e-9  )
                ( lessp
                  ( RectGetWidth ( getq Component bBox ) )
                  ColumnStackSpaceThreshold )
                (let (
                      ( Contacts 
                        ( setof Conn ( getq Component conns )
                                ( rexMatchp 
                                  ".*\\.[DS]$"
                                  ( getq ( getq Conn term ) name ) ) ) ) )
                  ( leqp
                    ( length 
                      ( setof Conn Contacts
                              ( or
                                ( equal ( getq ( getq Conn net ) name ) GNDNetName )
                                ( equal ( getq ( getq Conn net ) name ) VddNetName ) ) ) )
                    ( round ( times ( length Contacts ) 0.4 ) )
                  ) ) ) ) ) )
      ( foreach
        Column
        ( PlacementRegionsGetCellRegionDataColumnList
          CellRegionData )
        (let (
              ( LeftRegion ( PlacementRegionGetColumnLeftRegionRect Column ) )
              ( RightRegion ( PlacementRegionGetColumnRightRegionRect Column ) )
              )

          (let (
                ( LComponents
                  ( setof Component Components
                          ( apply
                            MoveTest 
                            ( list Component ColumnStackSpaceThreshold LeftRegion) )
                          ) )
                ( RComponents
                  ( setof Component Components
                          ( apply
                            MoveTest
                            ( list Component ColumnStackSpaceThreshold RightRegion ) )
                          ) )
                )
          ( println ( list
                      LeftRegion
                      RightRegion
                      LComponents
                      RComponents
                      ColumnStackSpaceThreshold ) )
            ( foreach BoundAndClump
                      ( tableToList
                        ( FigGetClumpTable
                          LComponents
                          ?ClumpPredicate (lambda ( Rect1 Rect2 )
                                            ( RangeDoIntersect
                                              ( RectGetYRange Rect1 )
                                              ( RectGetYRange Rect2 )
                                              1e-9 ) ) ) )
                      (let (
                            ( Box ( car BoundAndClump ) )
                            ( Clump ( cadr BoundAndClump ) ) 
                            )
                        (when ( cadr ( NameFilterInstances
                                       Clump
                                       SuperStackLibCellPairRegExs ) )
                        ( FigStackHorizontalMaintainSep
                          Clump
                          ( min
                            ( RectGetLeft Box )
                            ( max
                              ( difference 
                                ( RectGetLeft Box )
                                ColumnStackSpaceMaxShift )
                              ( plus 
                                ( RectGetLeft LeftRegion )
                                ColumnStackSpaceMinSpacing ) ) ) ) ) ) )
                          

            ( foreach BoundAndClump
                      ( tableToList
                        ( FigGetClumpTable
                          RComponents
                          ?ClumpPredicate (lambda ( Rect1 Rect2 )
                                            ( RangeDoIntersect
                                              ( RectGetYRange Rect1 )
                                              ( RectGetYRange Rect2 )
                                              1e-9 ) ) ) )
                      (let (
                            ( Box ( car BoundAndClump ) )
                            ( Clump ( cadr BoundAndClump ) ) 
                            )
                        (when ( cadr ( NameFilterInstances
                                       Clump
                                       SuperStackLibCellPairRegExs ) )
                        ( FigStackHorizontalMaintainSep
                          Clump
                          ( difference 
                            ( max 
                              ( RectGetRight Box )
                              ( min
                                ( plus ( RectGetRight Box )
                                       ColumnStackSpaceMaxShift )
                                ( difference
                                  ( RectGetRight RightRegion )
                                  ColumnStackSpaceMinSpacing ) ) )
                            ( RectGetWidth Box ) ) ) ) ) ) ) ) )
      ( dbSave CellView )
      )
CellView
)






