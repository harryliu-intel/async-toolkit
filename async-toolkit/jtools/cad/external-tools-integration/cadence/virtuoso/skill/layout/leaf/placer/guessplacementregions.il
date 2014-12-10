; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: GuessPlacementRegions
; Parameter: CellView (CVId)
; Return: CellView (CVId)
;
; Defines how large an region to contain all the components.
;
;
;


defun( GuessPlacementRegions ( CellView )

(defvar
  GateRegionHeight
  0 )

(unless InitialColumnCount
    (let (
          ( Filter1
            ( NameFilterInstances 
              ( getq CellView instances )
              GateSuperStackLibCellPairRegExs ) ) )
      (let (
            ( Filter2
              ( NameFilterInstances 
                ( car Filter1 )
                NSuperStackLibCellPairRegExs ) ) )
        (let (
              ( NComponents
                ( cadr Filter2 ) )
              ( PComponents
                ( car Filter2 ) )
              ( GateComponents
                ( cadr Filter1 ) ) )
          (defvar InitialColumnCount
            ( GuessPlacementRegionsGetInitialColumnCount
              CellView
              YPitch
              LeafCellMinVerticalSpacingToBoundary
              NComponents
              PComponents
              GateComponents
              ) )

          ( println 
            ( list ( NPSearchGetWidthFromIndex NWidthData 0 )
                   ( NPSearchGetWidthFromIndex PWidthData 0 )
                   TargetCellWidth ) )

          ( LinearSearchInitTable 
            ColumnSearchTable
            InitialColumnCount
            ( ceiling 
              ( quotient 
                TargetCellWidth 
                ( plus ( NPSearchGetWidthFromIndex NWidthData 0 )
                       ( NPSearchGetWidthFromIndex PWidthData 0 ) ) ) )
            InitialColumnCount  )
          ) ) ) ) 


( printf "ColumnSearchTable=" )
( println ( tableToList ColumnSearchTable ) )
(defvar ColumnCount 
  (if ( equal CurrentSearchTable ColumnSearchTable )
      ( arrayref ColumnSearchTable "trial" )
    ( arrayref ColumnSearchTable "max" ) )
    )

CellView
)

(defun GuessPlacementRegionsGetHeightOfComponents  ( 
                                                    Components )
  ( ListFindSum
    Components
    (lambda ( Component ) ( RectGetHeight ( getq Component bBox ) ) ) ) 
)

(defun GuessPlacementRegionsGetInitialColumnCount  ( CellView
                                                     YPitch
                                                     MinVerticalSpacingToBoundary
                                                     NComponents 
                                                     PComponents 
                                                     GateComponents 
                                                     )
  (let (
        ( TotalComponentHeight
          ( plus 
            ( max 
              ( GuessPlacementRegionsGetHeightOfComponents NComponents )
              ( GuessPlacementRegionsGetHeightOfComponents PComponents ) )
            ( GuessPlacementRegionsGetHeightOfComponents GateComponents ) ) )
        ( TotalAvailableHeight 
          ( PlacementRegionsCalculateTotalGateAndTransistorRegionsHeight
            YPitch
            MinVerticalSpacingToBoundary ) ) )
    ( ceiling
      ( quotient
        ( times 0.8 TotalComponentHeight )
        TotalAvailableHeight ) ) ) 
)

