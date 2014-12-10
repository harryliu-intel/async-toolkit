; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: PlacerCheck
; Parameter: CellView (CVId)
; Return: nil
;
; Run DRC on placed view.
;
;
defun( PlacerCheck ( TargetViewName PlacerCheckDRCAssuraSets )

 (let (
        ( CellView ( dbOpenCellViewByType 
                         LibName
                         CellName
                         TargetViewName 
                         "maskLayout"
                         "a" ) )
      )

  ( setq PlacementGood ( AssuraRunDRC 
                         CellView                          
                         PlacerCheckDRCRuleFile 
                         PlacerCheckDRCAssuraSets 
                         WorkingDir
                         ( sprintf nil "%s/%s" AssuraPlacerCheckDRCDir TargetViewName )
                         ) ) 
  )
)

