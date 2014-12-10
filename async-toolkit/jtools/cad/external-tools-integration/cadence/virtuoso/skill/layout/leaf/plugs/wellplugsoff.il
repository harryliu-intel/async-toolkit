; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: WellPlugsOn
; Parameter: CellView (CVId)
; Return: CellView (CVId)
;
; Force turn off the well plugs
;


defun( WellPlugsOff ( CellView )


  ( foreach Gate ( cadr ( NameFilterInstances
                          ( getq CellView instances )
                          GateLibCellPairRegExs ) )
            (when ( PCellGetParameterValue Gate "worst_case_folding" )
              ( dbReplaceProp Gate "worst_case_folding" "boolean" "TRUE" ) )
            (when ( PCellGetParameterValue Gate "well_plugs" )
              ( dbReplaceProp Gate "well_plugs" "boolean" "FALSE" ) ) )


  ( dbSave CellView )
CellView
)




