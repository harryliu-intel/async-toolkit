; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: DrawDirectiveKeepout
; Parameter: CellView (CVId)
; Return:  DRCStatus (boolean)
;
; Draw Keepout

defun( DrawDirectiveKeepout ( CellView )


  ( KeepOutDrawKeepOut 
    CellView  
    DirectiveTable
    ( mapcar 
      (lambda ( LPP )
        ( KeepOutCreateRoutingLayerStruct LPP ?Purpose "dummy" ) )
      ( list Metal3LPP ) )
    ( mapcar 
      (lambda ( LPP )
        ( KeepOutCreateRoutingLayerStruct LPP ?Purpose "dummy" ) )
      ( list Metal2LPP ) )
    BoundaryLPP 
    UserUnitsPerMeter
    DirectiveUnitsPerMeter
    ( list GNDNetName VddNetName )
    ( list nil ) ;    nil
    t
    nil
    0 )
  
  ( dbSave CellView )
 CellView 
)
