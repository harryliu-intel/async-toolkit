; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: RemoveInvalidLayers
; Parameter: CellView (CVId)
; Return:  CellView (CVId)
;
; Remove invalide layers

defun( RemoveInvalidLayers ( CellView )
  ( foreach LPP
            InvalidLPPs
            ( foreach Shape ( PinUtilGetAllShapesOnLPP CellView LPP )
                      ( dbDeleteObject Shape ) ) )
  ( dbSave CellView )
  CellView 
)

