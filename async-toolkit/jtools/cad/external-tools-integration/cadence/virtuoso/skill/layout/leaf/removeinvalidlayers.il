; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

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

