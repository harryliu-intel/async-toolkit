; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: RegionWell
; Parameter: CellView (CVId) 
; Return: CellView (CVId)
;
; 


defun( RegionWell ( CellView )
  
  ( foreach Shape
            ( append
              ( PinUtilGetAllShapesOnLPP
                CellView
                NWellLPP )
              ( PinUtilGetAllShapesOnLPP
                CellView
                PWellLPP ) )
            ( dbDeleteObject Shape ) )

  ( RegionWellDrawWellInTransistorRegions
    CellView
    CellRegionData
    NWellLPP
    PWellLPP
    NImplantLPP
    PImplantLPP )
  ( dbSave CellView )
  CellView
)
