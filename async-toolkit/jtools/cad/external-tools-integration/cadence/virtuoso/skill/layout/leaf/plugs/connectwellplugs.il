; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


; Function: ConnectWellPlugs
; Parameter: CellView (CVId)
; Return: CellView (CVId)
;
; Force connect the well plugs
;
;
;


defun( ConnectWellPlugs ( CellView )
  (let (
        ( NRegionPlugs ( cadr ( NameFilterInstances
                                ( getq CellView instances )
                                ( list ( list WellPlugLibrary 
                                              PWellPlugCellName ) ) ) ) )
        ( PRegionPlugs ( cadr ( NameFilterInstances
                                ( getq CellView instances )
                                ( list ( list WellPlugLibrary 
                                              NWellPlugCellName ) ) ) ) ) )
    ( foreach NRegionPlug NRegionPlugs
              ( PlugsForceConnectPlug 
                NRegionPlug
                CellView
                GNDNetName
                WellPlugTerminalName ) )

    ( foreach PRegionPlug PRegionPlugs
              ( PlugsForceConnectPlug 
                PRegionPlug
                CellView
                VddNetName
                WellPlugTerminalName ) )
             

    ( dbSave CellView )
 ) 
CellView
)

