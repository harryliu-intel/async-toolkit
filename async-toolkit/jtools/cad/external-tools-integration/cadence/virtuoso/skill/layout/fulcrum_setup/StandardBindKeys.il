; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Cadence Standard
; ================

load( prependInstallPath( "/samples/local/leBindKeys.il"))
load( prependInstallPath( "/samples/local/lxBindKeys.il"))
load( prependInstallPath( "/samples/local/schBindKeys.il"))

load( prependInstallPath( "/etc/sted/stroke.il" ))
load( prependInstallPath( "/etc/sted/defstrokes.il" ))
hiLoadStrokeFile( prependInstallPath( "/etc/sted/def.strokes" ) "Layout")

; In-house Skill Files
; ====================
;load "/home/group/cadadmin/technology/archive/TSMC13_micron/cadence_setup/FulcrumSetup.il"

