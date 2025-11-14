; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Copyright 2004 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun wcv () 
  "Quick helper function for getting cellview handles"
  ( geGetWindowCellView ) )

(defun DDOpen ( DDObj )
  "Opens the layout view associated with a lib/cell ddObj."
  ( dbOpenCellViewByType
    DDObj->lib->name
    DDObj->name
    "layout"
    nil
    "a" ) )
