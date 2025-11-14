; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: LVSNodes
; Parameter: CellView (CVId)
; Return:  CellView (CVId)
;
; Remove LVS Nodes

defun( LVSNodes ( CellView )
    ( LVSNodesDrawFromDirectives 
      CellView
      DirectiveTable
      Metal1LPP
      ContactLibCellExpressionPairs
      )
  ( dbSave CellView )
 CellView
)
