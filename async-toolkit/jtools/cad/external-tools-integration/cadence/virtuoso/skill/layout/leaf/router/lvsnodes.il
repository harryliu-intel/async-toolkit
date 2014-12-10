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
