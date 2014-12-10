; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: InlineSuperstacks
; Parameter: CellView (CVId)
; Return: CellView (CVId)
;
; Flatten one level of Hirachy of stack component to make all 
; instance gates
;
;

defun( InlineSuperstacks ( CellView  InlineMultipleGateStacks )

  (let (
        ( SuperStacks
          ( cadr ( NameFilterInstances 
                   ( getq CellView instances )
                   SuperStackLibCellPairRegExs ) ) ) )
                   
  ( InlineInstances
    CellView
    CellView
    ( setof SuperStack SuperStacks
            ( or ( not ( equal ( getq SuperStack cellName )
                               GateSuperStackCellName ) )
                 InlineMultipleGateStacks
                 ( equal 
                   ( length 
                     ( SuperStackGetComponentNameListFromSuperStackName
                       ( getq SuperStack name ) ) )
                   1 ) ) )
    "netlist"
    nil 
    FoldableCellLibCellPairRegExs
    SuperStackLibCellPairRegExs
    ?Verbose nil )

  ( dbSave CellView )
 ) 
CellView
)


