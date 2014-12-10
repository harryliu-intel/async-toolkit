; Copyright 2008 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$
( list "Delete All Shapes" "UIDeleteAllShapes" )

( UICreateMenuItemCallBack 
  `UIDeleteAllShapesCB
  (lambda ()
    (let (
          ( CellView ( UIGetCellView ) ) )
       foreach( shape CellView~>shapes
         dbDeleteObject( shape )
       )
    )
  )
)
