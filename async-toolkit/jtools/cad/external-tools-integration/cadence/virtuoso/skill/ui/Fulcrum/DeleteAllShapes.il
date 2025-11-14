; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

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
