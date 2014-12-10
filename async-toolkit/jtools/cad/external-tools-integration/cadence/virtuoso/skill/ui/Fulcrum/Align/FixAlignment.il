; Copyright 2004 Fulcrum Microsystems.  All rights reserved.
; $DateTime: 2009/10/05 15:21:21 $
; $Author: stevemc $

( list "Fix X Alignment" "UIFixXAlignment" )

( UICreateMenuItemCallBack
  `UIFixXAlignmentCB
  (lambda ()
    (fixXAlignment ?cv (geGetWindowCellView))
  )
)

