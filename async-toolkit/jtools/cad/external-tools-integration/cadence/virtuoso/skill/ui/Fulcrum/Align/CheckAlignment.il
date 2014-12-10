; Copyright 2004 Fulcrum Microsystems.  All rights reserved.
; $DateTime: 2009/10/05 15:21:21 $
; $Author: stevemc $

( list "Check Alignment" "UICheckAlignment" )

( UICreateMenuItemCallBack
  `UICheckAlignmentCB
  (lambda ()
    (CheckRoutedAlignment (geGetWindowCellView) ?dialog t)
  )
)

