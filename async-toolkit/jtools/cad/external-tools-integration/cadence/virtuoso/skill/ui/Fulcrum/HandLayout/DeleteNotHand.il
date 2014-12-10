; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id: $
; $DateTime: $
; $Author: $


( list "Delete All Objects Not Hand Layout" "UIDeleteNotHand" )


( UICreateMenuItemCallBack
  `UIDeleteNotHandCB
  (lambda ()
    (DeleteObjectsNotHandLayout) 
  )
)

