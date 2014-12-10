; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id: $
; $DateTime: $
; $Author: $


( list "Set Prop on Selected Objects    Ctrl-h" "UISetHandSelected" )


( UICreateMenuItemCallBack
  `UISetHandSelectedCB
  (lambda ()
    (SetHandLayoutSelectedObjects) 
  )
)

