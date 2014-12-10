; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id: $
; $DateTime: $
; $Author: $


( list "Set Prop on All Objects" "UISetHandAll" )


( UICreateMenuItemCallBack
  `UISetHandAllCB
  (lambda ()
    (SetHandLayoutAllObjects) 
  )
)

