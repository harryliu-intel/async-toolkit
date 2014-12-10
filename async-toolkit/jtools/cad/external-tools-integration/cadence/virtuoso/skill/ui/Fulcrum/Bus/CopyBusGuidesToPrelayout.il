; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id: $
; $DateTime: $
; $Author: $

;
; menu item Fulcrum/Bus
;
; reference function from layout/bus/bus.il
;
( list "Copy Guides to Prelayout" "UICopyBusGuidesToPrelayout" )


( UICreateMenuItemCallBack
  `UICopyBusGuidesToPrelayoutCB
  (lambda ()
    (CopyBusGuidesToPrelayout) 
  ) 
)

