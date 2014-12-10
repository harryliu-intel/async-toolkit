; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id: $
; $DateTime: $
; $Author: $

;
; menu item Fulcrum/Bus
;
; reference function from layout/bus/bus.il
;
( list "Move Guides to buswires cell" "UIMoveGuidesToBuswires" )


( UICreateMenuItemCallBack
  `UIMoveGuidesToBuswiresCB
  (lambda ()
    (MoveGuidesToBuswiresCell)
  )
)

