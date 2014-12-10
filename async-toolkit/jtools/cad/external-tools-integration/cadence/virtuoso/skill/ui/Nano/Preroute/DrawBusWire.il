; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id: $
; $DateTime: $
; $Author: $

;
; menu item Nano/Preroute
;
; reference function from layout/bus/bus.il
;
( list "Draw Bus Wires" "UIDrawBusWires" )


( UICreateMenuItemCallBack
  `UIDrawBusWiresCB
  (lambda ()
    DrawWires()
  )
)

