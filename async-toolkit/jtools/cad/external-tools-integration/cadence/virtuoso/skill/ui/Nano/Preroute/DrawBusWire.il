; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

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

