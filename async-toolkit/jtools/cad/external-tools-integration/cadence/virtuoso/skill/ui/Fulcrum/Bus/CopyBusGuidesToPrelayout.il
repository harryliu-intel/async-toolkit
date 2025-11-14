; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

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

