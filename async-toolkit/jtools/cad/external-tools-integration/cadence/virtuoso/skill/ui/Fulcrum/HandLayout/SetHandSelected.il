; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

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

