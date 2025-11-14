; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/CreateCDL.il#11 $
; $DateTime: 2004/08/21 16:39:54 $
; $Author: clayton $


( list "5.Flush Design and Sync" "UIFlushAndSync" )


( UICreateMenuItemCallBack
  `UIFlushAndSyncCB
  (lambda ()
    (FlushAndSync (geGetEditCellView))
  ) 
)
