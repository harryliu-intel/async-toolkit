; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


( list "Sync To NetList" "UISyncToNetList" )


( UICreateMenuItemCallBack 
  `UISyncToNetListCB
  (lambda () (SyncToNetlist ?CV (UIGetCellView)))
  )
