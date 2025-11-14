; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/intel/cad/external-tools-integration/cadence/virtuoso/skill/ui/Fulcrum/SyncToNetList.il#1 $
; $DateTime: 2011/10/03 13:24:55 $
; $Author: aubrey $


( list "05. Draw Power Struts" "UILeafDrawStruts" )


( UICreateMenuItemCallBack 
  `UILeafDrawStrutsCB
  (lambda () (VStruts (UIGetCellView)))
  )

