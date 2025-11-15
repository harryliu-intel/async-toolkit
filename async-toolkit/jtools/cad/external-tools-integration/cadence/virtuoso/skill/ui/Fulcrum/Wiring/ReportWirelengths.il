; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id: $
; $DateTime: $
; $Author: $

;
( list "Report Wirelengths" "UIReportWirelengths" )


( UICreateMenuItemCallBack
  `UIReportWirelengthsCB
  (lambda ()
   (let (cv filename cellname cmd)
    cv=(geGetWindowCellView)
    cellname=cv->cellName
    filename=(sprintf nil "%s.wirelengths" cellname)

    (CheckLengthReportAll cv filename)
    cmd=(sprintf nil "sort -k5n,5 %s > %s.sorted" filename filename)
    (shell cmd)
   )
  ) 
)

