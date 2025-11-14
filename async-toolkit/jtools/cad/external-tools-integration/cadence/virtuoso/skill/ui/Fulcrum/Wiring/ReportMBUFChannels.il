; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id: $
; $DateTime: $
; $Author: $

;
( list "Report MBUF Channels" "UIReportMBUFChannels" )


( UICreateMenuItemCallBack
  `UIReportMBUFChannelsCB
  (lambda ()
   (let (cv filename)
    cv=(geGetWindowCellView)
    filename=(sprintf nil "%s.bundled_channels" cv->cellName)
    (ReportMBUFChannels filename ?filter_buswires t) 
   )
  ) 
)

