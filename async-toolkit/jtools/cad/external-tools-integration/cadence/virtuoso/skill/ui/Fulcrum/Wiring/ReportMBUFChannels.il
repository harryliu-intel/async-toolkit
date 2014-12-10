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

