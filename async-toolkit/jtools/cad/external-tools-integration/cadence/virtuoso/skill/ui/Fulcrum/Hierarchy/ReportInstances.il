; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id: $
; $DateTime: $
; $Author: $


( list "Report Instances" "UIReportInstances" )


( UICreateMenuItemCallBack
  `UIReportInstancesCB
  (lambda ()
    (ReportInstances) 
  ) 
)

