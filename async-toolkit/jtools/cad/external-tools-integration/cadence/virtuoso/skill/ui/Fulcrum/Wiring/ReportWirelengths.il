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

