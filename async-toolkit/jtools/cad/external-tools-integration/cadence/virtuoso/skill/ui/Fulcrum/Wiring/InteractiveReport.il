; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id: $
; $DateTime: $
; $Author: $

;
( list "Interactive Report" "UICheckWireInteractive" )


( UICreateMenuItemCallBack
  `UICheckWireInteractiveCB
  (lambda ()
   (let (cv filein fileout cmd)
    cv=(geGetWindowCellView)
    filein=(sprintf nil "%s.wirelengths" cv->cellName)
    (CheckLengthReportForm (CheckLengthReadFullReport filein))
   )
  )
)
