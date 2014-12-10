; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id: $
; $DateTime: $
; $Author: $

;
( list "Check BusWires" "UICheckBusWires" )


( UICreateMenuItemCallBack
  `UICheckBusWiresCB
  (lambda ()
   (let (cv filein fileout cmd)
    cv=(geGetWindowCellView)
    filein=(sprintf nil "%s.wirelengths.sorted" cv->cellName)
    fileout=(sprintf nil "%s.wirelengths.buswires" cv->cellName)
    (CompareBusWireLengthsToManhattan filein fileout) 

    cmd=(sprintf nil "sort -k2n,2 %s > %s.sorted" fileout fileout)
    (shell cmd)

   )
  ) 
)

