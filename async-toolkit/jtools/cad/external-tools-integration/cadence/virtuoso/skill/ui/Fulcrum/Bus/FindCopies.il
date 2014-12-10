; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id: $
; $DateTime: $
; $Author: $

;
; menu item Fulcrum/Bus
;
; reference function from layout/bus/bus.il
;
( list "Find Copied Guidepaths" "UIFindCopies" )


( UICreateMenuItemCallBack
  `UIFindCopiesCB
  (lambda ()
    copies=(FindDuplicateBuswires ?check_points nil)
    (when copies
      (UIPopUpDialog "Paths found which are possibly copies.")
      (foreach path copies
        (printf "Pattern: %s Prefix: %s\n" (GetProp path "BusPattern" nil) (GetProp path "BusPrefix" nil) )
      )
    )
  ) 
)

