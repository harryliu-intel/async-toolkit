; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/ui/Nano/Prelayout/CreatePrelayout.il#5 $
; $DateTime: 2007/01/11 17:54:06 $
; $Author: khe $

;
; menu item Nano/Prelayout
;
( list "Canonicalize Pin Names" "UICanonicalizePins" )

( UICreateMenuItemCallBack
  `UICanonicalizePinsCB
  (lambda ()
    CanonicalizePins() 
  ) 
)


