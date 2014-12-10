; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/intel/cad/external-tools-integration/cadence/virtuoso/skill/ui/Fulcrum/SyncToNetList.il#1 $
; $DateTime: 2011/10/03 13:24:55 $
; $Author: aubrey $


( list "10. MakeAbstract" "UILeafAbstract" )


( UICreateMenuItemCallBack 
  `UILeafAbstractCB
  (lambda () 
    (MakeLeafAbstract)
    (View "abstract")
  )
)

