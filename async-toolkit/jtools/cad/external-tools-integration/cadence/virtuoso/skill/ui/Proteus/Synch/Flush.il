; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/CreateCDL.il#11 $
; $DateTime: 2004/08/21 16:39:54 $
; $Author: clayton $


( list "1.Flush Design " "UIFlush" )


( UICreateMenuItemCallBack
  `UIFlushCB
  (lambda ()
    (FlushDesignFiles (geGetEditCellView))
  ) 
)
