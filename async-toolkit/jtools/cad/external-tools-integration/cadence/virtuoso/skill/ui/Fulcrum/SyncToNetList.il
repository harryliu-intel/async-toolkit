; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


( list "Sync To NetList" "UISyncToNetList" )


( UICreateMenuItemCallBack 
  `UISyncToNetListCB
  (lambda () (SyncToNetlist ?CV (UIGetCellView)))
  )
