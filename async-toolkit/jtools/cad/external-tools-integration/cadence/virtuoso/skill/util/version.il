; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun VersionGetMajorVersion ()
  ( substring ( nth 2 ( parseString ( getVersion ) " " )  ) 1 1 ) )
