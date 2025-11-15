; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "cdsp4add" "UIcdsp4add" )

( UICreateMenuItemCallBack 
  `UIcdsp4addCB
  (lambda ()
    (if ( and
          ( equal ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" ) "" )
          ( or
            ( equal ( ConfigFileGetValue TheCDSConfigTable "P4USER" ) "" )
            ( equal ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" ) "" ) ) )
        (error "Must specify P4CONFIG or P4USER and P4CLIENT in envirnoment or cds.config" ) )    
    (let ( 
          ( ErrorStr ( CDSP4Add
                           ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
                           ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
                           ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
                           ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
                           ( UIGetCellView )
                           ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) ) ) )
      (if ( stringp ErrorStr )
          ( println ErrorStr ) ) ) ) )
