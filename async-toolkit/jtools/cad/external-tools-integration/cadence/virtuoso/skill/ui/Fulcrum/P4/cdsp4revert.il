; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "cdsp4revert" "UIcdsp4revert" )

( UICreateMenuItemCallBack 
  `UIcdsp4revertCB
 (lambda ()
   (if ( and
         ( equal ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" ) "" )
         ( or
           ( equal ( ConfigFileGetValue TheCDSConfigTable "P4USER" ) "" )
           ( equal ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" ) "" ) ) )
       (error "Must specify P4CONFIG or P4USER and P4CLIENT in envirnoment or cds.config" ) )
   (let ( 
         ( ErrorStr ( CDSP4Revert
                      ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
                      ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
                      ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
                      ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
                      ( UIGetCellView )
                      ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) ) ) )
     (if ( stringp ErrorStr )
         ( println ErrorStr ) ) ) ) )
