; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "cdsp4submit" "UIcdsp4submit" )

( UICreateMenuItemCallBack   
  `UIcdsp4submitCB  
  (lambda ()
    (if ( equal ( ConfigFileGetValue TheCDSConfigTable "P4LOG" ) "" )
        (error "Must specify P4LOG in envirnoment or cds.config" ) )
    (if ( equal ( ConfigFileGetValue TheCDSConfigTable "P4HOST" ) "" )
        (error "Must specify P4HOST in envirnoment or cds.config" ) )
    (if ( and
          ( equal ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" ) "" )
          ( or
            ( equal ( ConfigFileGetValue TheCDSConfigTable "P4USER" ) "" )
            ( equal ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" ) "" ) ) )
        (error "Must specify P4CONFIG or P4USER and P4CLIENT in envirnoment or cds.config" ) )
    ( hiDisplayAppDBox
      ?name `UIcdsp4submitDBox
      ?dboxText "Submit?"
      ?callback "( UIcdsp4submitOKCB )" )
 ) )

( UICreateAction
  `UIcdsp4submitOKCB
  (lambda ()
    (let ( 
          ( ErrorStr ( CDSP4Submit
                       ( ConfigFileGetValue TheCDSConfigTable "P4HOST" )
                       ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
                       ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
                       ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
                       ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
                       ( UIGetCellView )
                       ( ConfigFileGetValue TheCDSConfigTable "P4LOG" )
                       ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) ) ) )
      (if ( stringp ErrorStr )
          ( println ErrorStr ) ) ) ) )
