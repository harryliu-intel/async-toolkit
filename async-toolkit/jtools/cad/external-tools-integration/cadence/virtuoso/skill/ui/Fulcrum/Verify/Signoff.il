; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "Signoff" "UISignoff" )

( UICreateMenuItemCallBack
  `UISignoffCB
  (lambda ()
    (let (
          ( CellView ( UIGetCellView ) ) )
      (let (
            ( TargetSignoffFile
              ( sprintf 
                nil
                "%s/drc_signoff"
                ( NameGetCellPathFromCellNameAndView
                  CellView->cellName
                  CellView->viewName ) ) )
            ( SignoffFile
              ( ddGetStartup ( strcat CellView->cellName ".evd" ) ) )
            )

   ( system ( sprintf nil "touch %s" TargetSignoffFile ) )

  (if ( not SignoffFile )
      (error ( sprintf nil "signoff file %s doesn't exist.  Create it by running assura ui or avview" SignoffFile ) ) )
  (if ( equal ( ConfigFileGetValue TheCDSConfigTable "P4LOG" ) "" )
      (error "Must specify P4LOG in envirnoment or cds.config" ) )
  (if ( equal ( ConfigFileGetValue TheCDSConfigTable "P4HOST" ) "" )
      (error "Must specify P4HOST in envirnoment or cds.config" ) )
  (if ( and
        ( equal ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" ) "" )
        ( or
          ( equal ( ConfigFileGetValue TheCDSConfigTable "P4USER" ) "" )
          ( equal ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" ) "" ) ) )
      (error 
       "Must specify P4CONFIG or P4USER and P4CLIENT in envirnoment or cds.config" ) )
  (let (
        ( ErrorStr
          ( CDSP4RunCommandOnLibCellViewTripples
            ( sprintf nil "%s/%s \"--dfII-dir=%s\" --file=drc_signoff"
                      ( PackageGetBinRoot )
                      (cond (
                             ( isWritable TargetSignoffFile )
                             "cdsp4add" )
                            (
                             "cdsp4edit" ) )
                      ( NameGetDFIIDirFromCellName 
                        CellView->cellName ) )
            ( ConfigFileGetValue TheCDSConfigTable "P4HOST" )
            ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
            ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
            ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
            ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
            "default"
            ( list ( list 
                     CellView->libName
                     CellView->cellName
                     CellView->viewName ) )
            1
            ( ConfigFileGetValue TheCDSConfigTable "P4LOG" )
            ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
            t ) ) )
    (if ( stringp ErrorStr )
        ( println ErrorStr )
      (let (
            ( Append nil ) )
        
        ( hiDisplayAppDBox
          ?name `UISignoff_SaveDBox
          ?dboxBanner "Hello"
          ?dboxText "Choose Mode"
          ?buttons ( list "Overwrite" "Append" )
          ?callback ( list "Append = nil" "Append = t" )
          ?buttonLayout `UserDefined
          )

      ( system
        ( sprintf nil "%s/signoff %s %s %s"
                  ( PackageGetBinRoot )
                  SignoffFile 
                  (if Append ">>" ">" )
                  TargetSignoffFile ) )
 ) ) ) ) ) ) )



