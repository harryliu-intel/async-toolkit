; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Simple utility functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cdsp4 (command @key (CV (geGetEditCellView))
                      (client (ConfigFileGetValue TheCDSConfigTable "P4CLIENT")))
  (let (DFII_DIR P4CLIENT)
    DFII_DIR = (ConfigFileGetValue TheCDSConfigTable "DFII_DIR")
    (shell (sprintf nil "cdsp4%s --dfII-dir=%s --client-spec=%s --view-name=%s %s"
                    command DFII_DIR client CV->viewName CV->cellName))
    )
  )

(defun cdsp4add (@key (CV (geGetEditCellView))
                      (client (ConfigFileGetValue TheCDSConfigTable "P4CLIENT")))
  (cdsp4 "add" ?CV CV ?client client)
  )

(defun cdsp4edit (@key (CV (geGetEditCellView))
                       (client (ConfigFileGetValue TheCDSConfigTable "P4CLIENT")))
  (cdsp4 "edit" ?CV CV ?client client)
  (dbReopen CV "a") ; change the mode
  )

(defun cdsp4revert (@key (CV (geGetEditCellView))
                         (client (ConfigFileGetValue TheCDSConfigTable "P4CLIENT")))
  (dbSave CV)
  (dbReopen CV "r") ; change the mode
  (cdsp4 "revert" ?CV CV ?client client)
  (dbRefreshCellView CV)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Complex stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun CDSP4IPCWait ( Proc )
  ( ipcWait Proc ) )

(defun CDSP4IsCellViewWriteable ( LibName CellName ViewName )
  (let (
        ( ViewDDObj ( ddGetObj LibName CellName ViewName ) ) )
    (when ( getq ViewDDObj files )
      ( ddIsObjWritable ( car ( getq ViewDDObj files ) ) ) ) ) )

(defun CDSP4IsLibCellViewTrippleWriteable ( LibCellViewTripple )
  ( CDSP4IsCellViewWriteable 
    ( car LibCellViewTripple )
    ( cadr LibCellViewTripple )
    ( caddr LibCellViewTripple ) ) )

(defun CDSP4BinLibCellViewTripples ( LibCellViewTripples )
  (let (
        ( Ret ( makeTable "foo" nil ) ) )
    ( foreach
      Tripple
      LibCellViewTripples
      (let (
            ( ViewName ( caddr Tripple ) ) )
        ( setarray
          Ret
          ViewName
          ( cons
            Tripple
            ( arrayref Ret ViewName ) ) ) ) )
    Ret ) )

(defun CDSP4SplitList ( OriginalList SplitLength )
  (let (
        ( Ret nil )
        ( CurrList nil )
        ( Count 0 ) )
    ( foreach
      Item
      OriginalList
      (let ()
        (when ( equal Count SplitLength )
          ( setq
            Ret
            ( tconc Ret ( car CurrList ) ) )
          ( setq CurrList nil )
          ( setq Count 0 ) )
        ( setq CurrList ( tconc CurrList Item ) )
        ( setq Count ( plus Count 1 ) ) ) )
    (when CurrList
      ( setq Ret ( tconc Ret ( car CurrList ) ) ) )
    ( car Ret ) ) )
          

(defun CDSP4MakeCommandScript ( CommandStr TempDir )
  (let (
        ( CommandFileName ( makeTempFileName 
                            ( sprintf nil "%s/cdsp4command" TempDir ) ) ) )
    (let (
          ( CommandFile ( outfile CommandFileName "w" ) ) )
      ( fprintf CommandFile "#!/bin/bash\n%s\n" CommandStr )
      ( close CommandFile ) )
    ( shell
      ( sprintf nil "chmod u+x \"%s\"" CommandFileName ) )
    CommandFileName ) )


(defun CDSP4MakePerforceCommandScript ( P4CommandString
                                        P4User
                                        P4Passwd
                                        P4Client
                                        P4Config
                                        P4Changelist
                                        P4LogFileName
                                        TempDir
                                        CellNameList
                                        )
  (let (
        ( CellNameListString ( ListApplyFuncToListAndAccumulateResult
                               CellNameList
                               (lambda
                                 ( CellName CurrString )
                                 ( sprintf nil "%s \"%s\"" CurrString CellName ) )
                               nil
                               "" ) )
        ( CommandScriptFileName ( makeTempFileName 
                                  ( sprintf nil "%s/cdsp4cmd" TempDir ) ) ) )
    (let (
          ( CommandScriptPort ( outfile CommandScriptFileName "w" ) ) )
       ( fprintf CommandScriptPort "#!/bin/bash\n" )
       ( fprintf CommandScriptPort "P4CONFIG=%s\n" P4Config )
       ( fprintf CommandScriptPort "export P4CONFIG\n" )
       ( fprintf CommandScriptPort "cd %s\n" ( NameGetDFIIDirFromCellName ( car CellNameList ) ) )
       (let (
             ( CommandStringWithClientSpec
               ( sprintf
                 nil
                 "%s --change-list=%s "
                 P4CommandString                
                 P4Changelist ) ) )

         (when ( and P4User ( not ( equal P4User "" ) ) )
           ( setq CommandStringWithClientSpec 
                  ( sprintf nil "%s --perforce-user=%s"
                            CommandStringWithClientSpec 
                            P4User ) ) )
         (when ( and P4Client ( not ( equal P4Client "" ) ) )
           ( setq CommandStringWithClientSpec 
                  ( sprintf nil "%s --client-spec=%s"
                            CommandStringWithClientSpec 
                            P4Client ) ) )

         (when ( and P4Passwd ( not ( equal P4Passwd "" ) ) )
           ( setq CommandStringWithClientSpec 
                  ( sprintf nil "%s --perforce-password=%s"
                            CommandStringWithClientSpec 
                            P4Passwd ) ) )
     

         ( setq CommandStringWithClientSpec 
                ( sprintf 
                  nil
                  "%s %s 1>>%s 2>>%s\n"
                  CommandStringWithClientSpec
                  CellNameListString 
                  P4LogFileName
                  P4LogFileName
                  ) )
         
         (let (
               ( LogPort ( outfile P4LogFileName ) ) )
           ( fprintf LogPort
                     CommandStringWithClientSpec )
           ( close LogPort ) )

         ( fprintf CommandScriptPort
                   CommandStringWithClientSpec )
         ( close CommandScriptPort )
         ( shell ( sprintf nil "chmod u+x \"%s\"" CommandScriptFileName ) ) )
       CommandScriptFileName ) ) )

(defun CDSP4RunCommandOnLibCellViewTripples ( P4CommandString
                                              P4User
                                              P4Passwd
                                              P4Client
                                              P4Config
                                              P4Changelist
                                              LibCellViewTripples
                                              CellsPerCommand 
                                              LogFileName
                                              TempDir
                                              IncludeView
                                              )

  (let (
        ( CommandList nil )
        ( P4CommandList nil )
        ( LogFileList nil )
        ( TripplesTable ( CDSP4BinLibCellViewTripples
                          LibCellViewTripples ) ) )
    ( foreach
      ViewName
      TripplesTable
      (let (
            ( CurrTripples ( arrayref TripplesTable ViewName ) ) )
        (let (
              ( CellNames ( ListApplyFuncToListAndAccumulateResults
                            CurrTripples
                            (lambda
                              ( Tripple )
                              ( cadr Tripple ) )
                            nil ) ) )
          (let (
                ( CellNameLists ( CDSP4SplitList
                                  CellNames
                                  CellsPerCommand ) ) )
            ( foreach
              CellNameList
              CellNameLists
              (let (
                    ( CellNameListString ( ListApplyFuncToListAndAccumulateResult
                                           CellNameList
                                           (lambda
                                             ( CellName CurrString )
                                             ( sprintf nil "%s \"%s\"" CurrString CellName ) )
                                           nil
                                           "" ) ) )
                (let (
                      ( CurrLogFileName ( makeTempFileName 
                                          ( sprintf nil "%s/cdsp4output" TempDir ) ) ) )
                  (let (
                        ( LogOutput ( outfile CurrLogFileName "w" ) ) )
                    ( close LogOutput ) )
                  ( setq LogFileList ( cons CurrLogFileName LogFileList ) )
                  (let (
                        ( P4CommandScript ( CDSP4MakePerforceCommandScript
                                            (if IncludeView
                                                ( sprintf nil "%s  --view-name=%s"
                                                          P4CommandString
                                                          ViewName )
                                              P4CommandString )
                                            P4User
                                            P4Passwd
                                            P4Client
                                            P4Config
                                            P4Changelist
                                            CurrLogFileName
                                            TempDir
                                            CellNameList
                                            ) ) )
                    ( setq P4CommandList ( cons P4CommandScript P4CommandList ) )
                    (let (
                          ( Command
                            ( sprintf
                              nil
                              "/p/rrc/tools/bin/fulcrum %s"
                              P4CommandScript ) ) )
                      
                      (let (
                            ( CommandFile ( CDSP4MakeCommandScript
                                            Command
                                            TempDir ) ) )
                        ( setq 
                          CommandList 
                          ( cons CommandFile CommandList ) )
                        ) ) ) ) ) ) ) ) ) )
    (let (
          ( ProcIdList ( ListApplyFuncToListAndAccumulateResults
                         CommandList
                         (lambda
                           ( CurrCommand )
                           ( ipcBatchProcess
                             CurrCommand
                             ""
                             "/dev/null" ) )
                         nil ) ) )
      ( foreach
        ProcId
        ProcIdList
        ( CDSP4IPCWait ProcId ) ) )

    (let (
          ( LogPort ( outfile LogFileName "a" ) ) )
      ( foreach
        LogFile
        LogFileList
        (let ( )
          ( FileUtilAppendFile LogPort LogFile ) 
          ( deleteFile LogFile ) ) )
      ( close LogPort ) )
    
    ( foreach
      Command
      CommandList
      ( deleteFile Command ) )
    ( foreach
      Command
      P4CommandList
      ( deleteFile Command ) ) ) 
  nil )
   


;Returns list of cells that could not be editted.
(defun CDSP4EditLibCellViewTripples ( P4User 
                                      P4Passwd
                                      P4Client
                                      P4Config
                                      P4Changelist
                                      LibCellViewTripples
                                      CellsPerCommand 
                                      LogFileName
                                      TempDir )
  ( CDSP4RunCommandOnLibCellViewTripples
    ( sprintf nil "%s/cdsp4edit \"--dfII-dir=%s\""
              ( PackageGetBinRoot )
              ( NameGetDFIIDirFromCellName ( cadr ( car LibCellViewTripples ) ) ) )
    P4User
    P4Passwd
    P4Client
    P4Config
    P4Changelist
    LibCellViewTripples
    CellsPerCommand 
    LogFileName
    TempDir 
    t )
  
  (let (
        ( Ret nil ) )
    ( foreach
      LibCellViewTripple
      LibCellViewTripples
      (unless ( CDSP4IsLibCellViewTrippleWriteable
                LibCellViewTripple )
        ( setq
          Ret
          ( tconc Ret LibCellViewTripple ) ) ) )
    ( car Ret ) ) )

(defun CDSP4MakeEditErrorString ( EditRet )
  (let (
        ( ErrorStr nil ) )
    ( foreach
      Tripple
      EditRet
      (let ( 
            ( CurrErrorStr ( sprintf 
                             nil 
                             "Unable to edit %L %L %L.\n"
                             ( car Tripple )
                             ( cadr Tripple )
                             ( caddr Tripple ) ) ) )
        ( setq
          ErrorStr
          (if ErrorStr
              ( strcat ErrorStr CurrErrorStr )
            CurrErrorStr ) ) ) )
    ErrorStr ) )

(defun CDSP4AddLibCellViewTripples ( P4User 
                                     P4Passwd
                                     P4Client
                                     P4Config
                                     P4Changelist
                                     LibCellViewTripples
                                     CellsPerCommand 
                                     LogFileName
                                     TempDir )
  ( CDSP4RunCommandOnLibCellViewTripples
    ( sprintf nil "%s/cdsp4add \"--dfII-dir=%s\""
              ( PackageGetBinRoot )
              ( NameGetDFIIDirFromCellName ( cadr ( car LibCellViewTripples ) ) ) )
    P4User
    P4Passwd
    P4Client
    P4Config
    P4Changelist
    LibCellViewTripples
    CellsPerCommand 
    LogFileName 
    TempDir
    t ) )

(defun CDSP4RevertLibCellViewTripples ( P4User 
                                        P4Passwd
                                        P4Client
                                        P4Config
                                        P4Changelist
                                        LibCellViewTripples
                                        CellsPerCommand 
                                        LogFileName
                                        TempDir
                                         )
  ( CDSP4RunCommandOnLibCellViewTripples
    ( sprintf nil "%s/cdsp4revert \"--dfII-dir=%s\""
              ( PackageGetBinRoot )
              ( NameGetDFIIDirFromCellName ( cadr ( car LibCellViewTripples ) ) ) )
    P4User 
    P4Passwd
    P4Client
    P4Config
    P4Changelist
    LibCellViewTripples
    CellsPerCommand 
    LogFileName 
    TempDir
    t ) )

(defun CDSP4Edit ( P4User P4Passwd P4Client P4Config CellView TempDir )
  (let (
        ( EditRet ( CDSP4EditLibCellViewTripples
                    P4User
                    P4Passwd
                    P4Client
                    P4Config
                    "default"
                    ( list
                      ( list
                        ( getq CellView libName )
                        ( getq CellView cellName )
                        ( getq CellView viewName ) ) )
                    1
                    ( strcat TempDir "/p4.log" )
                    TempDir ) ) )
    (if EditRet
        (error ( CDSP4MakeEditErrorString EditRet ) )
      ( dbReopen CellView "a" ) ) ) )

(defun CDSP4Add ( P4User P4Passwd P4Client P4Config CellView TempDir )
  ( CDSP4AddLibCellViewTripples
    P4User
    P4Passwd
    P4Client
    P4Config
    "default"
    ( list
      ( list
        ( getq CellView libName )
        ( getq CellView cellName )
        ( getq CellView viewName ) ) )
    1
    ( strcat TempDir "/p4.log" )
    TempDir ) )

(defun CDSP4Revert ( P4User P4Passwd P4Client P4Config CellView TempDir )
  ( CDSP4RevertLibCellViewTripples
    P4User
    P4Passwd
    P4Client
    P4Config
    "default"
    ( list
      ( list
        ( getq CellView libName )
        ( getq CellView cellName )
        ( getq CellView viewName ) ) )
    1
    ( strcat TempDir "/p4.log" )
    TempDir )
  ( dbReopen CellView "r" ) )


(defun CDSP4CellView (cv action)
        (when action=="add"
          ( CDSP4Add
                 ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
                 cv
                 ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) 
               )
        )
        (when action=="edit"
          ( CDSP4Edit
                 ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
                 cv
                 ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) 
               )
        )
        (when action=="revert"
          ( CDSP4Revert
                 ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
                 cv
                 ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) 
               )
        )
   cv
)

(defun CDSP4CellFile (cv filename action)
 (let (Command)
    Command = ( sprintf nil "%s/cdsp4%s \"--dfII-dir=%s\" \"--client-spec=%s\" \"--file=%s\" %s"
                ( PackageGetBinRoot )
                action
                ( ConfigFileGetValue TheCDSConfigTable "DFII_DIR" )
                ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
                filename
                cv->cellName
     )
    ( shell Command )
 )
)

