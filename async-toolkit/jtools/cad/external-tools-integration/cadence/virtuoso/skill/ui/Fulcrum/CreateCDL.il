; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


( list "Create CDL" "UICreateCDL" )

( UICreateMenuItemCallBack 
  `UICreateCDLCB
  (lambda ()
    (let (
          ( CellView ( UIGetCellView ) ) )
      (let (
            ( TargetDFIIPath 
              ( NameGetCellDirFromDFIIDirAndCellName
                ( ConfigFileGetValue TheCDSConfigTable "TARGET_DFII_DIR" )
                ( getq CellView cellName ) ) ) )
        (let (
              ( CDLFile 
                ( sprintf nil "%s/%s.cdl" 
                          TargetDFIIPath
                          ( getq CellView cellName ) ) )
              ( ExecCmd (cond (
                               ( and ( boundp `ExecCmd ) ( stringp ExecCmd ) )
                               ExecCmd )
                              (
                               "" ) ) )
              ( CDLPath  ( ConfigFileGetValue TheCDSConfigTable "CDL_DIR" ) )
              )
          (when ( not ( isFile CDLFile ) )
            ( printf "%s doesn't exist...creating\n" CDLFile )
            ( when CDLPath==""
                   (setq CDLPath ( ConfigFileGetValue TheCDSConfigTable "TEMP" ))
            )
            ( setq CDLFile
                   ( sprintf 
                     nil
                     "%s/%s.cdl"
                     CDLPath
                     ( getq CellView cellName ) ) )
            (let (
                  ( Output
                    ( TempFileGetTempFileName
                      "cdltmp" )
                    )
                  ( Command
                    ( sprintf 
                      nil
                      "%s %s/cast2cdl --cast-path=%s --cell=%s --cadence-name --output=%s --max-heap-size=%dM --name-map=%s/share/Fulcrum/lve/transistor.map --process-dependent-name --64 --cdl-mos-parameters=m"
                      ExecCmd
                      ( PackageGetBinRoot )
                      ( ConfigFileGetValue TheCDSConfigTable "CAST_PATH" )
                      ( getq CellView cellName )
                      CDLFile
                      ( nrGetMaxHeapSize )
                      ( ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT" )
                      ) )
                  )
              (let (
                    ( DoneFunc
                      ( eval
                        ( ExpressionReplaceSymbolsWithValues
                          `(lambda ( Child Status )
                             (if ( equal Status 0 )
                                 ( printf "CDL file is %s\n" CDLFile )
                               ( printf  "cast2cdl failed on %s\n" CDLFile ) )
                             ( shell ( sprintf nil "cat %s; rm -f %s" Output Output ) ) )
                  ( list `Output `CDLFile ) ) ) ) )

            ( printf "%s\n" Command )
            ( ipcBeginProcess
              Command
              ""
              nil
              nil
              DoneFunc
              )
            ) )
          
          ) ) ) ) ) )
