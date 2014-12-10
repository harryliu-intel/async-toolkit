; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun RouterImport ( DestLibName DestCellName DestViewName WorkingDir )
  (let (        
        ( ImportCommand
          ( sprintf nil "%s/importccar" ( PackageGetBinRoot ) ) )
        ( ImportLog ( sprintf nil
                              "%s/import.log"
                              WorkingDir
                              ) )
        )   
    ( setq ImportCommand
           ( sprintf 
             nil 
             "%s --dest-lib=%s --dest-cell=%s --dest-view=%s --working-dir=%s"
             ImportCommand
             DestLibName
             DestCellName
             DestViewName
             WorkingDir ) )
    ( println ImportCommand )
    ( printf "Log in %s\n" ImportLog )
    ( ipcWait ( ipcBatchProcess ImportCommand "" ImportLog ) )
    ) )

(defun RouterUseInstantiatorView ( CellView
                                   InstantiatorView 
                                   FoldableCellLibCellPairRegExs
                                   SuperStackLibCellPairRegExs
                                 )
  (let (
        ( InstantiatorInst
          (cond (
                 ( dbFindAnyInstByName
                   CellView
                   "theInstantiators" ) )
                (
                 ( dbCreateInst
                   CellView
                   InstantiatorView
                   "theInstantiators"
                   0:0
                   "R0" ) ) ) ) )

        ;inline the instantator cell in the result
        ;this will copy conductor shapes but not pins
        (when InstantiatorInst 
          ( InlineInstances
            CellView
            CellView
            ( list InstantiatorInst )
            "layout"
            nil
            FoldableCellLibCellPairRegExs
            SuperStackLibCellPairRegExs
            ?CopyPins nil
            ?Verbose nil ) )
        )
  )

(defun RouterRunRouter ( SrcLibName SrcCellName SrcViewName
                         DestLibName DestCellName DestViewName
                         ConductorDepth
                         KeepoutDepth
                         RuleFile
                         DoFiles
                         WorkingDir
                         PowerNetNames
                         @key 
                         ( FulcrumPDKRoot "" )
                         ( PinConnect "strong" )
                         ( Quit t )
                         ( Import t )
                         ( Blocking t )   
                         ( Graphics t )
                         ( ExportPCells t )
                         ( Area nil )
                         ( UseConstraints t )
                         )
  (let (
        ( TempDirForThisRun 
          (if Blocking 
              ( makeTempFileName 
                ( sprintf 
                  nil
                  "%s/CCAR.XXXXXX" 
                  WorkingDir ) )
            WorkingDir ) )
        ( DFIIDir 
          ( NameGetDFIIDirFromLibName "gate" ) )
        ( CellView
          ( dbOpenCellViewByType
            SrcLibName 
            SrcCellName
            SrcViewName
            "maskLayout"
            "a" ) )
        ( DestCellViewObj 
          ( ddGetObj 
            DestLibName
            DestCellName
            DestViewName ) )
        )

    (when ( and Import DestCellViewObj )
      ( printf "WARNING...overwriting dest cell view\n" )
      ( ddDeleteObj DestCellViewObj ) )

    (let (
          ( ConstraintsDoFile 
            ( sprintf nil "%s/FulcrumConstraints.do" TempDirForThisRun ) )
          ( CCARLog
            ( sprintf nil
                      "%s/ccar.log"
                      TempDirForThisRun
                      ) )
          ( CCARRunLog 
            ( sprintf nil "%s/runccar.log" TempDirForThisRun ) )
          ( CCARDebugLog
            ( sprintf nil "%s/runccar.debug" TempDirForThisRun ) )
          ( CCARCommand
            ( sprintf nil "%s/runccar" ( PackageGetBinRoot ) ) ) )

      ( createDir TempDirForThisRun )

      (when ( not
              ( isReadable RuleFile ) )
        ( printf "Can't read rule file %s\n" RuleFile ) )

      ( ConstraintsDumpICCToFile
        CellView
        ConstraintsDoFile
        ICCUnitsPerMeter
        PowerNetNames
      )
      
      ( setq 
        CCARCommand 
        ( sprintf nil "%s --src-lib=%s --src-cell=%s --src-view=%s --dest-lib=%s --dest-cell=%s --dest-view=%s --conductor-depth=%d --keepout-depth=%d --pin-connect=%s --rule-file=%s --working-dir=%s --fulcrum-pdk-root=%s --dfII-dir=%s --log-file=%s --debug-log-file=%s"
                  CCARCommand
                  SrcLibName SrcCellName SrcViewName
                  DestLibName DestCellName DestViewName 
                  ConductorDepth KeepoutDepth PinConnect
                  RuleFile TempDirForThisRun FulcrumPDKRoot DFIIDir
                  CCARRunLog CCARDebugLog
                  ) )


      (when UseConstraints
        ( setq DoFiles ( cons ConstraintsDoFile DoFiles ) ) )

      ( foreach DoFile DoFiles
                (cond (
                       ( isReadable DoFile )
                       ( setq CCARCommand ( sprintf nil "%s --do-file=%s" 
                                                    CCARCommand
                                                    DoFile ) ) )
                      (
                       ( printf "Can't read do file %s...ignoring\n" DoFile ) ) ) )

      (when ( not Graphics )
        ( setq CCARCommand 
               ( sprintf nil "%s --nog" CCARCommand ) ) )
      (when Quit
        ( setq CCARCommand 
               ( sprintf nil "%s --quit " CCARCommand ) ) )
      (when Import
        ( setq CCARCommand 
               ( sprintf nil "%s --import " CCARCommand ) ) )

      (when ( null ExportPCells )
        ( setq CCARCommand 
               ( sprintf nil "%s --no-export-pcells " CCARCommand ) ) )

      (when Area
        ( setq CCARCommand
               ( sprintf nil "%s --area=%f,%f,%f,%f "
                         CCARCommand
                         ( caar Area )
                         ( cadar Area )
                         ( caadr Area )
                         ( cadadr Area )
                         ) ) )
      
      ( printf "%s\n" CCARCommand )

      ( LicenseGetLicense "Virtuoso_Layout_Suite_GXL" 11.0 8 )
      ( LicenseReturnLicense "Virtuoso_Layout_Suite_GXL" )
      
      (let (
            ( Crossing -1 )
            ( Clearance -1 )
            ( Unroutes -1 )
            ( Completion -1.0 )
            ( InPort nil ) )
        (cond (
               Blocking
               ( shell CCARCommand )
               )
              (
               t
               ( printf "Log in %s\n" CCARLog )
               ( ipcBatchProcess CCARCommand "" CCARLog ) ) )
        (let (
              ( InPort ( infile CCARRunLog ) ) )
          (when InPort 
            ( fscanf InPort "%d %d %d %f" Crossing Clearance Unroutes Completion )
            ( close InPort ) )
          )
        (when Blocking
          t
          ;( shell ( sprintf nil "rm -rf %s &>/dev/null" TempDirForThisRun ) )
          )

        ( list Crossing Clearance Unroutes Completion ) ) ) ) )



(defun RouteLeaf ()
    (let ( CellView WorkingDir TclFile)

        CellView = UIGetCellView( )
        SetHandLayoutAllObjects( )
        WorkingDir = sprintf( nil
                            "%s/%s" 
                            ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
                            CellView->cellName )
        createDir( WorkingDir )

        AddLeafBlockageCell( CellView )
        TclFile=strcat(WorkingDir "/router_temp.tcl")
;        PDKRoot = (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")

        mycmd=sprintf( nil "/usr/bin/sed -e \"s/LIBNAME/%s/g\" -e \"s/CELLNAME/%s/g\" -e \"s/VIEWNAME/%s/g\" -e \"s^EXCLUDENET^%s^g\" <%s >%s\n" CellView->libName CellView->cellName CellView->viewName UIRoute_Form->UIRoute_Form_ExcludeNet->value  UIRoute_Form->UIRoute_Form_VsrTcl->value TclFile ) 
        println(mycmd)
        system(mycmd)
        deInstallApp(getCurrentWindow() "Virtuoso XL")
;        ipcSleep(30)

        FixMustConnectTerm( CellView )
        ; move all fig in XL to add connectivity to via/path shapes
        geSelectAllFig( CellView)
        MoveAndSnap_MoveSelected( 1 1 )
        MoveAndSnap_MoveSelected( -1 -1 )
        geDeselectAll()
        RoutingGood = rdeSource(TclFile)
        DeleteLeafBlockageCell( CellView )
    )
)
