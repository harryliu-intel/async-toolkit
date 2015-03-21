; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; grunt work to run Assura to draw some generated shapes in the current cell
(defun SimpleAssuraLayerProcessor (AssuraLayerMappings rulName @key (CV (geGetEditCellView)))
  (let (TEMP AssuraRuleFile AssuraRunLog assuraCV)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    AssuraRuleFile = (sprintf nil "%s/share/Fulcrum/cell_automation/%s.rul"
                              (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT") rulName)
    AssuraRunLog = (sprintf nil "%s/%s.%s.log" TEMP CV->cellName rulName)
    (AssuraRunAssuraLayerProcessor
     CV CV->libName CV->cellName rulName
     AssuraRuleFile TEMP AssuraLayerMappings nil
     ?AssuraRunLog AssuraRunLog
     ?LeaveMess nil
     )
    assuraCV = (dbOpenCellViewByType CV->libName CV->cellName rulName nil "r")
    (unless assuraCV (error "Unable to open %s %s %s\n" CV->libName CV->cellName rulName))
    (foreach fig assuraCV->shapes (dbCopyFig fig CV))
    (ddDeleteObj (ddGetObj CV->libName CV->cellName rulName nil nil "r"))
    )
  CV
  )

(defun AssuraGetAreas ( CellView 
                        LPP
                        LeafCellPredicate )
  "Calculates
   <dl>
   <dt> LeafArea </dt> <dd> The area of the union of all leaf cell bounding boxes </dd>
   <dt> MergeArea </dt> <dd> The area of the bloat/shrunk leaf cell bounding boxes. </dd>
   <dt> MidBoundArea  </dt> <dd> The area of the top level prBoundary. </dd>
   <dt> SubCellArea </dt> <dd> The area of the union of all subcell bounding boxes </dd>
   </dl>
  Uses <package vfe> with <a href=$src/cad/external-tools-integration/cadence/virtuoso/skill/layout/assura/area.rul>area.rul</a>.
  "

  (let (
        ( TechFileDBObj ( techGetTechFile CellView ) ) )
    (let (
          ( TempDirForThisRun
            ( makeTempFileName
              ( sprintf
                nil
                "%sASSURA.XXXXXX"
                ( ddGetStartup "" )
                ) ) )
          )

      ( createDir TempDirForThisRun )

      (let (
            ( TempLibrary
              ( LibCreateTempLibrary
                TechFileDBObj
                "lib" 
                TempDirForThisRun
                ) )
            ( Rects
              ( MidLevelGetRects
                CellView 
                LPP
                LeafCellPredicate ) )
            ( BoundShape
              ( PinUtilFindPRBoundShape
                LPP
                CellView ) )
            ( RunAssuraScript
              ( sprintf
                nil
                "%s/share/script/perl/ve/front-end/vfe"
                ( PackageGetPackageRoot ) ) )
            ( CdsLibFile ( ddGetStartup "cds.lib" ) )
            )
        (let (
              ( TempCellView
                ( dbOpenCellViewByType
                  TempLibrary
                  ( getq CellView cellName )
                  "area_calc" 
                  "maskLayout"
                  "w" ) ) )

    ;leaf cells
    ( foreach Rect Rects
              ( dbCreateRect TempCellView ( list "prBoundary" "drawing" )
                             Rect ) )
    ;subcells
    ( foreach Rect ( getq CellView instances )~>bBox
              ( dbCreateRect TempCellView ( list "prBoundary" "label" )
                             Rect ) )
    ;boundary
    (when BoundShape ( dbCopyFig BoundShape TempCellView )->lpp =
          ( list "prBoundary" "boundary" )
          )

    ( dbSave TempCellView )

    (let (
          ( AssuraRuleFile ( sprintf nil "%s/share/skill/layout/assura/area.rul" ( PackageGetPackageRoot ) ) )
          ( AssuraRunCmd ( sprintf nil "%s/assura.cmd" TempDirForThisRun ) )
          ( AssuraRunLog ( sprintf nil "%s/assura.log" TempDirForThisRun ) )
          ( Netlist ( sprintf nil "%s/netlist" TempDirForThisRun ) )
          ( AreaLog ( sprintf nil "%s/area" TempDirForThisRun ) )
          )
      (let (
            ( AssuraRunCmdOut
              ( outfile AssuraRunCmd ) ) )

        ( fprintf AssuraRunCmdOut "#!/bin/bash\n" )
        ( fprintf
          AssuraRunCmdOut
          "%s VerificationType=AssuraLvs \"RunName=%s\" \"CdsLib=%s\"  \"Layout\
Cell=%s\" \"LayoutLibrary=%s\" \"LayoutView=%s\" \"RuleFile=%s\" \"WorkingDir=%s\""
          RunAssuraScript
          ( getq TempCellView cellName )
          CdsLibFile
          ( getq TempCellView cellName )
          ( getq TempCellView libName )
          ( getq TempCellView viewName )
          AssuraRuleFile
          TempDirForThisRun
          )

        ( close AssuraRunCmdOut )
        ( shell ( sprintf nil "chmod u+x %s" AssuraRunCmd ) )
        ( system ( sprintf
                   nil
                   "%s&>%s"
                   AssuraRunCmd
                   AssuraRunLog ) )

        (let ( 
              ( LeafArea 0.0 )
              ( MidBoundArea 0.0 )
              ( MergeArea 0.0 )
              ( SubCellArea 0.0 )
              ( Area 0.0 )
              ( Cmd
                ( sprintf
                  nil
                  "vldbToSpice %s/%s.ldb %s; cat %s"
                  TempDirForThisRun
                  ( getq TempCellView cellName )
                  Netlist
                  Netlist
                  ) )
              ( ReadLine t )
              )
          (let (
                ( CID
                  ( ipcBeginProcess Cmd ) ) )
            (ipcWaitForProcess CID)
            (unless ( ipcIsAliveProcess CID )
              (error "assura failed" ) )
            ( rexCompile ".* \\(.*\\) area=\\(.*\\)" )
            (while ReadLine
              ReadLine = ( StringUtilReadLine CID 10 )
              (when ReadLine
                ( rexExecute ReadLine )
                ( println ( rexSubstitute "\\1" ) )
                (cond (
                       ( equal ( rexSubstitute "\\1" ) "leaf" )
                       LeafArea = LeafArea + 
                       ( atof ( rexSubstitute "\\2" ) ) )
                      (
                       ( equal ( rexSubstitute "\\1" ) "mid_bound" )
                       MidBoundArea = MidBoundArea +
                       ( atof ( rexSubstitute "\\2" ) ) )
                      (
                       ( equal ( rexSubstitute "\\1" ) "subcell" )
                       SubCellArea = SubCellArea + 
                       ( atof ( rexSubstitute "\\2" ) ) )
                      (
                       ( equal ( rexSubstitute "\\1" ) "merge" )
                       MergeArea = MergeArea +
                       ( atof ( rexSubstitute "\\2" ) ) ) ) ) )
;            ( shell ( sprintf nil "rm -rf %s" TempDirForThisRun ) )
            
            ( list LeafArea MergeArea MidBoundArea SubCellArea )
            ) ) ) ) ) ) ) ) )

        


(defun AssuraCopyShapesFromCellView ( SourceCellView
                                      TargetCellView
                                      ShapePredicate )
  "Copies all shapes in SourceCellView that satisfy ShapePredicate to TargetCellView"
  ( foreach Shape ( getq SourceCellView shapes )
            (if ( or ( null ShapePredicate )
                     ( apply ShapePredicate ( list Shape ) ) )
                ( dbCopyFig Shape TargetCellView ) ) ) )



(defun AssuraWriteOutFileRSF ( RSFFile 
                               LibName
                               WorkingDir
                               TechFile
                               Mode
                               AssuraLayerMappings )
  "Writes an .rsf include file, which instructs assura to write an output database in WorkingDir using the TechFile filename, with assura-dfII layer mappings defined as in AssuraLayerMappings(see <skill AssuraMakeOutLayersStringFromLayerMappings> for a description of AssuraLayerMappings)."
  (let (
        ( OutPort ( outfile RSFFile ) ) ) 
    
    ( fprintf 
      OutPort
      "avParameters( ?outputErrorLib modifyExistingLibrary ?flagMalformed nil ?ignoreMissingOutLayer t )\n" )
    ( fprintf 
      OutPort
      "outFile(\"DF2\" \"%s\" \"%s\" \"%s\" %s %s )\n"
      LibName
       WorkingDir
       TechFile
       Mode
      ( AssuraMakeOutLayersStringFromLayerMappings
        AssuraLayerMappings ) )
    
    ( close OutPort )
    ) )


(defun AssuraMakeOutLayersStringFromLayerMappings ( AssuraLayerMappings )
  "Given a list of pairs ((assuraLayer LPP) ...), creates an outLayer string for  use in an .rsf file.  This string tells assura how to map between internal layers and dfII/gdsII layers in the outfile database." 
  (let (
        ( Result "" ) )
    ( foreach
      AssuraLayerMapping
      AssuraLayerMappings
      (let (
            ( AssuraLayerName ( car AssuraLayerMapping ) )
            ( TargetLPP ( cadr AssuraLayerMapping ) ) )
        (let (
              ( TargetLayerName ( car TargetLPP ) )
              ( TargetLayerPurpose ( cadr TargetLPP ) ) )
          ( setq Result ( sprintf 
                          nil 
                          "outLayer( %s \"%s\" type( \"%s\" ) ) %s" 
                           AssuraLayerName
                          TargetLayerName
                          TargetLayerPurpose
                          Result ) ) ) ) )
    Result ) )


(defun AssuraPrepareCellForProcessing ( CellView TempLibName CellMappings )
  "Copies over all subcells of Cellview to temporary library called TempLibName according to CellMappings: a list of pairs (((srclibname srccellname srcviewname)->destcellname)...), and updates the instances in CellView accordingly.<br>  This is so assura can reference these cells in the output database using outfile.  See <skill AssuraFixGeneratedInstances> for the reverse process."
  (let (
        ( CellViewCopy ( dbCopyCellView
                         CellView
                         TempLibName
                         ( getq CellView cellName )
                         ( getq CellView viewName )
                         nil
                         nil
                         t ) ) )
    ( dbSave CellViewCopy )
    )
                         
  
  ( foreach
    CellMapping
    CellMappings
    (let (
          ( SrcCellTripple ( car CellMapping ) )
          ( TargetCellName ( cadr CellMapping ) ) )
      (let (
            ( SrcLibName ( car SrcCellTripple ) )
            ( SrcCellName ( cadr SrcCellTripple ) )
            ( SrcViewName ( caddr SrcCellTripple ) ) )
        (let (
              ( SrcCellViewDDObj ( ddGetObj SrcLibName SrcCellName SrcViewName ) ) )
          (if ( and SrcCellViewDDObj ( getq SrcCellViewDDObj files ) )
              (let (
                    ( CurrSrcCellView ( dbOpenCellViewByType
                                        SrcLibName
                                        SrcCellName
                                        SrcViewName
                                        nil
                                        "r" ) ) )
                (let (
                      ( CellViewCopy
                        ( dbCopyCellView
                          CurrSrcCellView
                          TempLibName
                          TargetCellName
                          "layout"
                          nil
                          nil
                          t ) ) )
                  (if CellViewCopy
                      (let ()
                        ( dbSave CellViewCopy )
                        ( dbClose CellViewCopy )
                        )
                    ( printf
                      "Could not copy %L %L %L to %L %L %L.\n"
                      SrcLibName
                      SrcCellName
                      SrcViewName
                      TempLibName
                      TargetCellName
                      "layout" ) ) ) )
            ( printf "%L %L %L does not exist.\n" SrcLibName SrcCellName SrcViewName ) ) ) ) ) )
  nil )


(defun AssuraFixGeneratedInstances ( CellView
                                     TempLibName
                                     CellMappings )
  "Changes instantiations in CellView according to the inverse of CellMappings.  This will reverse <skill AssuraPrepareCellForProcessing>.  Apply this to the cellview that assura writes with the outfile function."
  (let (
        ( MappingsTable ( makeTable "foo" nil ) ) )
    ( foreach
      CellMapping
      CellMappings
      (let (
            ( SrcCellTripple ( car CellMapping ) )
            ( TargetCellName ( cadr CellMapping ) ) )
        (let (
              ( SrcLibName ( car SrcCellTripple ) )
              ( SrcCellName ( cadr SrcCellTripple ) )
              ( SrcViewName ( caddr SrcCellTripple ) ) )
          (let (
                ( SrcCellViewDDObj ( ddGetObj SrcLibName SrcCellName SrcViewName ) ) )
            (when ( and SrcCellViewDDObj ( getq SrcCellViewDDObj files ) )
              (let (
                    ( SrcCellView ( dbOpenCellViewByType
                                    SrcLibName
                                    SrcCellName
                                    SrcViewName
                                    nil
                                    "r" ) ) )
                ( setarray
                  MappingsTable
                  ( list TempLibName TargetCellName "layout" ) 
                  SrcCellView ) ) ) ) ) ) )
    ( foreach
      Instance
      ( getq CellView instances )
      (let (
            ( NewMaster ( arrayref 
                          MappingsTable
                          ( list
                            ( getq Instance libName )
                            ( getq Instance cellName )
                            "layout" ) ) ) )
        (when NewMaster
          ( dbSetq Instance NewMaster master ) ) ) ) ) )

(defun AssuraRunDRC ( CellView                               
                      AssuraRuleFile
                      AssuraSets
                      ScratchDirectory ;path must be < 255 characters
                      OutputDirectory
                      )
  "Run's assura DRC with the AssuraRuleFile on CellView.  Uses the <a href=$toolDoc#vfe>vfe</a>.  Returns t if DRC passes, and nil otherwise."
  (let (        
        ( RunAssuraScript 
          ( sprintf 
            nil
            "%s/share/script/perl/ve/front-end/vfe"
            ( PackageGetPackageRoot ) ) )
        ( CdsLibFile ( ddGetStartup "cds.lib" ) )
        ( TempDirForThisRun 
          ( makeTempFileName 
            ( sprintf 
              nil
              "%s/DRC.XXXXXX" 
              ScratchDirectory ) ) ) )
    ( createDir TempDirForThisRun )
    (let (
          ( AssuraRunCmd ( sprintf nil "%s/assura.cmd" TempDirForThisRun ) )
          ( AssuraRunLog ( sprintf nil "%s/assura.log" TempDirForThisRun ) )
          )
      (let (
            ( AssuraRunCmdOut
              ( outfile AssuraRunCmd ) ) )
      
        ( fprintf AssuraRunCmdOut "#!/bin/bash\n" )        
        ( fprintf 
          AssuraRunCmdOut
          "%s VerificationType=AssuraDrc \"RunName=%s\" \"CdsLib=%s\"  \"LayoutCell=%s\" \"LayoutLibrary=%s\" \"LayoutView=%s\" \"RuleFile=%s\" \"WorkingDir=%s\" %s"
          RunAssuraScript 
          ( getq CellView cellName )
          CdsLibFile
          ( getq CellView cellName )
          ( getq CellView libName )
          ( getq CellView viewName )
          AssuraRuleFile
          TempDirForThisRun
          ( StringUtilPrefix
            "AssuraSet.="
            AssuraSets ) )

        ( close AssuraRunCmdOut )
        ( shell ( sprintf nil "chmod u+x %s" AssuraRunCmd ) )
                
        (let (
              ( AssuraRunPID
                ( ipcBatchProcess 
                  AssuraRunCmd
                  ""
                  AssuraRunLog ) ) )
          ( ipcWait AssuraRunPID )
          (let (
                ( AssuraRunExitStatus
                  ( ipcGetExitStatus AssuraRunPID ) ) )
            ;clean up
            ( shell ( sprintf
                      nil
                      "mkdir -p %s; cp -rf %s/* %s; rm -rf %s" 
                      OutputDirectory
                      TempDirForThisRun
                      OutputDirectory
                      TempDirForThisRun ) )
            ( equal AssuraRunExitStatus 0 )
            ) ) ) ) ) )

(defun AssuraRunLVS ( CellView
                      ExtractRuleFile
                      AssuraSets
                      BindFile
                      CompareFile
                      IncludeFile
                      SchematicFile
                      ScratchDirectory ;path must be < 255 characters
                      OutputDirectory )
  "Run's assura LVS with the ExtractRuleFile/CompareFile/BindFile/LVS Include File on CellView vs CDL SchematicFile.  Uses the <a href=$toolDoc#vfe>vfe</a>.  Returns t if LVS passes, and nil otherwise."
  (let (
        ( RunAssuraScript 
          ( sprintf
            nil
            "%s/share/script/perl/ve/front-end/vfe"
            ( PackageGetPackageRoot ) ) )
        ( CdsLibFile ( ddGetStartup "cds.lib" ) )
        ( TempDirForThisRun ( makeTempFileName 
                              ( sprintf 
                                nil
                                "%s/LVS.XXXXXX" 
                                ScratchDirectory ) ) ) )
    ( createDir TempDirForThisRun )
    (let (
          ( AssuraRunCmd ( sprintf nil "%s/assura.cmd" TempDirForThisRun ) )
          ( AssuraRunLog ( sprintf nil "%s/assura.log" TempDirForThisRun ) )
          )
      (let (
            ( AssuraRunCmdOut
              ( outfile AssuraRunCmd ) ) )
      
        ( fprintf AssuraRunCmdOut "#!/bin/bash\n" )
        ( fprintf 
          AssuraRunCmdOut
          "%s VerificationType=AssuraLvs \"RunName=%s\" \"CdsLib=%s\"  \"LayoutCell=%s\" \"LayoutLibrary=%s\" \"LayoutView=%s\" \"RuleFile=%s\" \"CompareFile=%s\" \"AssuraRsfInclude=%s\" \"WorkingDir=%s\" \"BindingFile=%s\" \"SchematicFile=%s\" %s"
          RunAssuraScript 
          ( getq CellView cellName )
          CdsLibFile
          ( getq CellView cellName )
          ( getq CellView libName )
          ( getq CellView viewName )
          ExtractRuleFile
          CompareFile
          IncludeFile
          TempDirForThisRun
          BindFile
          SchematicFile
          ( StringUtilPrefix
            "AssuraSet.="
            AssuraSets )
          )

        ( close AssuraRunCmdOut )
        ( shell ( sprintf nil "chmod u+x %s" AssuraRunCmd ) )
        (let (
              ( AssuraRunPID
                ( ipcBatchProcess 
                  AssuraRunCmd
                  ""
                  AssuraRunLog ) ) )
          ( ipcWait AssuraRunPID )
          (let (
                ( AssuraRunExitStatus
                  ( ipcGetExitStatus AssuraRunPID ) ) )
            ;clean up
            ( shell ( sprintf
                      nil
                      "mkdir -p %s; cp -rf %s/* %s; rm -rf %s" 
                      OutputDirectory
                      TempDirForThisRun
                      OutputDirectory
                      TempDirForThisRun ) )
            ( equal AssuraRunExitStatus 0 ) ) ) ) ) ) )


(defun AssuraRunAssuraLayerProcessor ( SrcCellView 
                                       TargetLibraryName
                                       TargetCellName
                                       TargetViewName
                                       AssuraRuleFile
                                       ScratchDirectory ;path must be < 255 characters
                                       AssuraLayerMappings
                                       CellMappings
                                       @key
                                       ( AssuraSets nil )
                                       ( LeaveMess nil )
                                       ( Area nil )
                                       ( NoPCells nil )
                                       ( Promote nil )
                                       ( CustomFill nil )
                                       ( AssuraRunLog "/dev/null" )
                                       ( allbloat nil )
                                      )
"
<ol>
<li> copies all instances to a temporary library. See <skill AssuraPrepareCellForProcessing>.</li>
<li> dumps the binary tech file to an ascii tech file for assura to use </li>
<li> optionally defines a cds.lib with gates and stacks replaced with gates-fake and stacks-fake libraries </li>
<li> defines an included .rsf file with given assura layer-dfII LPP mappings </li>
<li Calls assura using <a href=$toolDoc#vfe>vfe</a> with <a href=$cwd/area.rul>area.rul</a></li>
<li> Do some postprocessing on mosaics/instances.  See dependencies.</li>
</ol>
"
(let (
        ( RunAssuraScript 
          ( sprintf
            nil
            "%s/share/script/perl/ve/front-end/vfe"
            ( PackageGetPackageRoot ) ) )
        ( CdsLibFile ( ddGetStartup "cds.lib" ) )
        ( TechFileDBObj ( techGetTechFile SrcCellView ) )
        ( TempDirForThisRun 
          ( makeTempFileName 
            ( sprintf 
              nil
              "%s/ASSURA.XXXXXX" 
              ScratchDirectory ) ) )
        )
    ( createDir TempDirForThisRun )

    (let (
          ;munge the input and put all cells to be added in here
          ( TempInputLibName
            ( LibCreateTempLibrary 
              TechFileDBObj 
              ( sprintf nil "ASSURA_%s" ( getq SrcCellView cellName ) )
              TempDirForThisRun ) )
          ;munge the output to reference the original cells
          ( TempOutputLibName
            ( LibCreateTempLibrary
              TechFileDBObj
              ( sprintf nil "ASSURA_%s" ( getq SrcCellView cellName ) )
              TempDirForThisRun
              ) )
          ( RSFIncludeFile
            ( sprintf nil "%s/outfile.rsf" TempDirForThisRun ) )
          ( AsciiDumpOfTechFile
            ( sprintf nil "%s/techfile.txt" TempDirForThisRun ) )
          ( OutputLibPath nil )
          ( AssuraRunCmd ( sprintf nil "%s/assura.cmd" TempDirForThisRun ) )
          )
      
      ( setq OutputLibPath
             ( PathDirName
               ( car ( ddGetObj TempOutputLibName )->files~>readPath) ) )

      ( tcDumpTechFile TechFileDBObj AsciiDumpOfTechFile ( list "layerDefinitions" "layerRules" "physicalRules" "lxRules") )
      
      (when NoPCells
        (let (
              ( NoPCellCdsLibFile 
                ( makeTempFileName 
                  ( sprintf nil "%s/cds.lib.XXXXXX" TempDirForThisRun ) ) ) )
          (let (
                ( NoPCellCdsLibPort
                  ( outfile NoPCellCdsLibFile ) )
                ( GateFakeDir
                  ( PathDirName
                    ( car ( ddGetObj "gate-fake" )->lib->files )->readPath ) )
                ( StackFakeDir
                  ( PathDirName
                    ( car ( ddGetObj "stack-fake" )->lib->files )->readPath ) )
                )
            ( fprintf NoPCellCdsLibPort 
                      "SOFTINCLUDE %s\n"
                      CdsLibFile )
            ( fprintf NoPCellCdsLibPort 
                      "UNDEFINE gate\n" )
            ( fprintf NoPCellCdsLibPort 
                      "UNDEFINE stack\n" )
            ( fprintf NoPCellCdsLibPort 
                      "UNDEFINE gate#2dfake\n" )
            ( fprintf NoPCellCdsLibPort 
                      "UNDEFINE stack#2dfake\n" )
            ( fprintf NoPCellCdsLibPort 
                      "DEFINE gate %s\n" GateFakeDir )
            ( fprintf NoPCellCdsLibPort 
                      "DEFINE stack %s\n" StackFakeDir )
            ( close NoPCellCdsLibPort )
              
            ( setq CdsLibFile NoPCellCdsLibFile )
            ) ) )

      ( AssuraWriteOutFileRSF 
        RSFIncludeFile
        TempOutputLibName
        TempDirForThisRun
        AsciiDumpOfTechFile
        (if ( or CellMappings Promote )
            ""
          "flat" )
        AssuraLayerMappings )

      
      ( AssuraPrepareCellForProcessing
        SrcCellView TempInputLibName CellMappings )


      (let ( tmpCellView )
           if( allbloat then
               ; what is this? Why is this hard coded as "abstract"?
               tmpCellView=nrOpenCellViewWritable( TempInputLibName ( getq SrcCellView cellName ) "abstract" )
               if( tmpCellView  then
                  printf( "Deleting all instances of %s %s %s\n" tmpCellView~>libName tmpCellView~>cellName tmpCellView~>viewName)
                  foreach( inst tmpCellView->instances
                    if( ! inst~>isShape then
                        ;printf("Deleting %s %s\n" inst~>baseName inst~>cellName ) ; too much spam
                        if(inst~>objType!="mosaicInst"
                          dbDeleteObject( inst )
                          ;dbDeleteObject( inst~>mosaic ) ; probably should not be deleting mosaics
                        )
                      )
                    )
                    dbSave( tmpCellView )
                    dbClose( tmpCellView )
                  else
                     printf("Warning: Cannot open temporary cell for modification\n")
                  )
             )
      )

      ( printf "Starting assura\n" )
        
      (let (
            ( AssuraRunCmdOut
              ( outfile AssuraRunCmd "w" ) )
            )
          
        ( fprintf AssuraRunCmdOut "#!/bin/bash\n" )
        ( fprintf 
          AssuraRunCmdOut
          "%s VerificationType=AssuraDrc \"RunName=%s\" \"CdsLib=%s\"  \"LayoutCell=%s\" \"LayoutLibrary=%s\" \"LayoutView=%s\" \"RuleFile=%s\" OutFile=1 \"AssuraRsfInclude=%s\" \"WorkingDir=%s\" \"Area=%s\" %s"
          RunAssuraScript 
          ( getq SrcCellView cellName )
          CdsLibFile
          ( getq SrcCellView cellName )
          TempInputLibName
          ( getq SrcCellView viewName )
          AssuraRuleFile
          RSFIncludeFile
          TempDirForThisRun
          (if Area
              ( sprintf nil "%f:%f,%f:%f"
                        ( caar Area )
                        ( cadar Area )
                        ( caadr Area )
                        ( cadadr Area ) )
            "all" )
          ( StringUtilPrefix
            "AssuraSet.="
            AssuraSets ) )

        ( close AssuraRunCmdOut )
        ( shell 
          ( sprintf nil "chmod u+x %s" AssuraRunCmd ) )
        ;ipcBeginProcess was dying mysteriously, so i use 'system' now
        ( system ( sprintf 
                   nil
                   "%s&>%s"
                   AssuraRunCmd
                   AssuraRunLog ) )

        ; make sure the library manager doesn't bitch at you
        ( ddUpdateLibList )
        ( ddCreateLib TempOutputLibName 
                      OutputLibPath )
        
        ( printf "Done with assura\n" )
        
        (let (
              ( ResultCellDDObj
                ( ddGetObj
                  TempOutputLibName
                  ( getq SrcCellView cellName )
                  ) ) )
          (if ResultCellDDObj
              (let (
                    ( ResultCellViewDDObj
                      ( car ( getq ResultCellDDObj views ) ) ) )
                (if ( and
                      ResultCellViewDDObj
                      ( getq ResultCellViewDDObj files ) )
                    (let (
                          ( ResultCellView ( dbOpenCellViewByType
                                             TempOutputLibName
                                             ( getq SrcCellView cellName )
                                             ( getq ResultCellViewDDObj name )
                                             nil
                                             "a" ) ) )
                      (let ( 
                            ( TargetCellView ( dbCopyCellView
                                               ResultCellView
                                               TargetLibraryName
                                               TargetCellName
                                               TargetViewName
                                               nil 
                                               nil
                                               t ) ) )

                        (if TargetCellView
                            (let ()
                              ( ddUpdateLibList )
                              ( AssuraFixGeneratedInstances
                                TargetCellView
                                TempOutputLibName
                                CellMappings )
                              
                              (when CustomFill
                                ( AssuraFixCustomFill 
                                  TargetCellView
                                  AssuraLayerMappings
                                  CellMappings
                                  TargetLibraryName ) )

                              ( dbSave TargetCellView )
                                                            
                              ( dbPurge TargetCellView )
                              ( dbPurge ResultCellView )
                              (unless LeaveMess
                                (when ( ddGetObj TempInputLibName )
                                  ( ddDeleteObj 
                                    ( ddGetObj TempInputLibName ) ) )
                                (when ( ddGetObj TempOutputLibName )
                                  ( ddDeleteObj
                                    ( ddGetObj TempOutputLibName ) ) )
                                ( shell ( sprintf nil "rm -rf %s" TempDirForThisRun ) ) )
                              nil )
                          ( sprintf
                            nil
                            "Unable to copy result from %L to %L.   %s"
                            ( list 
                              TempOutputLibName
                              ( getq SrcCellView cellName ) 
                              ( getq ResultCellViewDDObj name ) )
                            ( list TargetLibraryName TargetCellName TargetViewName )
                            TempDirForThisRun ) ) ) )
                  ( sprintf
                    nil
                    "Result cell %L has no views.   %s"
                    ( list TempOutputLibName ( getq SrcCellView cellName ) )
                    TempDirForThisRun ) ) )
            ( sprintf
              nil
              "Result cell %L does not exist.   %s"
              ( list TempOutputLibName ( getq SrcCellView cellName ) )
              TempDirForThisRun ) ) )
      ) ) ) )


(defun AssuraFixCustomFill ( CellView
                             AssuraLayerMappings
                             CellMappings
                             LibName )
  "This function implements post-processing on custom fill cells(i.e. cells with shapes/instances created with assura's generateRectangle/generateCustomFill). Currently, it just replaces generateRectangle shapes with instances.  See <a href=$pdk/Fulcrum/filler/fillershapes.rul></a>"
  ;just create the mappings from LPP->libcellview and use our friendly
  ;neighborhood helper function
  (let (
        ( Done (  makeTable `foo nil ) )
        ( LayerToLPP ( makeTable `foo nil ) )
        ( Mappings ( makeTable `foo nil ) ) )

  ( foreach Mapping AssuraLayerMappings
            ( setarray LayerToLPP ( car Mapping ) ( cadr Mapping ) ) )
  
  ( foreach Mapping CellMappings
            ( setarray Done ( cadr ( car Mapping ) ) t )
            ( setarray Mappings
                       ( arrayref LayerToLPP ( cadr Mapping ) )
                       ( car Mapping ) ) )

  ( AssuraFixCustomFillImpl 
    CellView
    Mappings
    Done
    LibName ) ) )

(defun AssuraFixCustomFillImpl ( CellView
                                 Mappings
                                 Done
                                 LibName
                               )
  "Recursive helper for <skill AssuraFixCustomFillImpl>"
  (cond (
         ( arrayref Done CellView->cellName )
         )
         (
          t
          ( printf "Doing %s\n" CellView->cellName )
          ( setq
            CellView
            (cond (
                   ( equal LibName CellView->libName )
                   CellView )
                  (
                   ( dbCopyCellView
                     CellView
                     LibName
                     CellView->cellName
                     CellView->viewName
                     nil nil t ) ) ) )
            
          ( dbReopen CellView "a" )

          ;check for crappiness
          ( foreach Mosaic CellView->mosaics
                    ( foreach Orient Mosaic->tileArray
                              (when ( not ( equal Orient "R0" ) )
                                (error ( sprintf nil "orient %L in %L %L\n" 
                                                 Orient
                                                 CellView->cellName 
                                                 Mosaic->xy ) ) ) ) )
          ;recurse
          ( foreach 
            Instance
            ( append
              CellView->instances
              ( ListNonDestructiveMapCan
                (lambda ( Mosaic )
                  Mosaic->instanceList )
                CellView->mosaics ) )
            (when ( and Instance->orient
                        ( not ( equal Instance->orient "R0" ) ) )
              ( printf ( sprintf nil "orient %L %L in %L\n"  
                                 Instance->orient
                                 CellView->cellName
                                 Instance->xy
                                 ) ) )
            (when Instance->master
              ( dbSetq 
                Instance
                ( AssuraFixCustomFillImpl 
                  Instance->master
                  Mappings
                  Done
                  LibName )
                master ) )
            )

          ;do some work
          ( AssuraCustomFillConvertShapes CellView Mappings )
          ( dbSave CellView )
          ( setarray Done CellView->cellName CellView )
          
          CellView ) ) )
         
(defun AssuraCustomFillConvertShapes ( CellView Mappings )
  "converts shapes into instances according to Mappings:((LPP->libcellview)...)."
  ( foreach 
    LPP 
    CellView->lpps
    (let (
          ( LibCellView
            ( arrayref Mappings ( list LPP->layerName LPP->purpose ) ) ) )
      (when LibCellView
        (let (
              ( Master
                ( dbOpenCellViewByType
                  ( car LibCellView )
                  ( cadr LibCellView )
                  ( caddr LibCellView )
                  nil
                  "r" ) ) )
          (cond ( 
                 Master
                 ( foreach 
                   Shape 
                   LPP->shapes
                   ( dbCreateInst
                     CellView
                     Master
                     nil
                     ( car Shape->bBox )
                     "R0" )
                   ( dbDeleteObject Shape )
                   ) )
                (
                 ( printf 
                   "Couldn't load %L for %L\n"
                   LibCellView ( list LPP->layerName LPP->purpose ) ) ) ) ) ) ) ) )
