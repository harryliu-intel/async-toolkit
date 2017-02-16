; Automated routing using ICC
;
; Copyright 2010 Fulcrum Microsystems.  All rights reserved.

; Export, Route, Import
(defun Route
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        (botMetal nil)           ; bottom metal layer to use, or nil to use wires.il
        (topMetal nil)           ; top metal layer to use, or nil to use wires.il
        (Background nil)         ; optionally run in the background
        (importView "layout")    ; view name to import wiring back to
        (Mem 4000)               ; memory to use
        (doVerilog nil)          ; create .v
        (doGds t)                ; create gds for subcells (used for power hookups and maybe fill)
        (doLef t)                ; create .lef
        (doDef t)                ; create .def
        (doNondefaultDef t)      ; create _nondefault.def
        (doSkipRouting t)        ; create SetSkipRouting.tcl
        (hookupD04 nil)          ; add power hookups around d04 cells
        (hookupLaygen nil)       ; add power hookups around Laygen cells
        (metalFill t)            ; add metal fill
        (topHalo nil)            ; add halos inside prBoundary
        (softMacro nil)          ; lock all instances
        (noPowerFill nil)
        )
  (let ()
    ; export
    CV = (RouteExport ?CV CV
                          ?botMetal botMetal ?topMetal topMetal
                          ?doVerilog doVerilog ?doGds doGds ?doLef doLef ?doDef doDef
                          ?doNondefaultDef doNondefaultDef
                          ?doSkipRouting doSkipRouting
                          ?hookupLaygen hookupLaygen ?hookupD04 hookupD04
                          ?metalFill metalFill ?topHalo topHalo ?softMacro softMacro
                          ?noPowerFill noPowerFill)

    ; copy CV to importView so in-place pins don't need to wait on router to finish
    (when Background (betterCopyCellView CV CV->libName CV->cellName importView nil nil t))

    ; route
    (RouteRun ?CV CV ?Background Background ?Mem Mem)
    
    ; import
    (cond (Background (printf "Route running in background, finish with RouteImport\n"))
          (t CV = (RouteImport ?CV CV ?viewName importView))
          )
    CV
    )
  )

; Export files to the router
(defun RouteExport
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        (botMetal nil)           ; bottom metal layer to use, or nil to use wires.il
        (topMetal nil)           ; top metal layer to use, or nil to use wires.il
        (doVerilog nil)          ; create cellName.v
        (doGds t)                ; create gds for subcells (used for power hookups and maybe fill)
        (doLef t)                ; create cellName.lef
        (doDef t)                ; create cellName.def
        (doNondefaultDef nil)    ; create cellName_nondefault.def
        (doSkipRouting nil)      ; create cellName.SkipRouting.tcl
        (hookupD04 nil)          ; add power hookups around d04 cells
        (hookupLaygen nil)       ; add power hookups around Laygen cells
        (metalFill t)            ; add metal fill
        (topHalo nil)            ; add halos inside prBoundary
        (softMacro nil)          ; lock all instances
        (noPowerFill nil)
	(doPowerGates nil)       ; optionally skip export of placed pillars
        )
  (let (TEMP WDIR LEFDEF_DIR MaxHeapSize CAST_PATH DFII_DIR SPAR_DIR PDK_DIR
        PowerGridCellName abstractCV
        Command (statusVerilog t)
        lef_subcells GdsMapFile gds_name
        pout (statusGds t) (statusLef t) 
        pinmap dirmap (statusDef t)
        SkipRoutingFileName inplacedPins
        excludeCellsFile
        (statusNonDefault t) (errorStr "") placed)

    (printf "Export Start %s\n" (getCurrentTime))

    ; load wires.il and set options
    (LoadWires ?CV CV)
    (unless botMetal botMetal = bundled_bottom_layer)
    (unless topMetal topMetal = bundled_top_layer)

    ; global settings
    CAST_PATH  = (ConfigFileGetValue TheCDSConfigTable "CAST_PATH")
    DFII_DIR = (ConfigFileGetValue TheCDSConfigTable "DFII_DIR")
    SPAR_DIR = (ConfigFileGetValue TheCDSConfigTable "SPAR_DIR") 
    PDK_DIR = (strcat (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT") 
                      "/share/Fulcrum")
    TEMP     = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR     = (strcat TEMP "/route/" CV->cellName)
    LEFDEF_DIR  = (strcat WDIR "/lefdef")
    MaxHeapSize = (nrGetMaxHeapSize)
    (csh (strcat "rm -rf " WDIR))
    (csh (strcat "mkdir -p " WDIR))


    ; make flatten view automatically
    (printf "Flatten Start %s\n" (getCurrentTime))
    (when (or CV->viewName=="floorplan" CV->viewName=="prelayout")
      CV = (MakeFlatten ?CV CV ?topMetal topMetal ?hookupLaygen hookupLaygen ?hookupD04 hookupD04
                        ?noPowerFill noPowerFill ?doPowerGates doPowerGates))

    ; cell names
    PowerGridCellName = (strcat CV->cellName "_pg")

    ; write proteus.config
    (ProteusConfig ?CV CV ?botMetal botMetal ?topMetal topMetal ?metalFill metalFill ?topHalo topHalo)

    ; export Verilog
    (when doVerilog
      (printf "Verilog Start %s\n" (getCurrentTime))
      Command = (sprintf nil
                         (strcat "prs2verilog"
                                 " --cast-path=%s"
                                 " --cell=%s"
                                 " --outfile=%s/%s.v"
                                 " --max-heap-size=%dM"
                                 " --converter=netlist"
                                 " --skip-power-rail"
                                 " --routed"
                                 " --translate=cadence"
                                 " --cadence-name"
                                 " --by-name"
                                 "&> %s/%s.v.log")
                         CAST_PATH
                         CV->cellName
                         LEFDEF_DIR CV->cellName
                         MaxHeapSize
                         LEFDEF_DIR CV->cellName)
      (printf "%s\n" Command)
      statusVerilog = (shell Command)
      )

    ; find subcells for GDS and LEF
    excludeCellsFile = (strcat SPAR_DIR "/cell_list")
    lef_subcells = (ListLefSubcells ?CV CV ?excludeCellsFile excludeCellsFile)

    ; export GDS
    (when doGds
      (printf "GDS Start %s\n" (getCurrentTime))
      statusGds = (ExportCellsGDS (strcat LEFDEF_DIR "/gds") lef_subcells)
      )

    ; export LEF
    (when doLef
      (printf "LEF Start %s\n" (getCurrentTime))
      statusLef = (ExportCellsLEF (strcat LEFDEF_DIR "/" CV->cellName) lef_subcells)
      )

    ; export DEF
    (when doDef
      (printf "DEF Start %s\n" (getCurrentTime))
      statusDef = t
      ExportCV = (betterCopyCellView CV CV->libName CV->cellName "export_tmp" nil nil t)
      (foreach inst ExportCV->instances
               abstractCV  = (dbOpenCellViewByType inst->libName inst->cellName "abstract")
               placed = (if inst->status inst->status "placed") ; keep inst->status if it exists
               (unless softMacro placed="locked")
               (cond (abstractCV
                      inst->master=abstractCV
                      inst->status=(if abstractCV->cellType=="core" placed "locked")
                      )
                     ((isStandardCell inst->master) inst->status=placed)
                     ((isVendorMacro  inst->master) inst->status="locked")
                     ((IsWiringCell   inst->master)
                      (printf "NOTE: flattening wiring cell %s\n" inst->cellName)
                      (dbFlattenInst inst 32)
                      )
                     (t
                      errorStr = (strcat "No abstract view for " inst->cellName)
                      statusDef=nil
                      )
                     )
               )
      (unless doPowerGates
        (DeletePwrGateCells ?CV ExportCV)
        )
      (CheckPinsForExport ?CV ExportCV)

      (dbSave ExportCV)

      ; export DEF
      (when statusDef
        statusDef = (ldtrDefWriteOA
                     (sprintf nil "%s/%s.def" LEFDEF_DIR CV->cellName)
                     ExportCV->libName
                     ExportCV->cellName
                     ExportCV->viewName
                     (sprintf nil "%s/%s.def.log" LEFDEF_DIR CV->cellName)
                     "5.7")
        )
      (dbPurge ExportCV)
      )

    ; write SetSkipRouting.tcl to avoid routing inplace pins
    (when doSkipRouting
      SkipRoutingFileName = (strcat LEFDEF_DIR "/" CV->cellName ".SetSkipRouting.tcl")
      pout=(outfile SkipRoutingFileName)
      (fprintf pout "proc ProteusUserSetSkipRouting {} {\n")
      inplacedPins=(FindInplacedPins CV)
      (foreach pinName inplacedPins 
               rexCompile("\\[")
               pinName=(rexReplace pinName "\\\\[" 0)
               rexCompile("\\]")
               pinName=(rexReplace pinName "\\\\]" 0)
               (fprintf pout "  setAttribute -net \"%s\" -skip_routing true\n" pinName))
      (fprintf pout "}\n")
      (close pout)
      )

    ; export DEF for nondefault rules
    (when doNondefaultDef
      Command = (sprintf nil
                         (strcat "cast2def"
                                 " --cast-path=%s"
                                 " --cell=%s"
                                 " --outfile=%s/%s_nondefault.def"
                                 " --cadence-name"
                                 " --max-heap-size=%dM"
                                 " &> %s/%s_nondefault.def.log")
                         CAST_PATH
                         CV->cellName
                         LEFDEF_DIR CV->cellName
                         MaxHeapSize
                         LEFDEF_DIR CV->cellName
                         )
      (printf "%s\n" Command)
      statusNonDefault = (shell Command)
      )

    ; save
    (dbSave CV)

    ; summarize errors
    (unless statusVerilog errorStr = (strcat errorStr " Verilog"))
    (unless statusGds errorStr = (strcat " GDS Error. " errorStr))
    (unless statusLef errorStr = (strcat " LEF Error. " errorStr))
    (unless statusDef errorStr = (strcat " DEF Error. " errorStr))
    (unless statusNonDefault errorStr = (strcat errorStr " NonDefaultDEF"))
    (when errorStr!="" (error errorStr))

    (printf "Export Finish %s\n" (getCurrentTime))
    ; return CV
    CV
    )
  )

; utility to find the list of subcells that need LEF
; includes power grid, excludes other wiring cells
(defun ListLefSubcells
  (@key (CV (geGetEditCellView))     ; specify CellView
        (excludeCellsFile nil)       ; exclude these cells too
        )
  (let (fin cellName excluded subcells found)
    excluded=(makeTable "excluded" nil)
    (when excludeCellsFile
      fin = (infile excludeCellsFile)
      (unless fin (error "Can't read %s\n" excludeCellsFile))
      (while (fscanf fin "%s" cellName)
        excluded[cellName] = t
        )
      (close fin)
      )
    subcells = nil
    found = (makeTable "found" nil)
    (foreach inst CV->instances
             (when (and !(isStandardCell inst->master)
                        !(isVendorMacro  inst->master)
                        !(excluded[inst->cellName])
                        inst->libName!=TechLibName
                        !found[inst->master->cellView]
                        )
               subcells = (cons inst->master->cellView subcells)
               found[inst->master->cellView] = t
               )
             )
    (sort subcells (lambda (a b) (strcmp a->cellName b->cellName)<0))
    )
  )

; write proteus.config
(defun ProteusConfig
  (@key (CV (geGetEditCellView)) ; specify CellView or use edit view
        (botMetal 0)             ; bottom metal layer
        (topMetal 8)             ; top metal layer
        (metalFill t)
        (topHalo nil)
        )
  (let (CAST_PATH DFII_DIR TEMP WDIR LEFDEF_DIR PROTEUS_DIR file)
    ; files and options
    CAST_PATH = (ConfigFileGetValue TheCDSConfigTable "CAST_PATH")
    DFII_DIR  = (ConfigFileGetValue TheCDSConfigTable "DFII_DIR")
    SPAR_DIR  = (ConfigFileGetValue TheCDSConfigTable "SPAR_DIR")
    TEMP      = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR      = (strcat TEMP "/route/" CV->cellName)
    PROTEUS_DIR = (strcat WDIR "/proteus")
    LEFDEF_DIR  = (strcat WDIR "/lefdef")

    ; write config file
    file = (outfile (strcat WDIR "/proteus.config"))
    (fprintf file "--cast-path=%s\n" CAST_PATH)
    (fprintf file "--dfII-dir=%s\n" DFII_DIR)
    (fprintf file "--spar-dir=%s\n" SPAR_DIR)
    (fprintf file "--cast-dir=%s/cast\n" WDIR)
    (fprintf file "--spec-dir=%s/spec\n" WDIR)
    (fprintf file "--lefdef-dir=%s\n" LEFDEF_DIR)
    (fprintf file "--output-dir=%s\n" PROTEUS_DIR)
    (fprintf file "--cell=%s" (GetCastCellName CV->cellName))
    (fprintf file "--base=%s" (GetCastCellName CV->cellName))
    (fprintf file "--bottom-layer=%d\n" botMetal)
    (fprintf file "--top-layer=%d\n" topMetal)
    (fprintf file "--task=route\n")
    (fprintf file "--rdt-run-fill=%d\n" (if metalFill 1 0))
    (fprintf file "--top-halo=%d\n" (if topHalo 1 0))
    (close file)
    )
  )

; Run the router
(defun RouteRun
  (@key (CV (geGetEditCellView)) ; specify CellView or use edit view
        (Background nil)         ; run in background
        (Mem 4000)               ; qsub memory
        )
  (let (TEMP WDIR defFile Command status)
    ; files and options
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR = (strcat TEMP "/route/" CV->cellName)
    defFile = (strcat WDIR "/" CV->cellName "_routed.def")

    ; run
    (shell (strcat "rm -rf " defFile))
    Command = (sprintf nil "proteus --include=%s/proteus.config --mem=%dM %s"
                       WDIR Mem (if Background "&" "--runlocal=1"))
    (printf "%s\n" Command)
    status = (shell Command)
    (unless status (error "Proteus failed\n"))

    ; wait for slow filesystem until def is created
    (or Background (WaitForFile defFile 60))
    )
  )

; Wait for file to appear on filesystem
(defun WaitForFile (FileName MaxSeconds)
  (let (n)
    n=0
    (while (and n<MaxSeconds !(isFile FileName))
      (printf "Waiting for filesystem...\n") (sleep 1) n=n+1)
    (sleep 5)
    (n<MaxSeconds)
    )
  )

; detect which libNames are referred to by a DEF (very hackish solution)
(defun GetSubcellLibNames (defFile)
  (let (file line libName libNames comp)
    file = (infile defFile)
    (while (gets line file)
      (cond ((strncmp line "COMPONENTS" 10)==0 comp=t)
            ((strncmp line "END COMPONENTS" 14)==0 comp=nil)
            (comp
             fields = (parseString line)
             (when (car fields)=="-"
                   libName = (GetCastLibName (caddr fields))
                   (unless libName (printf "ERROR: can't parse %s\n" line))
                   (unless (member libName libNames)
                     libNames = (cons libName libNames)
                     )
                   )
             )
            )
      )
    (close file)
    libNames
    )
  )

; Import from router
(defun RouteImport
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        (libName  nil)
        (cellName nil)
        (viewName "layout")
        (defFile nil)
        )
  (let (TEMP WDIR SubcellLibNames libPath logFile Command status ImportCV layoutCV floorplanCV
             win instmap SubcellLibString topMetal)

    ; choose libName and cellName
    (unless libName  libName  = CV->libName)
    (unless cellName cellName = CV->cellName)

    ; directories and files
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR = (strcat TEMP "/route/" cellName)
    (unless defFile defFile = (strcat WDIR "/" cellName "_routed.def"))
    (unless (fileTimeModified defFile) (error "Cannot read %s\n" defFile))

    ; create library
    (GDSIIHierMakeLibrary libName (getShellEnvVar "DFII_DIR") TechLibName)

    ; figure out libNames for all exported subcells
    SubcellLibNames  = (GetSubcellLibNames defFile)
    SubcellLibString = (buildString SubcellLibNames)

    ; import def
    libPath = (getShellEnvVar "DFII_DIR") 
    logFile = (strcat defFile ".log")
    status = (ldtrDefReadOA
              defFile ; t_fileName
              libName ; t_libName
              libPath ; [t_libPath libPath]
              cellName ; [t_cellName cellName]
              viewName ; [t_viewName viewName]
              nil ; [t_techName techName] (see BUG 22162)
              "layout abstract floorplan"  ; [t_viewNameList viewNameList]
              SubcellLibString ; [t_masterLibs masterLibs]
              nil ; [ shared nil]
              nil ; [ noRouting nil]
              logFile ; [ t_logName logName]
              nil ; [ useCustomVias nil]
              t   ; [ overwrite nil]
              nil ; [ createModHier nil]
              ; [ t_commentChar commentChar]
              ; [ t_templateFileName templateFileName]
              )
    (unless status (error "Unsuccessful defin\n"))
    ImportCV = (dbOpenCellViewByType libName cellName viewName "maskLayout" "a")
    (dbRefreshCellView ImportCV)

    ; HACK: redraw power pins because they don't import correctly from DEF
    topMetal = (dbGetPropByName CV "topMetal")->value
    (when topMetal (CreatePowerPins ?topMetal topMetal ?CV ImportCV))

    ; label pins
    (LabelPins ?CV ImportCV)
    (dbSave ImportCV)

    ; change window to ImportCV
    win = (hiGetCurrentWindow)
    (when win
      (geOpen ?lib libName ?cell cellName ?view viewName
              ?viewType "maskLayout" ?mode "a" ?window win)
      )

    ; execute custom function
    (FinishRouterImport ?CV ImportCV)

    ; save layout
    (dbSave ImportCV)

    ;return layout
    ImportCV
    )
)

; Finish router import, user over-ridable in wires.il
(defun FinishRouterImport (@key (CV (geGetEditCellView)))
  (DeleteKeepout ?CV CV)
  (InstantiateColorGrid ?CV CV)
  (ConvertFill ?CV CV)    ; HACK: ICC should distinguish fill shapes better
  (FixFillerCells ?CV CV) ; HACK: change threshold of spc00 to avoid NV1/PV1 DRC errors
  (CopyPinsFromSourceView ?CV CV)
  (defun RunAfterImportDef (@key (CV (geGetEditCellView))) t)
  (LoadWires ?CV CV)
  (RunAfterImportDef ?CV CV)
  (printf "Route Import finished.\n")
  )

; Convert ICC's fill rects to fill purpose
(defun ConvertFill (@key (CV (geGetEditCellView)))
  (foreach fig CV->shapes
           (when fig->objType=="rect" && fig->pin==nil
                 fig->lpp = (list (car fig->lpp) "fill")
                 )
           )
  t
  )

; Write GDS for a list of cell-views
(defun ExportCellsGDS (directory cells)
  (let (PDK_DIR GdsMapFile pout gds_name Command (statusGds t))
    PDK_DIR = (strcat (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT") "/share/Fulcrum")
    (foreach cell cells
             (shell (strcat "mkdir -p " directory))
             GdsMapFile = (sprintf nil "%s/%s.map" directory cell->cellName)
             pout = (outfile GdsMapFile)
             gds_name = (translateName "cell" "cadence" "gds2" cell->cellName)
             (fprintf pout "%s %s %s %s\n" cell->libName cell->cellName cell->viewName gds_name)
             (close pout)
             Command = (sprintf nil
                                (strcat "strmout"
                                        " -view '%s'"
                                        " -runDir %s"
                                        " -outputDir %s"
                                        " -layerMap %s/stream/strmout.layermap"
                                        " -propMap %s/stream/prop.map"
                                        " -objectMap %s/stream/objectmap"
                                        " -cellListFile %s"
                                        " -cellMap %s"
                                        " -logFile %s.log"
                                        " ; mv %s/%s %s/%s.gds")
                                cell->viewName directory directory
                                PDK_DIR PDK_DIR PDK_DIR
                                GdsMapFile GdsMapFile GdsMapFile
                                directory gds_name directory gds_name)
               statusGds = statusGds && (shell Command)
               )
    statusGds
    )
  )

; Write LEF for a list of cell-views
(defun ExportCellsLEF (filename cells)
  (let (LefCellsList MWLefCellsList pout)
    ; write lef cells list
    LefCellsList = (sprintf nil "%s.lef_cells_list" filename)
    pout = (outfile LefCellsList)
    (foreach c cells (fprintf pout "%s %s abstract\n" c->libName c->cellName))
    (close pout)
    ; write another without the lib and view so that it can be read directly later
    MWLefCellsList = (sprintf nil "%s.gen_db_mw.lef_cells_list" filename)
    pout = (outfile MWLefCellsList)
    (foreach c cells (fprintf pout "%s\n" c->cellName))
    (close pout)
    
    ; export LEF for cells
    (ldtrLefWriteOA
     (sprintf nil "%s.lef" filename)
     TechLibName
     nil
     LefCellsList
     nil
     (sprintf nil "%s.lef.log" filename)
     t
     "5.7")
    )
  )


(defun GetCastCellName (cellname
                       @key (tempdir ConfigFileGetValue(TheCDSConfigTable "TEMP"))
                            (castpath ConfigFileGetValue(TheCDSConfigTable "CAST_PATH")) )
  (let (castname filename ExecCmd Command file)
    castname "junk"
    filename = strcat( tempdir "/" cellname ".castname" )
    ExecCmd = cond( ( ( boundp( `ExecCmd ) && stringp( ExecCmd ) ) 
                               ExecCmd )
                    ( "" ) )

    when( !isFile(filename)
      Command = sprintf( nil
                "echo %s | %s %s/rename --type=cell --from=cadence --to=cast > %s"
		cellname
                ExecCmd
                PackageGetBinRoot( )
		filename
                )
       printf( "%s\n" Command )
       shell( Command )
    )

    file=infile( filename )
    if( file then
      while( gets( line file )
        when( line=car(parseString(line " "))
          castname = line
        )
      )
    else
      printf("Can't open file %s.\n" filename)
    )
    close( file )
    castname
  )
)

; Change placement status of all instances to "locked".  Use in floorplan view of custom cells.
(defun LockInstances (@key (CV (geGetEditCellView)) (status "locked"))
  (foreach inst CV->instances inst->status=status)
  t
  )


(defun CopyPinsFromSourceView (@key (CV (geGetEditCellView)) (srcCV nil))
  (let (fCV fig net pin trm)
    ; assumes same cell name unless specified
    (unless srcCV
      fCV = (dbOpenCellViewByType CV->libName CV->cellName "floorplan")
      (unless fCV (printf "Source view not found.\n"))
    )
    (when fCV
      (DeletePins ?CV CV)
      (foreach trm CV->terminals (dbDeleteObject trm))
      (foreach trm CV->terminals (dbDeleteObject trm))

      (foreach shape fCV->shapes
        (when shape->pin
          net = (dbMakeNet CV shape->net->name)
	  trm = (dbCreateTerm net net->name shape->pin->term->direction)
          fig = (dbCopyFig shape CV)
          pin = (dbCreatePin net fig); net->name trm)

          ; need to be sure we have drawing purpose too
	  (when fig->purpose=="pin" && 
	        (dbGetPropByName fig "PinType")->value=="BusScript"
             (dbCreateRect CV (list fig->layerName "drawing") fig->bBox)
          )
        )
       )

    (CreatePowerPins ?CV CV)
    (dbSave CV)
    CV
    )
  )
)


; function to validate pins while exporting to proteus
; for now, just checks that we don't have any on complement purpose since ICC crashes
; it would certainly be reasonable to enhance this function to check other things like pin direction
(defun CheckPinsForExport (@key (CV (geGetEditCellView)) )
  (let (fig ok)
    ok = t
    (when (setof fig CV->shapes fig->layerName=="mc0")
      (error "Shape found on mc0 drawing. ICC doesn't like this.\n")
    ok = nil
    )
  ok
  )
)

