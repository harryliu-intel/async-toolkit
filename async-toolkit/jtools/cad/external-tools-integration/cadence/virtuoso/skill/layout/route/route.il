; Automated routing using ICC
;
; Copyright 2010 Fulcrum Microsystems.  All rights reserved.

; Export, Route, Import
(defun Route
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        (botMetal nil)           ; bottom metal layer to use, or nil to use wires.il
        (topMetal nil)           ; top metal layer to use, or nil to use wires.il
        (pwrMetal nil)           ; top metal layer for power
        (topHalo t)              ; APR should add a top halo
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
        (noPowerFill nil)
        (colorGrid nil)          ; add a color grid to flatten view
        (timingDriven t)
        (softMacro nil)          ; run APR
        (abstractCellName nil)   ; DFII name for APR abstract cell
        (targetCellName nil)     ; DFII name for APR target   cell
        (ecoIter 0)              ; number of spr ECO passes
        (chooseVariant nil)      ; automatically pick v1/v2/v3 variant
        )
  (let (TEMP WDIR)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR = (strcat TEMP "/route/" CV->cellName)
    (unless targetCellName targetCellName=CV->cellName)

    ; export
    CV = (RouteExport ?CV CV
                          ?botMetal botMetal ?topMetal topMetal ?pwrMetal pwrMetal ?topHalo topHalo
                          ?doVerilog doVerilog ?doGds doGds ?doLef doLef ?doDef doDef
                          ?doNondefaultDef doNondefaultDef
                          ?doSkipRouting doSkipRouting
                          ?hookupLaygen hookupLaygen ?hookupD04 hookupD04
                          ?metalFill metalFill ?softMacro softMacro
                          ?abstractCellName abstractCellName ?targetCellName targetCellName
                          ?noPowerFill noPowerFill ?timingDriven timingDriven ?colorGrid colorGrid
                          ?ecoIter ecoIter ?chooseVariant chooseVariant)

    ; copy CV to importView so in-place pins don't need to wait on router to finish
    (when Background (betterCopyCellView CV CV->libName CV->cellName importView nil nil t))

    ; route
    (RouteRun ?CV CV ?Background Background ?Mem Mem ?targetCellName targetCellName)

    ; import
    (cond (Background (printf "Route running in background, finish with RouteImport\n"))
          (t
           CV = (RouteImport ?libName (GetCastLibName targetCellName)
                             ?cellName targetCellName ?viewName importView
                             ?oasFile (strcat WDIR "/" targetCellName ".oas")
                             ?baseCV CV
                             )
           )
          )
    CV
    )
  )

; Export files to the router
(defun RouteExport
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        (botMetal nil)           ; bottom metal layer to use, or nil to use wires.il
        (topMetal nil)           ; top metal layer to use, or nil to use wires.il
        (pwrMetal nil)           ; top metal layer for power
        (topHalo t)              ; APR should add a top halo
        (doVerilog nil)          ; create cellName.v
        (doGds t)                ; create gds for subcells (used for power hookups and maybe fill)
        (doLef t)                ; create cellName.lef
        (doDef t)                ; create cellName.def
        (doHierDef t)            ; create cellName.def with hierarchy separator
        (doNondefaultDef nil)    ; create cellName_nondefault.def
        (doSkipRouting nil)      ; create cellName.SkipRouting.tcl
        (hookupD04 nil)          ; add power hookups around d04 cells
        (hookupLaygen nil)       ; add power hookups around Laygen cells
        (metalFill t)            ; add metal fill
        (noPowerFill nil)
        (doPowerGates nil)       ; optionally skip export of placed pillars
        (powerGrid nil)          ; add a powergrid to flatten view
        (colorGrid nil)          ; add a color grid to flatten view
        (timingDriven t)         ; timing driven custom routing
        (softMacro nil)          ; run APR
        (abstractCellName nil)   ; DFII name for APR abstract cell
        (targetCellName nil)     ; DFII name for APR target   cell
        (ecoIter 0)              ; number of spr ECO passes
        (chooseVariant nil)      ; automatically pick v1/v2/v3 variant
        )
  (let (TEMP WDIR LEFDEF_DIR MaxHeapSize CAST_PATH DFII_DIR SPAR_DIR PDK_DIR
        abstractCV
        Command (statusVerilog t) statusAlignment
        lef_subcells GdsMapFile gds_name
        pout (statusGds t) (statusLef t)
        pinmap dirmap (statusDef t)
        SkipRoutingFileName inplacedPins
        excludeCellsFile
        (hierSep "!") ; a character that is legal, but not otherwise used
        (statusNonDefault t) (errorStr "") placed offset x y gx gy)

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
    (csh (strcat "mkdir -p " LEFDEF_DIR))

    ; make flatten view automatically
    (printf "Flatten Start %s\n" (getCurrentTime))
    (when (or CV->viewName=="floorplan" CV->viewName=="prelayout")
      CV = (MakeFlatten ?CV CV ?topMetal topMetal ?hookupLaygen hookupLaygen ?hookupD04 hookupD04
                        ?noPowerFill noPowerFill ?doPowerGates doPowerGates
                        ?powerGrid powerGrid ?colorGrid colorGrid ?softMacro softMacro
                        ?chooseVariant chooseVariant))

    ; check Alignment
    statusAlignment = (length (MisalignedInstances ?CV CV))==0

    ; write proteus.config
    (ProteusConfig ?CV CV ?botMetal botMetal ?topMetal topMetal
                   ?pwrMetal pwrMetal ?topHalo topHalo ?metalFill metalFill
                   ?timingDriven timingDriven ?softMacro softMacro
                   ?abstractCellName abstractCellName ?targetCellName targetCellName
                   ?ecoIter ecoIter)

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
               abstractCV  = (ReadCV inst->libName inst->cellName "abstract")
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
                     )
               (when doHierDef
                 inst->name = (ListApplyFuncToListAndAccumulateResult
                                (or inst->hierName (list inst->name))
                                (lambda (elem accum)
                                  (if accum (strcat elem hierSep accum) elem))
                                nil
                                nil))
               )

      ; HACK: offset to avoid negative numbers because RDT is stupid
      offset = (GetRDTOffset ?CV ExportCV)
      (OffsetEverything -(car offset):-(cadr offset) ?CV ExportCV)

      ; offset for cell alignment
      offset = (cadr (GetCellAlignment ?CV ExportCV))
      (OffsetEverything -(car offset):-(cadr offset) ?CV ExportCV)

      ; save
      (dbSave ExportCV)

      ; check
      (CheckPinsForExport ?CV ExportCV)

      ; export DEF
      (when statusDef
        defFile = (sprintf nil "%s/%s.def" LEFDEF_DIR CV->cellName)
        statusDef = (ldtrDefWriteOA
                     defFile
                     ExportCV->libName
                     ExportCV->cellName
                     ExportCV->viewName
                     (sprintf nil "%s/%s.def.log" LEFDEF_DIR CV->cellName)
                     "5.8")
        (when (and statusDef doHierDef)
          div = (or (dbGetPropByName (dbGetPropByName (techGetTechFile CV)
                                                      "LEF Legacy")
                                     "DIVIDER")->value
                    "/")
          command = (sprintf nil "sed -i 's:\\%s:\\%s:g' '%s'" hierSep div defFile)
          (shell command))
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
    (unless statusAlignment errorStr = (strcat errorStr "Alignment "))
    (unless statusVerilog errorStr = (strcat errorStr "Verilog "))
    (unless statusGds errorStr = (strcat errorStr " GDS "))
    (unless statusLef errorStr = (strcat errorStr " LEF "))
    (unless statusDef errorStr = (strcat errorStr " DEF "))
    (unless statusNonDefault errorStr = (strcat errorStr "NonDefaultDEF "))
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
      (unless fin (error "ListLefSubcells unable to read %s\n" excludeCellsFile))
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
        (pwrMetal nil)           ; top metal layer for power
        (topHalo t)              ; APR should add a top halo
        (metalFill t)
        (timingDriven t)
        (softMacro nil)          ; run APR
        (abstractCellName nil)   ; DFII name of APR abstract cell
        (targetCellName nil)     ; DFII name of APR target   cell
        (ecoIter 0)              ; number of spr ECO passes
        )
  (let (CAST_PATH DFII_DIR TEMP WDIR file)
    ; files and options
    CAST_PATH = (ConfigFileGetValue TheCDSConfigTable "CAST_PATH")
    CAST_DIR  = (ConfigFileGetValue TheCDSConfigTable "CAST_DIR")
    SPEC_DIR  = (ConfigFileGetValue TheCDSConfigTable "SPEC_DIR")
    DFII_DIR  = (ConfigFileGetValue TheCDSConfigTable "DFII_DIR")
    SPAR_DIR  = (ConfigFileGetValue TheCDSConfigTable "SPAR_DIR")
    TEMP      = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR      = (strcat TEMP "/route/" CV->cellName)

    ; write config file
    file = (outfile (strcat WDIR "/proteus.config"))
    (fprintf file "--cast-path=%s\n" CAST_PATH)
    (fprintf file "--dfII-dir=%s\n" DFII_DIR)
    (fprintf file "--spar-dir=%s\n" SPAR_DIR)
    (fprintf file "--cast-dir=%s\n" CAST_DIR)
    (fprintf file "--spec-dir=%s\n" SPEC_DIR)
    (fprintf file "--lefdef-dir=lefdef\n")
    (fprintf file "--output-dir=proteus\n")
    (fprintf file "--bottom-layer=%d\n" botMetal)
    (fprintf file "--top-layer=%d\n" topMetal)
    (when pwrMetal (fprintf file "--pwr-layer=%d\n" pwrMetal))
    (fprintf file "--top-halo=%d\n" (if topHalo 1 0))
    (fprintf file "--rdt-run-fill=%d\n" (if metalFill 1 0))
    (fprintf file "--timing-driven-route=%d\n" (if softMacro || timingDriven 1 0))
    (fprintf file "--rdt-eco-iter=%d\n" ecoIter)
    (fprintf file "--base=%s\n" (GetCastCellName CV->cellName))
    (cond (softMacro && abstractCellName && targetCellName
           (fprintf file "--cell=%s\n" (GetCastCellName targetCellName))
           (fprintf file "--abstract=%s\n" (GetCastCellName abstractCellName))
           (fprintf file "--task=spr\n")
           (fprintf file "--pv-generate-eco=1 --pv-eco-opts=setup,hold,mpw\n")
           )
          (timingDriven
           (fprintf file "--cell=%s\n" (GetCastCellName CV->cellName))
           (fprintf file "--task=route,extract,pv\n")
           )
          (t
           (fprintf file "--cell=%s\n" (GetCastCellName CV->cellName))
           (fprintf file "--task=route\n"))
          )
    (close file)
    )
  )

; Run the router
(defun RouteRun
  (@key (CV (geGetEditCellView)) ; specify CellView or use edit view
        (Background nil)         ; run in background
        (Mem 4000)               ; qsub memory
        (targetCellName nil)     ; DFII name for APR target cell
        )
  (let (TEMP WDIR oasFile Command status)
    ; files and options
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR = (strcat TEMP "/route/" CV->cellName)
    (unless targetCellName targetCellName=CV->cellName)
    oasFile = (strcat WDIR "/" targetCellName ".oas")

    ; run
    (shell (strcat "rm -rf " oasFile))
    Command = (sprintf nil "cd \"%s\"; proteus --include=proteus.config --mem=%dM %s"
                       WDIR Mem (if Background "&" "--runlocal=1 --runlocal-pv=0"))
    (printf "%s\n" Command)
    status = (shell Command)
    (unless status (error "Proteus failed\n"))

    ; wait for slow filesystem until def is created
    (or Background (WaitForFile oasFile 60))
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
    (unless file (error "GetSubcellLibNames unable to read %s\n" defFile))
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
        (oasFile nil)
	(colorize t)
        (baseCV nil)
        )
  (let (TEMP WDIR libPath logFile Command status ImportCV layoutCV floorplanCV
             win instmap offset CID oasName templib)

    ; choose libName and cellName
    (unless libName  libName  = CV->libName)
    (unless cellName cellName = CV->cellName)
    (unless cellName (error "no cellName\n"))
    (unless libName  libName = (GetCastLibName cellName))
    (unless baseCV baseCV=(dbOpenCellViewByType libName cellName "floorplan"))

    ; map to icc cellname
    CID = (NameStartRename "cell" "cadence" "gds2")
    oasName = (NameDoRename CID cellName)
    fields = (parseString cellName ".")
    short_name = (nth ((length fields)-2) fields)
    top_name = (NameDoRename CID short_name)

    ; directories and files
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR = (strcat TEMP "/route/" cellName)
    (unless oasFile oasFile = (strcat WDIR "/" cellName ".oas"))
    (unless (fileTimeModified oasFile) (error "Cannot read %s\n" oasFile))

    ; create temp library and import cell
    (GDSIIHierMakeLibrary libName (getShellEnvVar "DFII_DIR") TechLibName)
    libPath = (getShellEnvVar "DFII_DIR")
    logFile = (strcat oasFile ".log")
    templib = (strcat "oas_import_temp_" top_name)
    status = (ImportOAS oasFile templib top_name viewName)
    (unless status (error "Unsuccessful OAS import\n"))
    ImportCV = (dbOpenCellViewByType templib top_name viewName "maskLayout" "a")

    ; flatten ICC cells
    (foreach inst ImportCV->instances
      (when (rexMatchp top_name inst->cellName)
	(dbFlattenInst inst 32)
	)
      )

    ; convert instances to dfII hierarchy
    (RouteRemapInstances ?CV ImportCV)

    ; HACK: copy properties from baseCV.  Fix alignment too.
    (when baseCV
      (CopyCellViewProps baseCV ImportCV)
      offset = (GetRDTOffset ?CV baseCV)
      (OffsetEverything (car offset):(cadr offset) ?CV ImportCV)
      )

    ; fixup pins and connectivity
    (DeleteExtraLabels ?CV ImportCV)
    (LabelsToPins ?CV ImportCV)

    ; restore custom alignment
    offset = (cadr (GetCellAlignment ?CV ImportCV))
    (OffsetEverything offset ?CV ImportCV)
    (dbSave ImportCV)

    ; copy ImportCV to correct lib and change window
    ImportCV = (betterCopyCellView ImportCV libName cellName viewName nil nil t)
    win = (hiGetCurrentWindow)
    (when win
      (geOpen ?lib libName ?cell cellName ?view viewName
              ?viewType "maskLayout" ?mode "a" ?window win)
      )

    ; delete import_oas_temp library
    (ddDeleteObj (ddGetObj templib))
    (shell (strcat "rm -rf " templib))

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
  (LoadWires ?CV CV)
  (DeleteMarkers ?CV CV)
  (DeleteKeepout ?CV CV)
  ; (DeleteDumbThings ?CV CV)
  ; (Colorize ?CV CV)
  ; (ConvertFill ?CV CV)    ; HACK: ICC should distinguish fill shapes better
  ; (FixFillerCells ?CV CV) ; HACK: change threshold of spc00 to avoid NV1/PV1 DRC errors
  (FixImportedPins ?CV CV) ; HACK: make sure pins and labels are "pin" purpose
  (defun RunAfterImportDef (@key (CV (geGetEditCellView))) t)
  (RunAfterImportDef ?CV CV)
  (printf "Route Import finished.\n")
  )

; Convert ICC's fill rects to fill purpose
(defun ConvertFill (@key (CV (geGetEditCellView)))
  (foreach fig CV->shapes
           (when fig->objType=="rect" && fig->pin==nil &&
                 (fig->net->name=="floatiss_" || (strncmp fig->net->name "syn_generated" 13)==0 ||
                                  fig->net->name==GNDNetName || fig->net->name==VddNetName)
                 fig->lpp = (list (car fig->lpp) "fill")
                 fig->net = nil
                 )
           )
  t
  )

; Delete markers
(defun DeleteMarkers (@key (CV (geGetEditCellView)))
  (foreach x CV->markers (dbDeleteObject x))
  t
  )

; Delete incorrect shapes that shouldn't be added by ICC or qfill
DumbLayers = (list (list "SRAM" "fill")
	     	   (list "POLYCHECK" "fill")
		   (list "poly" "fill")
		   (list "tcn" "fill")
		   (list "optLego" "id")
		   (list "v1a" "colorA")
		   (list "v1b" "colorB")
		   (list "v1c" "colorC")
		   (list "v2a" "colorA")
		   (list "v2b" "colorB")
		   (list "v2c" "colorC")
		   (list "v3a" "colorA")
		   (list "v3b" "colorB")
		   )
(defun DeleteDumbThings (@key (CV (geGetEditCellView)))
  (foreach fig CV->shapes
           (when (IsInList fig->lpp DumbLayers)
                 (dbDeleteObject fig)
                 )
           )
  t
  )

; Write GDS for a list of cell-views
(defun ExportCellsGDS (directory cells)
  (let (status (statusGds t))
    (foreach cell cells
             status = (ExportGDS ?CV cell ?directory directory)
             statusGds = statusGds && status
             )
    statusGds
    )
  )

; convert a cell name from DFII to CAST
(defun GetCastCellName (cellname
                        @key (tempdir ConfigFileGetValue(TheCDSConfigTable "TEMP"))
                             (castpath ConfigFileGetValue(TheCDSConfigTable "CAST_PATH")))
  (let (castname filename ExecCmd Command file line)
    filename = (strcat tempdir "/" cellname ".castname")
    ExecCmd = (if (boundp `ExecCmd) && (stringp ExecCmd) ExecCmd "")
    (unless (isFile filename)
      Command = (sprintf nil
                         "echo %s | %s %s/rename --type=cell --from=cadence --to=cast > %s"
                         cellname
                         ExecCmd
                         (PackageGetBinRoot)
                         filename
                         )
      (printf "%s\n" Command)
      (shell Command )
      )

    file=(infile filename)
    (unless file (error "GetCastCellName unable to read %s\n" filename))
    (while (gets line file)
      (when line=(car (parseString line " \n"))
            castname = line
            )
      )
    (close file)
    castname
    )
  )

; Change placement status of all instances to "locked".  Use in floorplan view of custom cells.
(defun LockInstances (@key (CV (geGetEditCellView)) (status "locked"))
  (foreach inst CV->instances inst->status=status)
  t
  )

; Converts pins and labels to "pin" purpose
(defun FixImportedPins (@key (CV (geGetEditCellView)))
  (let (net purpose rect)
    (foreach label (setof x CV->shapes x->objType=="label" && x->purpose=="drawing")
             label->lpp = (list label->layerName "pin")
             )
    (foreach pin (setof x CV->shapes x->objType=="rect" && x->pin && x->purpose=="drawing")
             (cond ((member pin->net->name PowerNets)  purpose="vdd")
                   ((member pin->net->name GroundNets) purpose="gnd")
                   (t                                  purpose="drawing")
                   )
             rect = (dbCreateRect CV (list pin->layerName purpose) pin->bBox)
             rect->net = pin->net
             pin->lpp = (list pin->layerName "pin")
             )
    )
  CV
  )

; Copy pins from a floorplan view
(defun CopyPinsFromSourceView (@key (CV (geGetEditCellView)) (srcCV nil))
  (let (fig net pin trm)
    ; assumes same cell name unless specified
    (unless srcCV
      srcCV = (ReadCV CV->libName CV->cellName "floorplan")
      (unless srcCV (printf "Source view not found.\n"))
    )
    (when srcCV
      (DeletePins ?CV CV)
      (foreach trm CV->terminals (dbDeleteObject trm))
      (foreach trm CV->terminals (dbDeleteObject trm))

      (foreach shape srcCV->shapes
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

; Shift contents of a cell
(defun OffsetEverything (offset @key (CV (geGetEditCellView)))
  (let (contents)
    contents = (list CV->prBoundary)
    contents = (append contents CV->instances)
    contents = (append contents CV->shapes)
    contents = (append contents CV->mosiacs)
    contents = (append contents CV->vias)
    (foreach fig contents (dbMoveFig fig CV (list offset "R0")))
    )
  t
  )

; Return name mapping table for APR cell names back to DFII cell names
(defun RouteRemapCellTable ()
  (let (PDK_DIR CID rename name_in fields)
    PDK_DIR = (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
    CID = (NameStartRename "cell" "cast" "cadence")
    rename = (makeTable "rename" nil)
    (foreach file (list "/share/Fulcrum/v2cast/g1m.rul" "/share/Fulcrum/v2cast/hip.rul")
             names_in = (infile (strcat PDK_DIR file))
             (while (gets line names_in)
               fields = (parseString line " ")
               (when (car fields)=="c"
                     (when rename[(cadr fields)]
                           (error "RouteRemapCellTable duplicate mapping %s in file %s\n"
                                  (cadr fields) file)
                           )
                     rename[(cadr fields)]=(NameDoRename CID (caddr fields))
                     )
               )
             (close names_in)
             )
    rename
    )
  )

; Change instance masters from GDS to DFII
(defun RouteRemapInstances (@key (CV (geGetEditCellView)))
  (let (rename dfII_name master)
    rename = (RouteRemapCellTable)
    (foreach inst CV->instances
      dfII_name = rename[inst->cellName]
      (when !dfII_name && (strncmp inst->cellName "g1m" 3)==0 ; TODO: finish PDK/v2cast/g1m.rul
        dfII_name = (sprintf nil "vendor.intel.g1m.%s.%s"
                             (substring inst->cellName 4 8)
                             (substring inst->cellName 12 6))
        (printf "WARNING: implicitly renamed %s\n" dfII_name)
        )
      (unless dfII_name (error "unable to rename %s\n" inst->cellName))
      master=(dbOpenCellViewByType (GetCastLibName dfII_name) dfII_name "layout")
      (unless master (error "unable to read %s\n" dfII_name))
      (printf "rename instance %s type %s => %s\n"
              inst->name inst->cellName master->cellName)
      inst->master=master
      )
    )
  t
  )

; Copy OA properties from one cellview to another
(defun CopyCellViewProps (srcCV dstCV)
  (let (props)
    ;(printf "Copying properties from %s:%s to %s:%s\n" srcCV->cellName srcCV->viewName dstCV->cellName dstCV->viewName)
    props = srcCV->prop
    (foreach p props
      (unless p->name=="lxInternal"
        (dbCreateProp dstCV p->name p->valueType p->value)
	)
      )
    )
  )

; HACK to figure out how to offset prBoundary into upper right quadrant
(defun GetRDTOffset (@key (CV (geGetEditCellView)))
  (let (bbox x y gx gy offset)
    bbox=(GetPrbound CV)->bBox
    x=(car  (car bbox))
    y=(cadr (car bbox))
    offset = 0:0
    (when x<0 || y<0
          gx=(car  gridAlignment["lego"])
          gy=(cadr gridAlignment["lego"])
          offset = (list (floor x/gx)*gx (floor y/gy)*gy)
          )
    offset
    )
  )

; This is a nasty hack because the oasis from ICC puts labels on every net.
; Here we preserve the ones on the pins and delete the rest
(defun DeleteExtraLabels (@key (CV (geGetEditCellView)))
  (let (bb ovr)
    (foreach shape CV->shapes
      (when shape->objType=="label"
	bb = (list (car shape->xy)-MfgGrid:(cadr shape->xy)-MfgGrid
	           (car shape->xy)+MfgGrid:(cadr shape->xy)+MfgGrid)
        ; pin shapes on "pin" purpose are in the oas but the labels are "drawing"
	; so convert the labels purpose and then look for overlapping shapes
        shape->lpp = (list (car shape->lpp) "pin")
	ovr = (dbGetTrueOverlaps CV bb shape->lpp 0)
        (when (length ovr)==1 ; the only overlapping shape is itself
          (printf "Deleting label %s on %s:%s\n" shape->theLabel (car shape->lpp) (cadr shape->lpp))
	  (dbDeleteObject shape)
          )
	)
      )
    t
    )
  )

