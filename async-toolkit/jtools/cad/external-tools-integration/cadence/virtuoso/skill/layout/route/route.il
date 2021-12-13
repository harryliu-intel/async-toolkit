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
	(denseStaples nil)	 ; APR should add staples at dense pitch
        (thresholds (list "c" "b")) ; transistor thresholds to use
        (Background nil)         ; optionally run in the background
        (importView "layout")    ; view name to import wiring back to
        (Mem (nrGetMaxHeapSize)) ; memory to use
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
        (tau nil)                ; override tau for Proteus
        (ecoIter 0)              ; number of spr ECO passes
        (rdtUseFlops nil)        ; use flops instead of latches
        (rdtUserTcl nil)         ; optional tcl file
        )
  (let (TEMP WDIR)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR = (strcat TEMP "/route/" CV->cellName)
    (unless targetCellName targetCellName=CV->cellName)

    ; export
    CV = (RouteExport ?CV CV
                          ?botMetal botMetal ?topMetal topMetal ?pwrMetal pwrMetal
			  ?topHalo topHalo ?denseStaples denseStaples
                          ?doVerilog doVerilog ?doGds doGds ?doLef doLef ?doDef doDef
                          ?doNondefaultDef doNondefaultDef
                          ?doSkipRouting doSkipRouting
                          ?hookupLaygen hookupLaygen ?hookupD04 hookupD04
                          ?metalFill metalFill ?softMacro softMacro
                          ?abstractCellName abstractCellName ?targetCellName targetCellName
                          ?noPowerFill noPowerFill ?timingDriven timingDriven ?colorGrid colorGrid
                          ?tau tau ?ecoIter ecoIter ?rdtUseFlops rdtUseFlops ?rdtUserTcl rdtUserTcl
                          ?thresholds thresholds)

    ; copy CV to importView so in-place pins don't need to wait on router to finish
    (when Background (betterCopyCellView CV CV->libName CV->cellName importView nil nil t t))

    ; route
    (RouteRun ?CV CV ?Background Background ?Mem Mem ?targetCellName targetCellName)

    ; import
    (cond (Background (printf "Route running in background, finish with RouteImport\n"))
          (t
           CV = (RouteImport ?libName (GetCastLibName targetCellName)
                             ?cellName targetCellName ?viewName importView
                             ?gdsFile (strcat WDIR "/" targetCellName ".gds")
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
	(denseStaples nil)	 ; APR should add staples at dense pitch
        (thresholds (list "c" "b")) ; transistor thresholds to use
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
        (tau nil)                ; override tau for Proteus
        (ecoIter 0)              ; number of spr ECO passes
        (pinMap nil)             ; rename ports based on map
        (rdtUseFlops nil)        ; use flops instead of latches
        (rdtUserTcl nil)         ; optional tcl file
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
        (statusNonDefault t) (errorStr "") placed offset x y gx gy
        flattened_instances)

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
                        ?powerGrid powerGrid ?colorGrid colorGrid ?softMacro softMacro))

    ; check Alignment of locked non-standard cells
    statusAlignment=t
    (foreach inst (MisalignedInstances ?CV CV)
             (when inst->status=="locked" || inst->status=="firm" statusAlignment=nil)
             )

    ; write proteus.config
    (ProteusConfig ?CV CV ?botMetal botMetal ?topMetal topMetal
                   ?pwrMetal pwrMetal ?topHalo topHalo ?denseStaples denseStaples ?metalFill metalFill
                   ?timingDriven timingDriven ?softMacro softMacro
                   ?abstractCellName abstractCellName ?targetCellName targetCellName
                   ?tau tau ?ecoIter ecoIter ?rdtUseFlops rdtUseFlops ?rdtUserTcl rdtUserTcl
                   ?thresholds thresholds)

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

      ; reconstruct net name to distinguish routed=false hierarchy
      (when doHierDef
        (foreach net ExportCV->nets
                 (dbRenameNet net (LocalizeNet net->name ?flat_map flattened_instances ?hierSep hierSep))
                 )
        )

      ; rename ports
      (when pinMap
        (foreach term ExportCV->terminals
          newName = pinMap[term->name]
          (when (and newName newName!=term->name)
            term->name = newName
            (dbRenameNet term->net newName))))

      ; offset for cell alignment and avoid negative numbers because RDT is stupid
      offset = (GetRDTOffset ?CV ExportCV)
      (OffsetEverything offset ?CV ExportCV)

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
	(denseStaples nil)	 ; APR should add staples at dense pitch
        (thresholds (list "c" "b")) ; transistor thresholds to use
        (metalFill t)
        (timingDriven t)
        (softMacro nil)          ; run APR
        (abstractCellName nil)   ; DFII name of APR abstract cell
        (targetCellName nil)     ; DFII name of APR target   cell
        (tau nil)                ; override tau for Proteus
        (ecoIter 0)              ; number of spr ECO passes
        (rdtUseFlops nil)        ; use flops instead of latches
        (rdtUserTcl nil)         ; optional tcl file
        )
  (let (CAST_PATH DFII_DIR TEMP WDIR file)
    ; files and options
    CAST_PATH = (ConfigFileGetValue TheCDSConfigTable "CAST_PATH")
    CAST_DIR  = (ConfigFileGetValue TheCDSConfigTable "CAST_DIR")
    SPEC_DIR  = (ConfigFileGetValue TheCDSConfigTable "SPEC_DIR")
    DFII_DIR  = (ConfigFileGetValue TheCDSConfigTable "DFII_DIR")
    SPAR_DIR  = (ConfigFileGetValue TheCDSConfigTable "SPAR_DIR")
    VERI_DIR  = (ConfigFileGetValue TheCDSConfigTable "VERI_DIR")
    TEMP      = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR      = (strcat TEMP "/route/" CV->cellName)

    ; write config file
    file = (outfile (strcat WDIR "/proteus.config"))
    (fprintf file "--cast-path=%s\n" CAST_PATH)
    (fprintf file "--dfII-dir=%s\n" DFII_DIR)
    (fprintf file "--spar-dir=%s\n" SPAR_DIR)
    (fprintf file "--cast-dir=%s\n" CAST_DIR)
    (fprintf file "--spec-dir=%s\n" SPEC_DIR)
    (fprintf file "--verilog-dir=%s\n" VERI_DIR)
    (fprintf file "--lefdef-dir=lefdef\n")
    (fprintf file "--output-dir=proteus\n")
    (fprintf file "--bottom-layer=%d\n" botMetal)
    (fprintf file "--top-layer=%d\n" topMetal)
    (when pwrMetal (fprintf file "--pwr-layer=%d\n" pwrMetal))
    (fprintf file "--top-halo=%d\n" (if topHalo 1 0))
    (fprintf file "--dense-staples=%d\n" (if denseStaples 1 0))
    (fprintf file "--thresholds=%s\n" (buildString thresholds ","))
    (fprintf file "--rdt-run-fill=%d\n" (if metalFill 1 0))
    (fprintf file "--timing-driven-route=%d\n" (if softMacro || timingDriven 1 0))
    (fprintf file "--soft-macro=%d\n" (if softMacro 1 0))
    (when tau (fprintf file "--tau=%g\n" tau))
    (fprintf file "--rdt-eco-iter=%d\n" ecoIter)
    (when rdtUseFlops (fprintf file "--rdt-use-flops=1\n"))
    (when rdtUserTcl (fprintf file "--rdt-user-tcl=%s\n" (PathMakePathAbsolute rdtUserTcl)))
    (fprintf file "--base=%s\n" (GetCastCellName CV->cellName))
    (fprintf file "--csp-register-width=1025\n")
    (fprintf file "--pv-generate-sdf=1\n")
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
        (Mem (nrGetMaxHeapSize)) ; qsub memory
        (targetCellName nil)     ; DFII name for APR target cell
        )
  (let (TEMP WDIR gdsFile Command status)
    ; files and options
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR = (strcat TEMP "/route/" CV->cellName)
    (unless targetCellName targetCellName=CV->cellName)
    gdsFile = (strcat WDIR "/" targetCellName ".gds")

    ; run
    (shell (strcat "rm -rf " gdsFile))
    Command = (sprintf nil "cd \"%s\"; proteus --include=proteus.config --mem=%dM %s"
                       WDIR Mem (if Background "&" "--runlocal=1 --runlocal-pv=1"))
    (printf "%s\n" Command)
    status = (shell Command)
    (unless status (error "Proteus failed\n"))

    ; wait for slow filesystem until def is created
    (or Background (WaitForFile gdsFile 60))
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

; Given a flat net name, return a hierarchical path to the net
(defun LocalizeNet (name @key (flat_map flattened_instances) (hierSep "!"))
  (let (parts tryname newname sep)
    parts=(parseString name ".")
    tryname=(car parts)
    newname=(car parts)
    (foreach part (cdr parts)
             sep=(if flat_map[tryname] hierSep ".")
             tryname=(strcat tryname "." part)
             newname=(strcat newname sep part)
             )
    newname
    )
  )

; Import from router
(defun RouteImport
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        (libName  nil)
        (cellName nil)
        (viewName "layout")
        (oasFile nil)
        (gdsFile nil)
	(colorize t) ; run ColorizeByOverlay
        (baseCV nil)
        (top_name nil)
        (pinMap nil)
        (calibreOffset t)
        )
  (let (TEMP WDIR logFile Command status ImportCV layoutCV floorplanCV
             win instmap CID templib fields short_name offset file origFile oas)

    ; choose libName and cellName
    (unless libName  libName  = CV->libName)
    (unless cellName cellName = CV->cellName)
    (unless cellName (error "no cellName\n"))
    (unless libName  libName = (GetCastLibName cellName))
    (unless baseCV baseCV=(dbOpenCellViewByType libName cellName "floorplan"))

    ; map to icc cellname
    fields = (parseString cellName ".")
    (unless (length fields)>=3
            (error "cellName must be module.type.subtype, try ?top_name")
            )
    short_name = (nth ((length fields)-2) fields)
    (unless top_name
      CID = (NameStartRename "cell" "cadence" "gds2")
      top_name = (NameDoRename CID short_name)
      (NameStopRename CID)
      )

    ; directories and files
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR = (strcat TEMP "/route/" cellName)
    (cond (oasFile && gdsFile (error "oasFile and gdsFile options are exclusive"))
          (gdsFile file=gdsFile)
          (oasFile file=oasFile oas=t)
          (t file=(strcat WDIR "/" cellName ".gds"))
          )
    (unless (fileTimeModified file) (error "Cannot read %s\n" file))

    ; HACK: restore cell alignment by preprocessing GDS or OAS file using Calibre
    (when calibreOffset && baseCV
      offset = (GetRDTOffset ?CV baseCV)
      origFile = file
      file = (sprintf nil "%s/offset_layout_%s.%s"
                          TEMP
                          top_name
                          (if oas "oas" "gds"))
      Command = (sprintf nil "offset_layout %f %f %s %s" (car offset)
                                                         (cadr offset)
                                                         origFile
                                                         file)
      (unless (shell Command)
        (error "%s failed" Command)))

    ; create temp library and import cell
    (GDSIIHierMakeLibrary libName (getShellEnvVar "DFII_DIR") TechLibName)
    logFile = (strcat file ".log")
    templib = (strcat "route_import_" top_name)
    (GDSIIHierMakeLibrary templib TEMP TechLibName)
    status = (if oas
               (ImportOAS file templib top_name "layout")
               (ImportGDS file templib top_name ?prefix "" ?suffix "")
               )
    (unless status (error "Unsuccessful import\n"))
    (when origFile (shell (strcat "rm -f " file)))
    ImportCV = (dbOpenCellViewByType templib top_name "layout" "maskLayout" "a")

    ; flatten ICC cells
    (foreach inst ImportCV->instances
      (when (rexMatchp top_name inst->cellName)
	(dbFlattenInst inst 32)
	)
      )

    ; convert instances to dfII hierarchy
    (RouteRemapInstances ?CV ImportCV)

    ; HACK: copy properties from baseCV and restore cell alignment
    (when baseCV
      (CopyCellViewProps baseCV ImportCV)
      (unless calibreOffset
        offset = (GetRDTOffset ?CV baseCV)
        (OffsetEverything offset ?CV ImportCV ?reverse t)))

    ; colorize
    (when colorize (ColorizeByOverlay ?CV ImportCV))

    ; fixup pins and connectivity
    (LabelsToPins ?CV ImportCV)
    (when pinMap
      (foreach term ImportCV->terminals
        newName = pinMap[term->name]
        (when (and newName newName!=term->name)
          term->name = newName
          (dbRenameNet term->net newName))))
    (DeleteLabels ?CV ImportCV)
    (LabelPins ?CV ImportCV)
    (dbSave ImportCV)

    ; copy ImportCV to correct lib and change window
    (unless (IsCellViewWritable libName cellName viewName)
      viewName=(strcat viewName "_import")
      (warn "Import to view %s because target view was not writable\n" viewName)
      )
    ImportCV = (betterCopyCellView ImportCV libName cellName viewName nil nil t)
    win = (hiGetCurrentWindow)
    (when win
      (geOpen ?lib libName ?cell cellName ?view viewName
              ?viewType "maskLayout" ?mode "a" ?window win)
      )

    ; delete import_oas_temp library
    (ddDeleteObj (ddGetObj templib))

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
  (defun RunAfterImportDef (@key (CV (geGetEditCellView))) t)
  (LoadWires ?CV CV)
  (DeleteMarkers ?CV CV)
  (DeleteKeepout ?CV CV)
  ; (DeleteDumbThings ?CV CV)
  ; (Colorize ?CV CV)
  ; (ConvertFill ?CV CV)    ; HACK: ICC should distinguish fill shapes better
  ; (FixFillerCells ?CV CV) ; HACK: change threshold of spc00 to avoid NV1/PV1 DRC errors
  ;(FixImportedPins ?CV CV)  ; HACK: add drawing rectangles under pins
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

; Add drawing rectangles under pins (TODO: remove when unnecessary)
(defun FixImportedPins (@key (CV (geGetEditCellView)))
  (let (rect)
    (foreach pin (setof x CV->shapes x->objType=="rect" && x->pin && x->purpose=="pin")
             rect = (dbCreateRect CV (list pin->layerName "drawing") pin->bBox)
             rect->net = pin->net
             (SetShapeColor rect (GetShapeColor pin))
             (dbReplaceProp rect "autogen" "string" "FixImportedPins")
             )
    )
  t
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
(defun OffsetEverything (offset @key (CV (geGetEditCellView)) (reverse nil))
  (leMoveCellViewOrigin CV (if reverse offset -(car offset):-(cadr offset)))
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
               fields = (parseString line " \n")
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
    (NameStopRename CID)
    rename
    )
  )

; Change instance masters from GDS to DFII
(defun RouteRemapInstances (@key (CV (geGetEditCellView)))
  (let (rename flatten dfII_name master)
    flatten = (makeTable "flatten" nil)
    rename = (RouteRemapCellTable)
    (foreach inst CV->instances
      dfII_name = rename[inst->cellName]
      (when !dfII_name && (strncmp inst->cellName "g1m" 3)==0 ; TODO: finish PDK/v2cast/g1m.rul
        dfII_name = (sprintf nil "vendor.intel.g1m.%s.%s"
                             (substring inst->cellName 4 8)
                             (substring inst->cellName 12 6))
        (printf "WARNING: implicitly renamed %s\n" dfII_name)
        rename[inst->cellName] = dfII_name
        )
      (cond (dfII_name
             master=(dbOpenCellViewByType (GetCastLibName dfII_name) dfII_name "layout")
             (unless master (error "unable to read %s\n" dfII_name))
             inst->master=master
             )
            (t
             (unless flatten[inst->cellName]
               (printf "WARNING: unable to rename %s, flattening instead\n" inst->cellName)
               flatten[inst->cellName]=t
               )
             (dbFlattenInst inst 32)
             )
            )
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
  (let (bbox x y gx gy ox oy offset)
    offset = (cadr (GetCellAlignment ?CV CV))
    ox=(car  offset)
    oy=(cadr offset)
    bbox=(GetPrbound CV)->bBox
    x=(car  (car bbox)) + ox
    y=(cadr (car bbox)) + oy
    (when x<0
      gx=(car  gridAlignment["lego"])
      ox=ox - (floor x/gx)*gx
      )
    (when y<0
      gy=(cadr gridAlignment["lego"])
      oy=oy - (floor y/gy)*gy
      )
    ox:oy
    )
  )
