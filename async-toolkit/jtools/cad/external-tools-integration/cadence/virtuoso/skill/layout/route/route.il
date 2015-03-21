; Automated routing using VCAR.  Wastes less time than the old UI.
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
        (doVerilog t)            ; create .v
        (doLef t)                ; create .lef
        (doDef t)                ; create .def
        (doNondefaultDef t)      ; create _nondefault.def
        (doSkipRouting t)        ; create SetSkipRouting.tcl
        )
  (let ()
    ; export
    CV = (RouteExport ?CV CV
                          ?botMetal botMetal ?topMetal topMetal
                          ?doVerilog doVerilog ?doLef doLef ?doDef doDef
                          ?doNondefaultDef doNondefaultDef
                          ?doSkipRouting doSkipRouting)

    ; route
    (RouteRun ?CV CV ?Background Background ?Mem Mem)
    
    ; import
    (cond (Background (printf "Route running in background, finish with RouteImport\n"))
          (t (RouteImport ?CV CV ?viewName importView))
          )
    CV
    )
  )

; Export files to the router
(defun RouteExport
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        (botMetal nil)           ; bottom metal layer to use, or nil to use wires.il
        (topMetal nil)           ; top metal layer to use, or nil to use wires.il
        (doVerilog t)            ; create cellName.v
        (doLef t)                ; create cellName.lef
        (doDef t)                ; create cellName.def
        (doNondefaultDef t)      ; create cellName_nondefault.def
        (doSkipRouting t)        ; create cellName.SkipRouting.tcl
        )
  (let (TEMP WDIR MaxHeapSize CAST_PATH DFII_DIR
        PowerGridCellName abstractCV
        Command (statusVerilog t)
        LefCellsList pout (statusLef t) 
        pinmap dirmap (statusDef t)
        SkipRoutingFileName inplacedPins
        (statusNonDefault t) errorStr)

    ; load wires.il and set options
    (LoadWires ?CV CV)
    (unless botMetal botMetal = bundled_bottom_layer)
    (unless topMetal topMetal = bundled_top_layer)

    ; global settings
    CAST_PATH  = (ConfigFileGetValue TheCDSConfigTable "CAST_PATH")
    DFII_DIR = (ConfigFileGetValue TheCDSConfigTable "DFII_DIR")
    TEMP     = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR     = (strcat TEMP "/route/" CV->cellName)
    MaxHeapSize = (nrGetMaxHeapSize)
    (csh (strcat "mkdir -p " WDIR))

    ; make flatten view automatically
    (when (or CV->viewName=="floorplan" CV->viewName=="prelayout")
      CV = (MakeFlatten ?CV CV))

    ; cell names
    PowerGridCellName = (strcat CV->cellName "_pg")

    ; write proteus.config
    (ProteusConfig ?CV CV ?botMetal botMetal ?topMetal topMetal)

    ; export Verilog
    (when doVerilog
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
                         WDIR CV->cellName
                         MaxHeapSize
                         WDIR CV->cellName)
      (printf "%s\n" Command)
      statusVerilog = (shell Command)
      )

    ; export LEF
    (when doLef
      ; write lef cells list
      LefCellsList = (sprintf nil "%s/%s.lef_cells_list" WDIR CV->cellName)
      pout = (outfile LefCellsList)
      (foreach sub (ListLefSubcells ?CV CV ?includeWiringCellNames
                                    (list PowerGridCellName))
               (fprintf pout "%s %s abstract\n" sub->libName sub->cellName))
      (close pout)

      ; export LEF for subcells
      statusLef = (ldtrLefWriteOA
                   (sprintf nil "%s/%s.lef" WDIR CV->cellName)
                   TechLibName
                   nil
                   LefCellsList
                   nil
                   (sprintf nil "%s/%s.lef.log" WDIR CV->cellName)
                   t
                   "5.7")
      )

    ; export DEF
    (when doDef
      statusDef = t
      ExportCV = (dbCopyCellView CV CV->libName CV->cellName "export_tmp" nil nil t)
      (foreach inst ExportCV->instances
               abstractCV  = (dbOpenCellViewByType inst->libName inst->cellName "abstract")
               (cond (abstractCV inst->master=abstractCV)
                     ((isStandardCell inst->master)
                      (printf "WARNING: %s is a standard cell with no abstract\n" inst->cellName))
                     (!abstractCV && (IsWiringCell inst->master) (dbDeleteObject inst))
                     (t
                      (printf "ERROR: %s has no abstract view\n" inst->cellName)
                      statusDef=nil)
                     )
               )
      (dbSave ExportCV)

      ; export DEF
      (when statusDef
        statusDef = (ldtrDefWriteOA
                     (sprintf nil "%s/%s.def" WDIR CV->cellName)
                     ExportCV->libName
                     ExportCV->cellName
                     ExportCV->viewName
                     (sprintf nil "%s/%s.def.log" WDIR CV->cellName)
                     "5.7")
        )
      (dbPurge ExportCV)
      )

    ; write SetSkipRouting.tcl to avoid routing inplace pins
    (when doSkipRouting
      SkipRoutingFileName = (strcat WDIR "/" CV->cellName ".SetSkipRouting.tcl")
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
                         WDIR CV->cellName
                         MaxHeapSize
                         WDIR CV->cellName
                         )
      (printf "%s\n" Command)
      statusNonDefault = (shell Command)
      )

    ; save
    (dbSave CV)

    ; summarize errors
    errorStr = ""
    (unless statusVerilog errorStr = (strcat errorStr " Verilog"))
    (unless statusLef errorStr = (strcat errorStr " LEF"))
    (unless statusDef errorStr = (strcat errorStr " DEF"))
    (unless statusNonDefault errorStr = (strcat errorStr "NonDefaultDEF"))
    (when errorStr!="" (error errorStr))

    ; return CV
    CV
    )
  )

; utility to find the list of subcells that need LEF
; includes power grid, excludes other wiring cells
(defun ListLefSubcells
  (@key (CV (geGetEditCellView))     ; specify CellView
        (includeWiringCellNames nil) ; include specified wiring cells
        )
  (let (subcells)
    subcells = nil
    (foreach inst CV->instances
             (when (and !(isStandardCell inst->master)
                        inst->libName!=TechLibName
                        (member inst->master->cellView subcells)==nil
                        (or !(IsWiringCell inst->master)
                            (member inst->cellName includeWiringCellNames)
                            )
                        )
               subcells = (cons inst->master->cellView subcells)
               )
             )
    subcells
    )
  )

; write proteus.config
(defun ProteusConfig
  (@key (CV (geGetEditCellView)) ; specify CellView or use edit view
        (botMetal 1)             ; bottom metal layer
        (topMetal 8)             ; top metal layer
        )
  (let (CAST_PATH DFII_DIR TEMP WDIR file)
    ; files and options
    CAST_PATH = (ConfigFileGetValue TheCDSConfigTable "CAST_PATH")
    DFII_DIR  = (ConfigFileGetValue TheCDSConfigTable "DFII_DIR")
    TEMP      = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR      = (strcat TEMP "/route/" CV->cellName)

    ; write config file
    file = (outfile (strcat WDIR "/proteus.config"))
    (fprintf file "--cast-path=%s\n" CAST_PATH)
    (fprintf file "--dfII-dir=%s\n" DFII_DIR)
    (fprintf file "--cast-dir=%s/cast\n" WDIR)
    (fprintf file "--spec-dir=%s/spec\n" WDIR)
    (fprintf file "--lefdef-dir=%s\n" WDIR)
    (fprintf file "--scratch-dir=\n")
    (fprintf file "--verilog=%s/%s.v\n" WDIR CV->cellName)
    (fprintf file "--output-dir=%s\n" WDIR)
    (fprintf file "--cell=%s\n" CV->cellName)
    (fprintf file "--bottom-layer=%d\n" botMetal)
    (fprintf file "--top-layer=%d\n" topMetal)
    (fprintf file "--task=rdt_route\n")
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
    Command = (sprintf nil "proteus --include=%s/proteus.config --mem=%dM%s"
                       WDIR Mem (if Background " &" ""))
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
             win instmap SubcellLibString)

    ; choose libName and cellName
    (unless libName  libName  = CV->libName)
    (unless cellName cellName = CV->cellName)

    ; directories and files
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR = (strcat TEMP "/route/" CV->cellName)
    (unless defFile defFile = (strcat WDIR "/" CV->cellName "_routed.def"))

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
              t   ; [ useCustomVias nil]
              t   ; [ overwrite nil]
              nil ; [ createModHier nil]
              ; [ t_commentChar commentChar]
              ; [ t_templateFileName templateFileName]
              )
    (unless status (error "Unsuccessful defin\n"))
    ImportCV = (dbOpenCellViewByType libName cellName viewName "maskLayout" "a")

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
  (defun RunAfterImportDef (@key (CV (geGetEditCellView))) t)
  (LoadWires ?CV CV)
  (RunAfterImportDef ?CV CV)
  )
