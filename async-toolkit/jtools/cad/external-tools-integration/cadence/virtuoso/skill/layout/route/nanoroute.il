; Automated routing using VCAR.  Wastes less time than the old UI.
;
; Copyright 2010 Fulcrum Microsystems.  All rights reserved.

; Export, Route, Import using Nanoroute
(defun NanoRoute
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        (bottomMetal nil)        ; bottom metal layer to use, or nil to use wires.il
        (topMetal nil)           ; top metal layer to use, or nil to use wires.il
        (SaveQuit nil)           ; automatically save and quit NanoRoute
        (Background t)           ; select foreground or background
        (importView "layout_pg") ; view name to import wiring back to
        (Ticket "np")            ; QSUB ticket: np, nano
        (Mem 4000)               ; QSUB mem to request
        (Soc (getShellEnvVar "SOC_SCRIPT")) ; which version of Encounter to use
        (timingDriven nil)       ; use proteus script to route with timing constraints
        (Verify t)               ; run VerifyGeom and VerifyConn at the end
        (SkipCompleteNets t)     ; auto-skip complete nets
        (strictNondefault t)     ; strict nondefault width/spacing rules
        (AutoStop t)             ; stop routing if too many errors
        (routeGND nil)           ; route GND net
        (routeVdd nil)           ; route Vdd net
        (doVerilog t)            ; create .v
        (doLef t)                ; create .lef
        (doDef t)                ; create .def
        (doNondefaultDef t)      ; create _nondefault.def
        (doTracksDef nil)        ; create _tracks.def
        (doSkipRouting t)        ; create SetSkipRouting.tcl
        )
  (let ()
    ; load wires.il and set options
    (LoadWires)
    (unless bottomMetal bottomMetal = bundled_bottom_layer)
    (unless topMetal    topMetal    = bundled_top_layer)

    ; export files for NanoRoute
    CV = (NanoRouteExport ?CV CV ?timingDriven timingDriven
                          ?doVerilog doVerilog
                          ?doLef doLef ?doDef doDef ?doNondefaultDef doNondefaultDef
                          ?doTracksDef doTracksDef ?doSkipRouting doSkipRouting)

    ; write TCL and config files
    (ProteusConfig ?CV CV ?bottomMetal bottomMetal ?topMetal topMetal)
    (NanoRouteTcl ?CV CV ?bottomMetal bottomMetal ?topMetal topMetal
                  ?SkipCompleteNets SkipCompleteNets
                  ?SaveQuit (or SaveQuit Background)
                  ?Verify Verify ?AutoStop AutoStop
                  ?strictNondefault strictNondefault
                  ?routeGND routeGND ?routeVdd routeVdd)

    ; run NanoRoute
    (cond (timingDriven (ProteusRun ?CV CV ?Background Background ?Mem Mem))
          (t (NanoRouteRun ?CV CV ?Background Background ?Ticket Ticket ?Mem Mem ?Soc Soc))
          )
             
    ; import from NanoRoute
    (cond (Background
           (printf "NanoRoute running in background, finish with NanoRouteImport\n")
           )
          (t
           (NanoRouteImport ?CV CV ?viewName importView)
           )
          )
    CV
    )
  )

; Export files for Proteus 
(defun NanoRouteExport
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        (Proteus nil)            ; target Proteus flow
        (Synchronous nil)        ; target synchronous flow
        (timingDriven nil)       ; target timing driven flow
        (Verbose nil)            ; verbose feedback
        (doVerilog t)            ; create .v
        (doLef t)                ; create .lef
        (doDef t)                ; create .def
        (doNondefaultDef t)      ; create _nondefault.def
        (doTracksDef nil)        ; create _tracks.def
        (doSkipRouting t)        ; create SkipRouting.tcl
        )
  (let (FulcrumPDKRoot BinRoot TEMP MaxHeapSize CAST_PATH DFII_DIR
                       Components RComponents CellBaseName type_sub
                       PowerGridCellName abstractCV
                       VerilogFileName Command (statusVerilog t)
                       LefCellsList pout (statusLef t) 
                       pinmap dirmap (statusDef t)
                       SkipRoutingFileName inplacedPins
                       (statusNonDefault t) (statusTracks t) errorStr Custom)

    ; global settings
    FulcrumPDKRoot = (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
    BinRoot = (PackageGetBinRoot)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    MaxHeapSize = ( nrGetMaxHeapSize )
    CAST_PATH = (ConfigFileGetValue TheCDSConfigTable "CAST_PATH")
    DFII_DIR = (ConfigFileGetValue TheCDSConfigTable "DFII_DIR")
    Custom = !(or Proteus timingDriven Synchronous)

    ; make flatten view automatically
    (when (or CV->viewName=="floorplan" CV->viewName=="prelayout")
      CV = (MakeFlatten ?CV CV ?Proteus Proteus))

    ; cell and file names
    Components = (parseString CV->cellName ".")
    RComponents = (reverse Components)
    CellBaseName = (cadr RComponents)
    (when (length RComponents)<3 (error "Cell name violates CAST conventions\n"))
    type_sub = (sprintf nil "%s_%s" (cadr RComponents) (car RComponents)) 
    PowerGridCellName = (strcat CV->libName ".wires." type_sub "_POWER_GRID_TIEOFF")

    ; export Verilog
    (when (and Custom doVerilog)
      VerilogFileName = (strcat CV->cellName ".v")
      Command = (sprintf nil
                         (strcat "%s/prs2verilog"
                                 " --cast-path=%s"
                                 " --cell=%s"
                                 " --outfile=%s"
                                 " --max-heap-size=%dM"
                                 " --converter=netlist"
                                 " --skip-power-rail"
                                 " --routed"
                                 " --translate=cadence"
                                 " --cadence-name"
                                 "&> exportVerilogNetlist.log")
                         BinRoot
                         CAST_PATH
                         CV->cellName
                         VerilogFileName
                         MaxHeapSize)
      (printf "%s\n" Command)
      statusVerilog = (shell Command)
      )

    ; export LEF
    (when doLef
      ; write lef cells list
      LefCellsList = (sprintf nil "%s.lef_cells_list" CV->cellName)
      pout = (outfile LefCellsList)
      (foreach sub (ListLefSubcells ?CV CV ?includeWiringCellNames
                                    (list PowerGridCellName))
               (fprintf pout "%s %s abstract\n" sub->libName sub->cellName))
      (close pout)

      ; export LEF for subcells
      statusLef = (ldtrLefWriteOA
                   (sprintf nil "%s.lef" CV->cellName)
                   TechLibName
                   nil
                   LefCellsList
                   nil
                   (sprintf nil "%s.lef.log" CV->cellName)
                   t
                   "5.7")
      )

    ; export DEF
    (when doDef
      statusDef = t
      ExportCV = (dbCopyCellView CV CV->libName CV->cellName "export_tmp" nil nil t)
      (when Proteus (dbTransformCellView ExportCV 1.0 90.0)) ; rotate for Proteus
      (foreach inst ExportCV->instances
               abstractCV  = (dbOpenCellViewByType inst->libName inst->cellName "abstract")
               (cond (abstractCV inst->master=abstractCV)
                     (!abstractCV && (IsWiringCell inst) (dbDeleteObject inst))
                     (t
                      (printf "ERROR: %s has no abstract view\n" inst->cellName)
                      statusDef=nil)
                     )
               )
      (dbSave ExportCV)

      ; export DEF
      (when statusDef
        statusDef = (ldtrDefWriteOA
                     (sprintf nil "%s.def" CV->cellName)
                     ExportCV->libName
                     ExportCV->cellName
                     ExportCV->viewName
                     (sprintf nil "%s.def.log" CV->cellName)
                     "5.7")
        )
      )

    ; write SetSkipRouting.tcl
    (when doSkipRouting
      SkipRoutingFileName = (strcat CV->cellName ".SetSkipRouting.tcl")
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
                         (strcat "%s/cast2def"
                                 " --cast-path=%s"
                                 " --cell=%s"
                                 " --outfile=%s"
                                 " --cadence-name"
                                 " --max-heap-size=%dM"
                                 " &> exportNondefaultRuleDEF.log")
                         BinRoot
                         CAST_PATH
                         CV->cellName
                         (strcat CV->cellName "_nondefault.def")
                         MaxHeapSize)
      (printf "%s\n" Command)
      statusNonDefault = (shell Command)
      )

    ; export DEF for wiring tracks
    (when doTracksDef
      Command = (sprintf nil
                         (strcat "%s/genTracks"
                                 " --refdef=%s"
                                 " --outfile=%s"
                                 " --track-pitch=%.3f"
                                 " %s")
                         BinRoot
                         (strcat CV->cellName ".def")
                         (strcat CV->cellName "_tracks.def")
                         DefaultWiringPitch
                         (if Proteus "--rotate" ""))
      (printf "%s\n" Command)
      statusTracks = (shell Command)
      )

    ; save
    (dbSave CV)

    ; summarize errors
    errorStr = ""
    (unless statusVerilog (strcat errorStr " Verilog"))
    (unless statusLef (strcat errorStr " LEF"))
    (unless statusDef (strcat errorStr " DEF"))
    (unless statusNonDefault (strcat errorStr "NonDefaultDEF"))
    (unless statusTracks (strcat errorStr "TracksDEF"))
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
  (let (str subcells)
    subcells = nil
    (foreach inst CV->instances
             str = (strcat inst->libName ".wires")
             (when (and inst->libName!=TechLibName
                        (member inst->master->cellView subcells)==nil
                        (or (strncmp inst->cellName str (strlen str))!=0
                            (member inst->cellName includeWiringCellNames)
                            )
                        )
               subcells = (cons inst->master->cellView subcells)
               )
             )
    subcells
    )
  )

; Shortcut for Proteus asynchronous defaults
(defun ProteusExport (@key (CV (geGetEditCellView)))
  (NanoRouteExport ?CV CV ?Proteus t))

; Shortcut for Proteus synchronous defaults
(defun SynchronousExport (@key (CV (geGetEditCellView)))
  (NanoRouteExport ?CV CV ?Synchronous t))

; Write TCL file to run NanoRoute
(defun NanoRouteTcl
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        (bottomMetal 3)          ; bottom metal layer
        (topMetal    7)          ; top metal layer
        (Verify t)               ; run VerifyGeom and VerifyConn at the end
        (SkipCompleteNets t)     ; auto-skip complete nets
        (SaveQuit nil)           ; automatically save and quit NanoRoute
        (strictNondefault t)     ; strict nondefault width/spacing rules
        (AutoStop t)             ; stop routing if too many errors
        (routeGND nil)             ; route GND net
        (routeVdd nil)             ; route Vdd net
        )
  (let (CmdFile file)
    ; write TCL
    CmdFile  = (strcat CV->cellName ".tcl")
    file = (outfile CmdFile)
    (unless file (error "Unable to write %s\n" CmdFile))
    (fprintf file "set PDK_ROOT %s\n"
             (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT"))
    (fprintf file "set TOP \"%s\"\n" CV->cellName)
    (fprintf file "set TOP_LAYER %d\n" topMetal)
    (fprintf file "set BOTTOM_LAYER %d\n" bottomMetal)
    (fprintf file "source $PDK_ROOT/share/Fulcrum/nano/nano.tcl\n")
    (fprintf file "setNanoRouteMode -quiet -routeStrictlyHonorNonDefaultRule %s\n"
             (if strictNondefault "true" "false"))
    (fprintf file "setNanoRouteMode -quiet -drouteAutoStop %s\n"
             (if AutoStop "true" "false"))
    (fprintf file "FulcrumLoad\n")
    (when SkipCompleteNets (fprintf file "FulcrumSkipCompleteNets\n"))
    (unless routeGND (fprintf file "setAttribute -net GND -skip_routing true\n"))
    (unless routeVdd (fprintf file "setAttribute -net Vdd -skip_routing true\n"))
    (fprintf file "FulcrumRoute\n")
    (when Verify (fprintf file "FulcrumVerify\n"))
    (fprintf file "FulcrumSave\n")
    (when SaveQuit (fprintf file "exit\n"))
    (close file)
    )
  t
  )

; launch NanoRoute
(defun NanoRouteRun
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        (Background t)           ; run in foreground or background
        (Ticket "np")            ; QSUB ticket: np, nano
        (Mem 4000)               ; QSUB mem to request
        (Soc (getShellEnvVar "SOC_SCRIPT")) ; which version of Encounter to use
        )
  (let (CmdFile LicenseStr QsubOptions Command defFile status)
    ; files and options
    CmdFile = (strcat CV->cellName ".tcl")
    LicenseStr = "-socel"
    QsubOptions = (sprintf nil "-cwd -now n -l %s=1,mem=%dM,a=lx24-amd64 -N NanoRoute"
                           Ticket Mem)
    defFile = (strcat CV->cellName "_routed.def")

    ; run
    (shell (strcat "rm -rf " defFile))
    Command = (sprintf nil "qrsh %s %s encounter -64 %s -init %s %s"
                       QsubOptions Soc
                       LicenseStr CmdFile (if Background "-nowin &" "-win"))
    (printf "%s\n" Command)
    status = (shell Command)
    (unless status (error "Nanoroute failed\n"))

    ; wait for slow filesystem until def is created
    (or Background (WaitForFile defFile 60))
    )
  )

; write proteus.config
(defun ProteusConfig
  (@key (CV (geGetEditCellView)) ; specify CellView or use edit view
        (bottomMetal 3)          ; bottom metal layer
        (topMetal 7)             ; top metal layer
        )
  (let (CAST_PATH DFII_DIR SPAR_DIR TEMP file)
    ; files and options
    CAST_PATH = (ConfigFileGetValue TheCDSConfigTable "CAST_PATH")
    DFII_DIR = (ConfigFileGetValue TheCDSConfigTable "DFII_DIR")
    SPAR_DIR = "/mnt/fulcrum/local/checkouts/alta/alta/layout/tsmc65/spar"
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")

    ; write config file
    file = (outfile (strcat CV->cellName ".config"))
    (fprintf file "--cast-path=%s\n" CAST_PATH)
    (fprintf file "--dfII-dir=%s\n" DFII_DIR)
    (fprintf file "--spar-dir=%s\n" SPAR_DIR)
    (fprintf file "--cast-dir=%s/cast\n" TEMP)
    (fprintf file "--spec-dir=%s/spec\n" TEMP)
    (fprintf file "--lefdef-dir=.\n")
    (fprintf file "--lve-path=lve:/mnt/fulcrum/alta/lve/lve\n")
    (fprintf file "--output-dir=%s/proteus\n" TEMP)
    (fprintf file "--scratch-dir=\n")
    (fprintf file "--base=%s\n" CV->cellName)
    (fprintf file "--cell=%s\n" CV->cellName)
    (fprintf file "--scan-mode=off\n")
    (fprintf file "--macro-integration=1\n")
    (fprintf file "--bottom-layer=%d\n" bottomMetal)
    (fprintf file "--top-layer=%d\n" topMetal)
    (fprintf file "--task=cast2rtl,rc,clockfree,encounter,summarize\n")
    (close file)
    )
  )

; timing driven routing with Proteus script
(defun ProteusRun
  (@key (CV (geGetEditCellView)) ; specify CellView or use edit view
        (Background t)           ; run in background
        (Mem 4000)               ; qsub memory
        (bottomMetal 3)          ; bottom metal layer
        (topMetal 7)             ; top metal layer
        )
  (let (TEMP defFile Command status)
    ; files and options
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    defFile = (strcat TEMP "/proteus/" (cadr (reverse (parseString CV->cellName ".")))
                      ".qdi.def")

    ; run
    (shell (strcat "rm -rf " defFile))
    (shell (strcat "ln -sf " defFile " " CV->cellName "_routed.def"))
    Command = (sprintf nil "proteus --include=%s.config --mem=%dM%s"
                       CV->cellName Mem (if Background " &" ""))
    (printf "%s\n" Command)
    status = (shell Command)
    (unless status (error "Proteus failed\n"))

    ; wait for slow filesystem until def is created
    (or Background (WaitForFile defFile 60))
    )
  )

; wait for file to appear on filesystem
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

; import from NanoRoute
(defun NanoRouteImport
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        (libName  CV->libName)
        (cellName CV->cellName)
        (viewName "layout")
        (defFile (strcat (geGetEditCellView)->cellName "_routed.def")) ; def input file
        (proteus nil) ; import from Proteus?
        )
  (let (SubcellLibNames Command status ImportCV layoutCV floorplanCV win instmap nodemap)

    ; create library
    (when libName!=CV->libName
      (GDSIIHierMakeLibrary libName (getShellEnvVar "DFII_DIR") TechLibName)
      )

    ; figure out libNames for all exported subcells
    SubcellLibNames = (GetSubcellLibNames defFile)

    ; import def
    status = (ldtrDefReadOA
              defFile ; t_fileName
              libName ; t_libName
              (getShellEnvVar "DFII_DIR") ; [t_libPath libPath]
              cellName    ; [t_cellName cellName]
              viewName    ; [t_viewName viewName]
              TechLibName ; [t_techName techName]
              "abstract_proteus abstract"  ; [t_viewNameList viewNameList]
              (buildString SubcellLibNames) ; [t_masterLibs masterLibs]
              nil ; [ shared nil]
              nil ; [ noRouting nil]
              (strcat defFile ".log") ; [ t_logName logName]
              nil ; [ useCustomVias nil]
              t   ; [ overwrite nil]
              nil ; [ createModHier nil]
              ; [ t_commentChar commentChar]
              ; [ t_templateFileName templateFileName]
              )
    (unless status (error "Unsuccessful defin\n"))
    ImportCV = (dbOpenCellViewByType libName cellName viewName "maskLayout" "a")

    ; unrotate Proteus cells and wires, rename instances and nodes
    (when proteus
      instmap = (PinInstXrefTable)
      nodemap = (PinNodeXrefTable)
      (foreach inst (setof obj ImportCV->instances obj->viewName=="abstract_proteus")
               (ImportProteusInstance CV instmap inst)
               )
      (dbTransformCellView ImportCV 1.0 -90.0)
      )

    ; label pins
    (LabelPins ?CV ImportCV) 

    ; change abstract instances to layout or floorplan view
    (foreach inst (setof obj ImportCV->instances obj->viewName=="abstract")
             layoutCV    = (dbOpenCellViewByType inst->libName inst->cellName "layout")
             floorplanCV = (dbOpenCellViewByType inst->libName inst->cellName "floorplan")
             (cond (layoutCV    inst->master=layoutCV)
                   (floorplanCV inst->master=floorplanCV)
                   )
             )

    ; change window to ImportCV
    win = (hiGetCurrentWindow)
    (cond (win
           (geOpen ?lib libName ?cell cellName ?view viewName
                   ?viewType "maskLayout" ?mode "a" ?window win))
          (t
           (geOpen ?lib libName ?cell cellName ?view viewName
                   ?viewType "maskLayout" ?mode "a"))
          )

    ; execute custom function
    (FinishRouterImport)

    ; save layout
    (dbSave ImportCV)

    ; create floorplan view for Proteus
    (when proteus (MakeProteusFloorplanView ?CV ImportCV))

    ;return layout
    ImportCV
    )
  )

; defaults for importing Proteus cells
(defun NanoProteusImport (defFile libName cellName @key (CV (geGetEditCellView)))
  (NanoRouteImport ?libName libName ?cellName cellName ?defFile defFile ?proteus t ?CV CV)
  )

; HACK: special processing of Proteus instances
(defun ImportProteusInstance (CV map inst)
  (let (newname)
    ; unrotate (swap X and Y)
    (cond (inst->orient=="R0"   inst->orient="MXR90")
          (inst->orient=="R180" inst->orient="MYR90")
          (inst->orient=="MX"   inst->orient="R270")
          (inst->orient=="MY"   inst->orient="R90")
          )
    ; change to layout view
    inst->master = (dbOpenCellViewByType inst->libName inst->cellName "layout")
    ; translate instance name
    newname = (arrayref map inst->name)
    (when newname && inst->name!=newname inst->name=newname)
    inst
    )
  )

; Finish router import, user over-ridable in wires.il
(defun FinishRouterImport ()
  (DeleteKeepout)
  (defun RunAfterImportDef () t)
  (LoadWires)
  (RunAfterImportDef)
  )

; make a floorplan view
(defun MakeProteusFloorplanView (@key (CV (geGetEditCellView)))
  (let (fCV)
    fCV = (dbOpenCellViewByType CV->libName CV->cellName "floorplan" "maskLayout" "w")
    (foreach inst (setof s CV->instances s->libName!=TechLibName && s->name!="tiehilo")
             inst = (dbCopyFig inst fCV (list 0:0 "R0"))
             inst->master = (dbOpenCellViewByType inst->libName inst->cellName "floorplan")
             )
    (when CV->prBoundary
      (dbCopyFig CV->prBoundary fCV (list 0:0 "R0"))
      )
    fCV
    )
  )
