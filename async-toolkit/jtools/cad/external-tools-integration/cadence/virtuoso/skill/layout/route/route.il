; Automated routing using VCAR.  Wastes less time than the old UI.
;
; Copyright 2010 Fulcrum Microsystems.  All rights reserved.

; Export, Route, Import using CCAR
(defun Route
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        (type "mid")             ; routing rul/do file to use from PDK ("mid"/"leaf")
        (viaType "c")            ; via type ("c"/"min")
        (routeGND nil)           ; route GND net
        (routeVdd nil)           ; route Vdd net
        (bottomMetal nil)        ; bottom metal layer to use, or nil to use wires.il
        (topMetal nil)           ; top metal layer to use, or nil to use wires.il
        (SaveQuit nil)           ; automatically save and quit CCAR
        (Background nil)         ; run in foreground or background
        (subcellView "abstract_edit abstract") ; alternate views of subcells to export to CCAR
        (importView "layout_pg") ; view name to import wiring back to
        (conductorDepth 0)       ; sets CCAR conductor depth
        (retry t)                ; automatic retry
        )
  (let (TEMP FulcrumPDKRoot RouterPDK DoFile RouterDoFile SaveQuitStr
             rules options session status tiehilo win cmd)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP" )

    ; make flatten view automatically
    (when (or CV->viewName=="floorplan" CV->viewName=="prelayout")
      CV = (MakeFlatten ?CV CV))

    ; get config settings
    FulcrumPDKRoot = (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
    RouterPDK = (sprintf nil "%s/share/Fulcrum/cell_automation/router" FulcrumPDKRoot)
    DoFile = (sprintf nil "%s/%s.do" RouterPDK type)

    ; load wires.il and set options
    (LoadWires)
    (unless bottomMetal bottomMetal = (if type=="leaf" 1 bundled_bottom_layer))
    (unless topMetal    topMetal    = (if type=="leaf" 3 bundled_top_layer))

    ; write do file
    RouterDoFile = (WriteRouterDoFile CV ?viaType viaType
                                      ?routeGND routeGND ?routeVdd routeVdd
                                      ?bottomMetal bottomMetal
                                      ?topMetal topMetal)

    ; options
    SaveQuit = (or SaveQuit Background)
    rules = (sprintf nil "%s/%s.rul" RouterPDK type)
    SaveQuitStr = (sprintf nil "-do %s/savequit.do" RouterPDK)
    options = (sprintf nil "-guidir %s -noclean -virtuoso -do %s -do %s %s"
                       TEMP RouterDoFile DoFile
                       (if SaveQuit SaveQuitStr ""))
    session = (sprintf nil "%s/%s.ses" TEMP CV->cellName)
    (shell (sprintf nil "rm -f %s" session))

    ; export
    status = (iccExportCellview
              ?layoutLCV (list CV->libName CV->cellName CV->viewName)
              ?background nil
              ?exportDirectory TEMP
              ?rulesFile rules
              ?conductorDepth conductorDepth
              ?keepoutDepth 32
              ?pinConnection "strong"
              ?optionList (list "fullConnectivity" "interLayer")
              ?alternateViews (if subcellView!="" (parseString subcellView " ") nil)
              ?startICC nil)
    (unless status (error "Unable to export to VCAR\n"))

    ; route
    cmd = (sprintf nil "vcar %s/%s.dsn %s %s"
                   TEMP CV->cellName options (if Background "-nog &" ""))
    (printf "%s\n" cmd)
    status = nil
    (while !status
      status = (shell cmd)
      (unless status || retry (error "VCAR failed.  Inspect *.did file.\n"))
      (unless status
        (printf "Unable to run VCAR, retrying...\n")
        (sleep 30)
        )
      )

    ; import from VCAR
    (cond (Background (printf "Running in background, finish with RouteImport\n"))
          (t CV = (RouteImport ?CV CV ?importView importView))
          )
    CV
    )
  )

; Import from CCAR
(defun RouteImport
  (@key (CV (geGetEditCellView))
        (importView "layout_pg")
        )
  (let (TEMP session status win)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP" )
    session = (sprintf nil "%s/%s.ses" TEMP CV->cellName)

    ; copy flatten view to importView
    (when (and importView!="" CV->viewName!=importView)
      CV = (dbCopyCellView CV CV->libName CV->cellName importView nil nil t))

    ; import from CCAR
    status = (iccImportCellview
              ?layoutLCV (list CV->libName CV->cellName CV->viewName)
              ?iccFile session
              ?sectionList (list "routes")
              ?background nil)
    (unless status (error "Unable to import from VCAR\n"))

    ; change to importView
    win = (hiGetCurrentWindow)
    (unless win (error "Can't change to view %s\n" importView))
    (geOpen ?window win
            ?lib  CV->libName
            ?cell CV->cellName
            ?view CV->viewName
            ?viewType "maskLayout"
            ?mode "a")

    ; execute custom function
    (FinishRouterImport)

    ; save layout_pg
    (dbSave CV)

    ; create layout view
    (MakeLayout ?CV CV)

    ;return layout_pg
    CV
    )
  )

; shortcut for leaf cells
(defun DeprecatedRouteLeaf 
  (@key (CV (geGetEditCellView))
        (type "custom_leaf")
        (viaType "c")
        (importView "layout")
        )
  (Route ?CV CV
         ?type type
         ?viaType viaType
         ?bottomMetal 1
         ?topMetal 3
         ?routeGND t
         ?routeVdd t
         ?importView importView
         )
  )

; write FQCN.do for VCAR
(defun WriteRouterDoFile (CV @key (viaType "c")
                             (routeGND nil) (routeVdd nil)
                             (bottomMetal 3) (topMetal 7)
                             (maxheap (nrGetMaxHeapSize)))
  (let (TEMP DoFile SkillFile Command p_out wireType width space extension netProps
             status)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP" )
    DoFile = (sprintf nil "%s/%s.do" TEMP CV->cellName)

    ; get information from CAST
    SkillFile = (sprintf nil "%s/%s_routing.skill" TEMP CV->cellName )
    Command = (sprintf nil
               "%s/cast2def --cast-path=%s --cell=%s --outfile=%s --format=skill --cadence-name --max-heap-size=%dM > %s"
               (PackageGetBinRoot)
               (ConfigFileGetValue TheCDSConfigTable "CAST_PATH")
               CV->cellName
               SkillFile
               maxheap
               (sprintf nil "%s/%s_routing.log" TEMP CV->cellName)
               )
    (printf "%s\n" Command)
    status = (shell Command)
    (unless status (error "cast2def failed\n"))
    (load SkillFile)

    ; write do file
    p_out = (outfile DoFile)
    (unless p_out (error (sprintf( nil "Unable to write %s\n" DoFile))))

    ; emit width/clearance/extension directives
    (fprintf p_out "# wiring directives\n")
    (fprintf p_out "define (class RESET_3W3S)\n") ; make sure it exists
    (foreach nets NondefaultRoutingDirective
      wireType = (car nets)
      nets     = (cdr nets)
      netProps = (arrayref NondefaultRouting wireType )
      nets = (setof net nets (GetProp (dbFindNetByName CV net) "BundledNet" nil)==nil)

      ; define nets
      (fprintf p_out "define (class %s" wireType)
      (foreach net nets (fprintf p_out " %s" net))
      (fprintf p_out ")\n")
 
      ; define rules
      width     = (car netProps)
      space     = (cadr netProps)
      extension = (caddr netProps)
      (fprintf p_out "rule class %s (width %f)\n" wireType width)
      (fprintf p_out "rule class %s (clearance %f (type wire_wire))\n" wireType space)
      (fprintf p_out "rule class %s (clearance %f (type area_wire))\n" wireType space)
      (fprintf p_out "rule class %s (wire_extension %f)\n" wireType extension)
      )
    (fprintf p_out "\n")

    ; select metal and vias
    (fprintf p_out "# select metal and vias\n")
    (fprintf p_out "unselect all layers\n")
    (fprintf p_out "unselect all vias\n")
    (for layer bottomMetal topMetal
         (fprintf p_out "select layer M%d\n" layer)
         (when layer>=3
           (fprintf p_out "select via M%d_M%d%s\n" layer layer-1 viaType)
           )
         )
    (fprintf p_out "\n")

    ; optionally protect Vdd/GND
    (unless routeGND
      (fprintf p_out "# dont route power\n")
      (fprintf p_out "fix net GND\n\n")
      )
    (unless routeVdd
      (fprintf p_out "# dont route power\n")
      (fprintf p_out "fix net Vdd\n\n")
      )

    ; end of generated do file
    (close p_out)
    DoFile
    )
  )
