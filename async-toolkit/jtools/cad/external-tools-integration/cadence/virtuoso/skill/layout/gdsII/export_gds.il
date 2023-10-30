; Export GDS for current cell with only cell name conversion (now also used for lve)
(defun ExportGDS (@key (CV (geGetEditCellView))
                       (directory nil)
                       (gdsFile nil)
                       (layerMapFile nil)
                       (renameTop t) ; CopyExact applies to top cell too
                       (flattenVias nil)
                       (dbuPerUU (round 1/MfgGrid)))
  (let (TEMP PDK_DIR Command status cellMapFile gdsName)
    (unless CV->objType=="cellView" (error "CV not valid"))
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    (unless directory directory = (strcat TEMP "/gds/" CV->cellName))
    PDK_DIR = (strcat (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT") "/share/Fulcrum")
    (unless layerMapFile (sprintf layerMapFile "%s/stream/strmout.layermap" PDK_DIR))
    (shell (strcat "mkdir -p " directory))
    cellMapFile = (sprintf nil "%s/cellmap" directory)
    gdsName = (WriteCellmap cellMapFile ?CV CV ?renameTop renameTop) ; cellmap with CopyExact or GDS2 names
    (unless gdsFile (sprintf gdsFile "%s/%s.gds" directory gdsName))

    ; strmout command line arguments
    Command = (sprintf nil
                       (strcat "strmout"
                               (if flattenVias " -flattenVias" "")
                               " -library '%s'"
                               " -topCell '%s'"
                               " -view '%s'"
                               " -runDir '%s'"
                               " -outputDir '%s'"
                               " -layerMap '%s'"
                               " -propMap '%s/stream/prop.map'"
                               " -objectMap '%s/stream/objectmap'"
                               " -cellMap '%s'"
                               " -logFile '%s/%s.log'"
                               " -enableColoring"
                               (if dbuPerUU (sprintf nil " -dbuPerUU %d" dbuPerUU) "")
                               " -strmFile '%s'")
                       CV->libName CV->cellName CV->viewName
                       directory directory layerMapFile
                       PDK_DIR PDK_DIR cellMapFile
                       directory gdsName
                       gdsFile)
    status = (shell Command)
    (printf "%s : %s\n" (if status "SUCCESS" "FAIL") gdsFile)
    status
    )
  )

; Export GDS using alternate layerMapFile suitable for plotting
(defun PlotGDS (@key (CV (geGetEditCellView)) (gdsFile nil))
  (let (PDK_DIR)
    PDK_DIR = (strcat (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
                      "/share/Fulcrum")
    (ExportGDS ?CV CV ?gdsFile gdsFile
               ?layerMapFile (sprintf nil "%s/plot/gds2plot_layer_map.rules" PDK_DIR))
    )
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
