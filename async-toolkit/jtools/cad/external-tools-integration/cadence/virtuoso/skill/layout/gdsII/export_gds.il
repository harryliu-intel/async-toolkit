; Export GDS for current cell without all the name conversion we normally need
(defun ExportGDS (@key (CV (geGetEditCellView)) (gdsFile nil) (layerMapFile nil))
  (let (TEMP PDK_DIR Command status cellMapFile fmap)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    PDK_DIR = (strcat (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
                      "/share/Fulcrum")
    (unless gdsFile (sprintf gdsFile "%s/gds/%s.gds" TEMP CV->cellName))
    (unless layerMapFile (sprintf layerMapFile "%s/stream/strmout.layermap" PDK_DIR))
    (shell (strcat "mkdir -p " TEMP "/gds"))

    ; write cellmap to rename CopyExact cells back to their original name
    cellMapFile = (sprintf nil "%s/gds/%s.cellmap" TEMP CV->cellName)
    fmap = (outfile cellMapFile)
    (FindSubcells ?filter (lambda (CV) (dbGetPropByName CV "CopyExact")!=nil)
                  ?action (lambda (CV) (fprintf fmap "%s %s %s %s\n"
                                                CV->libName CV->cellName CV->viewName
                                                (dbGetPropByName CV "CopyExact")->value))
                  )
    (close fmap)

    ; strmout command line arguments
    Command = (sprintf nil
                       (strcat "strmout"
                               " -library '%s'"
                               " -topCell '%s'"
                               " -view '%s'"
                               " -runDir %s/gds"
                               " -outputDir %s/gds"
                               " -layerMap %s"
                               " -propMap %s/stream/prop.map"
                               " -objectMap %s/stream/objectmap"
                               " -cellMap %s"
                               " -logFile %s/gds/%s.log"
                               " -strmFile %s")
                       CV->libName CV->cellName CV->viewName
                       TEMP TEMP layerMapFile
                       PDK_DIR PDK_DIR cellMapFile
                       TEMP CV->cellName
                       gdsFile)
    status = (shell Command)
    (printf (if status "SUCCESS\n" "FAIL\n"))
    gdsFile
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
