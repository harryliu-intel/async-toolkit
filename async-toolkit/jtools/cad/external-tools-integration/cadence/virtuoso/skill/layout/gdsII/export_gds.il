; Export GDS for current cell without all the name conversion we normally need
(defun ExportGDS (@key (CV (geGetEditCellView)) (gdsFile nil) (layerMapFile nil))
  (let (TEMP PDK_DIR Command status)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    PDK_DIR = (strcat (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
                      "/share/Fulcrum")
    (unless gdsFile (sprintf gdsFile "%s/gds/%s.gds" TEMP CV->cellName))
    (unless layerMapFile (sprintf layerMapFile "%s/stream/strmout.layermap" PDK_DIR))
    (shell (strcat "mkdir -p " TEMP "/gds"))
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
                               " -logFile %s/gds/%s.log"
                               " -strmFile %s")
                       CV->libName CV->cellName CV->viewName
                       TEMP TEMP layerMapFile
                       PDK_DIR PDK_DIR
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
