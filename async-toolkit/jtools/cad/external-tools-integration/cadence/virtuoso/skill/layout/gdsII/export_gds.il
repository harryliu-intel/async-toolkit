; Export GDS for current cell without all the name conversion we normally need
(defun ExportGDS (@key (CV (geGetEditCellView)) (gdsFile nil) (layerMapFile nil))
  (let (TEMP Command status)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    (unless gdsFile (sprintf gdsFile "%s/gds/%s.gds" TEMP CV->cellName))
    (unless layerMapFile (sprintf layerMapFile "$PDK_DIR/stream/strmout.layermap"))
    (shell (strcat "mkdir -p " TEMP "/gds"))
    Command = (sprintf nil
                       (strcat "strmout"
                               " -library '%s'"
                               " -topCell '%s'"
                               " -view '%s'"
                               " -runDir %s/gds"
                               " -outputDir %s/gds"
                               " -layerMap %s"
                               " -propMap $PDK_DIR/stream/prop.map"
                               " -objectMap $PDK_DIR/stream/objectmap"
                               " -logFile %s/gds/%s.log"
                               " -strmFile %s")
                       CV->libName CV->cellName CV->viewName
                       TEMP TEMP layerMapFile
                       TEMP CV->cellName
                       gdsFile)
    status = (shell Command)
    (printf (if status "SUCCESS\n" "FAIL\n"))
    gdsFile
    )
  )

; Export GDS using alternate layerMapFile suitable for plotting
(defun PlotGDS (@key (CV (geGetEditCellView)) (gdsFile nil))
  (ExportGDS ?CV CV ?gdsFile gdsFile ?layerMapFile "$PDK_DIR/plot/gds2plot_layer_map.rules")
  )
