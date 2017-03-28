; Export GDS for current cell with only cell name conversion (unlike for lve)
(defun ExportGDS (@key (CV (geGetEditCellView))
                       (directory nil)
                       (gdsFile nil)
                       (layerMapFile nil)
                       (renameCell (lambda (cellName) (translateName "cell" "cadence" "gds2" cellName)))
                       (flattenVias t))
  (let (TEMP PDK_DIR Command status cellMapFile fmap gdsName name used)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    (unless directory directory = (strcat TEMP "/gds/" CV->cellName))
    PDK_DIR = (strcat (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
                      "/share/Fulcrum")
    gdsName = (CellNameGDS ?CV CV ?renameCell renameCell)
    (unless gdsFile (sprintf gdsFile "%s/%s.gds" directory gdsName))
    (unless layerMapFile (sprintf layerMapFile "%s/stream/strmout.layermap" PDK_DIR))
    (shell (strcat "mkdir -p " directory))

    ; Write cellmap to rename cells.  CopyExact cells get renamed to
    ; their original name as saved in the CopyExact property.  Other
    ; cells apply renameCell lambda function.
    cellMapFile = (sprintf nil "%s/%s.cellmap" directory gdsName)
    fmap = (outfile cellMapFile)
    used = (makeTable "used" nil)
    (foreach subCV (FindSubcells ?CV CV)
             name = (CellNameGDS ?CV subCV ?renameCell renameCell)
             (when used[name] name = (strcat name "_" subCV->viewName)) ; disambiguate
             (fprintf fmap "%s %s %s %s\n" subCV->libName subCV->cellName subCV->viewName name)
             used[name]=t
             )
    (close fmap)
    
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

; map a CV to its GDS name
(defun CellNameGDS (@key (CV (geGetEditCellView))
                         (renameCell (lambda (cellName) (translateName "cell" "cadence" "gds2" cellName))))
  (cond ((dbGetPropByName CV "CopyExact")!=nil (dbGetPropByName CV "CopyExact")->value)
        (renameCell!=nil                       (apply renameCell (list CV->cellName)))
        (t                                     CV->cellName)
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
