; Export Oasis for current cell with only cell name conversion (unlike for lve)
(defun ExportOAS (@key (CV (geGetEditCellView))
                       (directory nil)
                       (oasFile nil)
                       (layerMapFile nil)
                       (enableColoring t)
                       (flattenVias nil)
                       (dbuPerUU (round 1/MfgGrid)))
  (let (TEMP PDK_DIR Command status cellMapFile oasName CID)
    (unless CV->objType=="cellView" (error "CV not valid"))
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    (unless directory directory = (strcat TEMP "/oas/" CV->cellName))
    PDK_DIR = (strcat (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT") "/share/Fulcrum")
    CID = (NameStartRename "cell" "cadence" "gds2")
    oasName = (if (dbGetPropByName CV "CopyExact")
                  (dbGetPropByName CV "CopyExact")->value
                  (NameDoRename CID CV->cellName)
                  )
    (NameStopRename CID)
    (unless oasFile (sprintf oasFile "%s/%s.oas" directory oasName))
    (unless layerMapFile (sprintf layerMapFile "%s/stream/strmout.layermap" PDK_DIR))
    (shell (strcat "mkdir -p " directory))
    cellMapFile = (sprintf nil "%s/%s.cellmap" directory oasName)
    (WriteCellmap cellMapFile ?CV CV) ; write cellmap with CopyExact or GDS2 names

    ; strmout command line arguments
    Command = (sprintf nil
                       (strcat "oasisout"
                               (if flattenVias " -flattenVias" "")
                               " -library '%s'"
                               " -topCell '%s'"
                               " -view '%s'"
                               " -runDir '%s'"
                               " -outputDir '%s'"
                               " -layerMap '%s'"
                               " -propMap '%s/stream/prop.map'"
                               " -objectMap '%s/stream/objectmap'"
                               (if enableColoring " -enableColoring" "")
                               (if dbuPerUU (sprintf nil " -dbuPerUU %d" dbuPerUU) "")
                               " -cellMap '%s'"
                               " -logFile '%s/%s.log'"
                               " -oasisFile '%s'")
                       CV->libName CV->cellName CV->viewName
                       directory directory layerMapFile
                       PDK_DIR PDK_DIR cellMapFile
                       directory oasName
                       oasFile)
    status = (shell Command)
    (printf "%s : %s\n" (if status "SUCCESS" "FAIL") oasFile)
    status
    )
  )

