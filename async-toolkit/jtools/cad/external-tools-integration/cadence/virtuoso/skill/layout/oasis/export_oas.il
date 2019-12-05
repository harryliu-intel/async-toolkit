; Export GDS for current cell with only cell name conversion (unlike for lve)
(defun ExportOAS (@key (CV (geGetEditCellView))
                       (directory nil)
                       (oasFile nil)
                       (layerMapFile nil)
                       (flattenVias nil))
  (let (TEMP PDK_DIR Command status cellMapFile fmap oasName name used CID)
    (unless CV->objType=="cellView" (error "CV not valid"))
    CID = (NameStartRename "cell" "cadence" "oas2")
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    (unless directory directory = (strcat TEMP "/oas/" CV->cellName))
    PDK_DIR = (strcat (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT") "/share/Fulcrum")
    oasName = (if (dbGetPropByName CV "CopyExact")
                  (dbGetPropByName CV "CopyExact")->value
                  (NameDoRename CID CV->cellName)
                  )
    (unless oasFile (sprintf oasFile "%s/%s.oas" directory oasName))
    (unless layerMapFile (sprintf layerMapFile "%s/stream/strmout.layermap" PDK_DIR))
    (shell (strcat "mkdir -p " directory))

    ; Write cellmap to rename cells.  CopyExact cells get renamed to
    ; their original name as saved in the CopyExact property.  Other
    ; cells get OAS2 names.
    cellMapFile = (sprintf nil "%s/%s.cellmap" directory oasName)
    fmap = (outfile cellMapFile)
    used = (makeTable "used" nil)
    (foreach subCV (FindSubcells ?CV CV)
             name = (if (dbGetPropByName subCV "CopyExact")
                        (dbGetPropByName subCV "CopyExact")->value
                        (NameDoRename CID subCV->cellName)
                        )
             (when used[name] name = (strcat name "_" subCV->viewName)) ; disambiguate
             (fprintf fmap "%s %s %s %s\n" subCV->libName subCV->cellName subCV->viewName name)
             used[name]=t
             )
    (close fmap)
    (ipcKillProcess CID)

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

