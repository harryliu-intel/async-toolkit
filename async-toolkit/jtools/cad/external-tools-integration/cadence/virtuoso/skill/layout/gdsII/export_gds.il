; Export GDS for current cell without all the name conversion we normally need
(defun ExportGDS (@key (CV (geGetEditCellView)) (gdsFile nil) (layerMapFile nil)
                       (renameCell nil) (flattenVias nil))
  (let (TEMP WDIR PDK_DIR Command status cellMapFile fmap)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR = (strcat TEMP "/gds/" CV->cellName)
    PDK_DIR = (strcat (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
                      "/share/Fulcrum")
    (unless gdsFile (sprintf gdsFile "%s/%s.gds" WDIR CV->cellName))
    (unless layerMapFile (sprintf layerMapFile "%s/stream/strmout.layermap" PDK_DIR))
    (shell (strcat "mkdir -p " WDIR))

    ; Write cellmap to rename cells.  CopyExact cells get renamed to
    ; their original name as saved in the CopyExact property.  Other
    ; cells apply renameCell lambda function if it is defined.  If
    ; none it defaults to turning all special characters to "_".
    cellMapFile = (sprintf nil "%s/%s.cellmap" WDIR CV->cellName)
    fmap = (outfile cellMapFile)
    (FindSubcells ?CV CV
                  ?action (lambda (CV)
                            (cond ((dbGetPropByName CV "CopyExact")!=nil
                                   (fprintf fmap "%s %s %s %s\n"
                                            CV->libName CV->cellName CV->viewName
                                            (dbGetPropByName CV "CopyExact")->value)
                                   )
                                  (renameCell!=nil
                                   (fprintf fmap "%s %s %s %s\n"
                                            CV->libName CV->cellName CV->viewName
                                            (apply renameCell (list CV->cellName)))
                                   )
                                  )
                            )
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
                       WDIR WDIR layerMapFile
                       PDK_DIR PDK_DIR cellMapFile
                       WDIR CV->cellName
                       gdsFile)
    status = (shell Command)
    (printf (if status "SUCCESS\n" "FAIL\n"))
    gdsFile
    )
  )

; must match sw/cad/rdt/rdt_setup/sed.dfII2icc
;
; s/_/_U_/g
; s/-L/_L_/g
; s/-R/_R_/g
; s/\([_a-zA-Z][_a-zA-Z0-9]*\)\./\1_D_/g
renameCellForRDT =
(lambda (name)
  name = (pcreReplace (pcreCompile "_")  name "_U_" 0)
  name = (pcreReplace (pcreCompile "-L") name "_L_" 0)
  name = (pcreReplace (pcreCompile "-R") name "_R_" 0)
  ; is this over simplified?  Won't handle floating point metaparams.
  name = (pcreReplace (pcreCompile "\\.") name "_D_" 0)
  )

; Export GDS using alternate layerMapFile suitable for plotting
(defun PlotGDS (@key (CV (geGetEditCellView)) (gdsFile nil))
  (let (PDK_DIR)
    PDK_DIR = (strcat (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
                      "/share/Fulcrum")
    (ExportGDS ?CV CV ?gdsFile gdsFile
               ?renameCell nil
               ?layerMapFile (sprintf nil "%s/plot/gds2plot_layer_map.rules" PDK_DIR))
    )
  )
