; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Export Oasis for current cell with only cell name conversion (unlike for lve)
(defun ExportOAS (@key (CV (geGetEditCellView))
                       (directory nil)
                       (oasFile nil)
                       (layerMapFile nil)
                       (targetNamespace "gds2")
                       (renameTop t) ; CopyExact applies to top cell too
                       (flattenVias nil)
                       (dbuPerUU (round 1/MfgGrid)))
  (let (TEMP PDK_DIR Command status cellMapFile gdsName)
    (unless CV->objType=="cellView" (error "CV not valid"))
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    (unless directory directory = (strcat TEMP "/oas/" CV->cellName))
    PDK_DIR = (strcat (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT") "/share/Fulcrum")
    (unless layerMapFile (sprintf layerMapFile "%s/stream/strmout.layermap" PDK_DIR))
    (shell (strcat "mkdir -p " directory))
    cellMapFile = (sprintf nil "%s/cellmap" directory)
    gdsName = (WriteCellmap cellMapFile ?CV CV ?renameTop renameTop ?targetNamespace targetNamespace)
    (unless oasFile (sprintf oasFile "%s/%s.oas" directory gdsName))

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
                               " -enableColoring"
                               (if dbuPerUU (sprintf nil " -dbuPerUU %d" dbuPerUU) "")
                               " -cellMap '%s'"
                               " -logFile '%s/%s.log'"
                               " -oasisFile '%s'")
                       CV->libName CV->cellName CV->viewName
                       directory directory layerMapFile
                       PDK_DIR PDK_DIR cellMapFile
                       directory gdsName
                       oasFile)
    status = (shell Command)
    (printf "%s : %s\n" (if status "SUCCESS" "FAIL") oasFile)
    status
    )
  )

; Write OAS for a list of cell-views
(defun ExportCellsOAS (directory cells)
  (let (status (statusOas t))
    (foreach cell cells
             status = (ExportOAS ?CV cell ?directory directory)
             statusOas = statusOas && status
             )
    statusOas
    )
  )
