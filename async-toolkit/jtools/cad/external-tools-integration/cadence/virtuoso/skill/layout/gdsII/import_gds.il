; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Simpler flow to import GDS to DFII.  Streams in GDS to a temporary
; library, copies all cells to the desired destination libName,
; appends a prefix and suffix to create CAST-style cellName's, and
; updates the instance masters.  Then applies a user-defined
; postprocessing function to all cells, such as to rename instances
; and nodes.

; Import a GDS file and rename all subcells to CAST conventions
(defun ImportGDS (gdsFileName libName cellName
                              @key
                              (prefix nil)
                              (suffix ".0")
                              (mapCellName nil) ; lambda on cellName to choose libName/cellName
                              (action nil)
                              (recurse t)
                              (setCopyExact nil) ; lamda on CV to decide when to set CopyExact property
                              )
  (let (TEMP PDK_DIR temp_lib cmd status CV)
    ; stream in GDS into temp_lib library
    PDK_DIR = (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
    temp_lib = (sprintf nil "import_gds_%s" cellName)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    (GDSIIHierMakeLibrary temp_lib TEMP TechLibName)
    cmd = (strcat "strmin \\\n"
                  "-attachTechFileOfLib \"" TechLibName "\" \\\n"
                  "-layerMap \"" PDK_DIR "/share/Fulcrum/stream/strmin.layermap\" \\\n"
                  "-propMap \"" PDK_DIR "/share/Fulcrum/stream/prop.map\" \\\n"
                  "-objectMap \"" PDK_DIR "/share/Fulcrum/stream/objectmap\" \\\n"
                  "-enableColoring \\\n"
                  "-library \"" temp_lib "\" \\\n"
                  "-strmFile \"" gdsFileName "\"")
    status = (shell cmd)
    (unless status (error "strmin failed\n"))
    (ddUpdateLibList)

    ; open imported top CV
    CV = (dbOpenCellViewByType temp_lib cellName "layout")
    (unless CV (error "unable to open %s %s layout\n" temp_lib cellName))

    ; set CopyExact property
    (when setCopyExact
      (FindSubcells ?CV CV ?filter setCopyExact
                    ?action (lambda (CV) (dbReplaceProp CV "CopyExact" "string" CV->cellName) (dbSave CV)))
      )

    ; copy cells to destination library
    (unless prefix prefix = (strcat libName "."))
    CV = (CopyToNewLib libName ?CV CV ?prefix prefix ?suffix suffix ?mapCellName mapCellName ?recurse recurse)

    ; delete temp_lib library
    (ddDeleteObj (ddGetObj temp_lib))

    ; FindSubcells to recursively rename stuff
    (when action (FindSubcells ?action action ?CV CV))

    ; more processing of top level cell
    (dbReplaceProp CV "CastCell" "boolean" t) ; top-level only
    (LabelsToPins ?CV CV) ; create pins from labels on pinPurpose rectangles
    (DeleteLabels ?CV CV ?keepLPPs (list PWellIsoLPP NWellLPP))
    (LabelPins ?CV CV)
    (dbSave CV)

    ; return top-level CV
    CV
    )
  )

; Recursively copy a cell hierarchy to a new library.  Recurses only
; to instances mapped to the same libName as the target to avoid
; renaming technology cells like vias.  Adds optional prefix and
; suffix to cellNames.  Or use lambda function mapCellName to map the
; cellName to a new libName and cellName pair.  If mapCellname returns
; nil for a subcell, that subcell is presumed to be a wiring cell and
; is flattened.
(defun CopyToNewLib (libName @key (CV (geGetEditCellView))
                             (visited (makeTable "visited" nil))
                             (prefix "")
                             (suffix "")
                             (mapCellName nil) ; lambda on cellName to return (list libName cellName)
                             (recurse t) ; recursively import subcells as well
                             (top t)     ; is this the top cell
                             )
  (let (newCV lib_cell libN cellN master)
    newCV=visited[CV]
    (unless newCV
      (cond (mapCellName
             lib_cell = (apply mapCellName (list CV->cellName))
             libN  = (car  lib_cell)
             cellN = (cadr lib_cell)
             )
            (t
             libN  = libName
             cellN = (strcat prefix CV->cellName suffix)
             )
            )
      (cond (libN==libName && (recurse || top)
             (printf "CopyToNewLib: copy to %s %s %s\n" libN cellN CV->viewName)
             (UpdateFloorplanCreateLib libN) ; create destination library if necessary
             newCV=(betterCopyCellView CV libN cellN CV->viewName nil nil t)
             (unless newCV (error "CopyToNewLib unable to write %s %s %s\n" libN cellN CV->viewName))
             visited[CV]=newCV
             (foreach inst newCV->instances
                      master=(CopyToNewLib libName
                                           ?CV inst->master ?visited visited
                                           ?prefix prefix ?suffix suffix
                                           ?mapCellName mapCellName
                                           ?recurse recurse ?top nil)
                      (if master inst->master=master (dbFlattenInst inst 32)) ; remap or flatten
                      )
             (dbSave newCV)
             )
            (libN && cellN
             (printf "CopyToNewLib: refer to %s %s %s\n" libN cellN CV->viewName)
             newCV=(ReadCV libN cellN CV->viewName)
             (unless newCV (error "CopyToNewLib unable to read %s %s %s\n" libN cellN CV->viewName))
             visited[CV]=newCV
             )
            (t
             (printf "CopyToNewLib: flatten unmapped %s\n" CV->cellName)
             )
            )
      )
    newCV
    )
  )

; Use labels to create pins and attach net information
(defun LabelsToPins (@key (CV (geGetEditCellView)))
  (let (n net overlaps)
    n=0
    (foreach label CV->shapes
             (when label->objType=="label"
                   overlaps = (dbGetOverlaps CV (list label->xy label->xy) label->lpp 0)
                   net = (dbMakeNet CV label->theLabel)
                   (foreach s overlaps
                            (when s->objType=="rect" || s->objType=="path"
                                  s->net=net
                                  (when label->purpose=="pin" (dbCreatePin net s) n++)
                                  )
                            )
                   )
             )
    n
    )
  )
