; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Run QFILL to add fill and color shapes to a cell
(defun DoFill (@key (CV (geGetEditCellView)) (botLayer "m0") (topLayer "m10")
                    (layers nil) (delete t))
  (let (TEMP WDIR ExportCV offset all_layers dir cmd CID oasName oasFile qCV
             done offset)
    ; pick list of layers to fill
    all_layers = (list "cell"
                       "end_check_grid"
                       "diffcon"
                       "poly" "fti"
                       "vct" "vcg"
                       "m0" "v0"
                       "m1" "v1"
                       "m2" "v2"
                       "m3" "v3"
                       "m4" "v4"
                       "m5" "v5"
                       "m6" "v6"
                       "m7" "v7"
                       "m8" "v8"
                       "m9" "v9"
                       "m10" "v10"
                       "m11" "v11"
                       "m12" "v12"
                       "m13" "v13"
                       "m14" "v14"
                       "m15" "v15")
    (unless (member botLayer all_layers) (error "botLayer not valid"))
    (unless (member topLayer all_layers) (error "topLayer not valid"))
    (when botLayer all_layers = (member botLayer all_layers))
    
    (unless layers
      done = nil
      (foreach layer all_layers
               (unless done
                 layers = (append layers (list layer))
                 done = layer==topLayer
                 )
               )
      )

    ; delete previous qfill shapes
    (when delete (DeleteAutoGen ?CV CV ?propValue "qfill"))

    ; copy to temp view and offset according to alignment properties
    offset = (cadr (GetCellAlignment ?CV CV))
    ExportCV = (betterCopyCellView CV CV->libName CV->cellName "qfill" nil nil t)
    (OffsetEverything offset ?CV ExportCV)
    (dbSave ExportCV)

    ; export OAS
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR = (strcat TEMP "/oas/" CV->cellName)
    CID = (NameStartRename "cell" "cadence" "gds2")
    oasName = (NameDoRename CID CV->cellName)
    (NameStopRename CID)
    (ExportOAS ?CV ExportCV ?renameTop nil)
    (ddDeleteObj (ddGetObj ExportCV->libName ExportCV->cellName ExportCV->viewName))
    
    ; run qfill
    cmd = (sprintf nil "cd %s; qfill run_fill --layers %s --input %s.oas"
                   WDIR (buildString layers " ") oasName)
    (printf "\n%s\n" cmd)
    (shell cmd)

    ; import metal_via_fill
    oasFile = (sprintf nil "%s/qfill_work_run_fill/qfill_%s_%s_s_custom_merged_design/%s.oas"
                       WDIR oasName TechLibName oasName)
    qCV = (ImportOAS oasFile CV->libName (strcat oasName "_fill") "qfill")
    (CopyQfillShapes CV qCV offset)
    (foreach suffix (list "_fill" "_front_fill")
             (ddDeleteObj (ddGetObj CV->libName (strcat oasName suffix) "qfill"))
             )
    (dbSave CV)
    )
  CV
  )

; Copy shapes from qfill import and set autogen property
(defun CopyQfillShapes (CV qCV offset)
  (let (color)
    (foreach inst qCV->instances
           (dbFlattenInst inst 1 t)
           )
    (foreach shape qCV->shapes
           shape = (dbCopyShape shape CV (list -(car offset):-(cadr offset) "R0"))
           (dbReplaceProp shape "autogen" "string" "qfill")
           )
    )
  t
  )


; Block fill in parent cells
(defun BlockFill (topMetal @key (CV (geGetEditCellView)))
  (for n 0 topMetal   (dbCreateLayerBlockage CV Metal[n] "fill" CV->prBoundary->points))
  (for n 0 topMetal-1 (dbCreateLayerBlockage CV Via[n]   "fill" CV->prBoundary->points))
  t
  )

; Delete and redo fill
(defun RedoFill (topMetal @key (CV (geGetEditCellView)))
  (cdsp4edit ?CV CV)
  (foreach s CV->blockages (when s->type=="fill" (dbDeleteObject s)))
  (foreach s CV->shapes (when s->purpose=="fill" || s->purpose=="floatFill" (dbDeleteObject s)))
  (DoFill ?botLayer "m0" ?topLayer Metal[topMetal] ?CV CV)
  (BlockFill topMetal ?CV CV)
  (dbSave CV)
  CV
  )
