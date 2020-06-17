; Run QFILL to add fill and color shapes to a cell
(defun DoFill (@key (CV (geGetEditCellView)) (botLayer nil) (topLayer "m8"))
  (let (TEMP WDIR ExportCV offset all_layers layers dir cmd CID oasName oasFile qCV)

    ; pick list of layers to fill
    all_layers = (list "cell" "end_check_grid" "diffcon" "poly" "fti"
                       "vcg" "vct"
                       "m0" "v0"
                       "m1" "v1" "via1_signal_color"
                       "m2" "v2" "via2_signal_color"
                       "m3" "v3" "via3_signal_color"
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
    (when botLayer all_layers = (member botLayer all_layers))
    layers = nil
    done = nil
    (foreach layer all_layers
             (unless done layers = (cons layer layers)
                     done = layer==topLayer
                     )
             )

    ; delete previous qfill shapes
    (DeleteAutoGen ?CV CV ?propValue "qfill")

    ; copy to temp view and offset accordring to alignment properties
    offset = (cadr (GetCellAlignment ?CV CV))
    ExportCV = (betterCopyCellView CV CV->libName CV->cellName "qfill" nil nil t)
    (OffsetEverything -(car offset):-(cadr offset) ?CV ExportCV)
    (dbSave ExportCV)

    ; export OAS
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR = (strcat TEMP "/oas/" CV->cellName)
    CID = (NameStartRename "cell" "cadence" "gds2")
    oasName = (NameDoRename CID CV->cellName)
    (NameStopRename CID)
    (ExportOAS ?CV ExportCV)

    ; run qfill
    cmd = (sprintf nil "cd %s; qfill run_fill --process 1276.31 --layers %s --input_layout %s.oas"
                   WDIR (buildString layers " ") oasName)
    (printf "%s\n" cmd)
    (shell cmd)

    ; import metal_via_fill
    oasFile = (sprintf nil "%s/qfill_work/qfill_%s_1276.31_s_asic_g1m_240H_50p_run_metal_via_fill/qfill_run_metal_via_fill_output/%s_metal_via_fill.oas" WDIR oasName oasName)
    qCV = (ImportOAS oasFile CV->libName (strcat oasName "_metal_via_fill") "qfill")
    (CopyQfillShapes CV qCV offset)

    ; import signal_via_color
    (when (member "via1_signal_color" layers )!=nil ||
          (member "via2_signal_color" layers )!=nil ||
          (member "via3_signal_color" layers )!=nil
          oasFile = (sprintf nil "%s/qfill_work/qfill_%s_1276.31_s_asic_g1m_240H_50p_run_via_coloring/qfill_run_via_coloring_output/%s_signal_via_color.oas" WDIR oasName oasName)
          qCV = (ImportOAS oasFile CV->libName (strcat oasName "_signal_via_color") "qfill")
          (CopyQfillShapes CV qCV offset)
          )

    )
  CV
  )

; Copy shapes from qfill import and set autogen property
(defun CopyQfillShapes (CV qCV offset)
  (foreach shape qCV->shapes
           shape = (dbCopyShape shape CV (list offset "R0"))
           (dbReplaceProp shape "autogen" "string" "qfill")
           )
  qCV
  )
