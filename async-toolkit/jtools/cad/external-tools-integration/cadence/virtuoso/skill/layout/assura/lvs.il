; Batch and user friendly wrappers to create CDL and run LVS.

; Run LVS
(defun AssuraLVS
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        (RegenerateCDL nil)      ; force regeneration of CDL
        )
  (let (TEMP PDKRoot SchematicFile status
             LVSRuleFile LVSBindFile LVSCompareFile LVSIncludeFile)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP" )
    PDKRoot = (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
    LVSRuleFile = (sprintf nil "%s/share/Fulcrum/assura/extract.rul" PDKRoot)
    LVSBindFile = (sprintf nil "%s/share/Fulcrum/assura/bind.rul" PDKRoot)
    LVSCompareFile = (sprintf nil "%s/share/Fulcrum/assura/compare.rul" PDKRoot)
    LVSIncludeFile = (sprintf nil "%s/share/Fulcrum/assura/LVSinclude.rsf" PDKRoot)
    SchematicFile = (CreateCDL ?CV CV ?RegenerateCDL RegenerateCDL)
    (dbSave CV)
    status = (AssuraRunLVS CV
                           LVSRuleFile
                           (list "EXPAND")
                           LVSBindFile
                           LVSCompareFile
                           LVSIncludeFile
                           SchematicFile
                           TEMP
                           "lvs")
    (printf "LVS: %s\n" (if status "PASS" "FAIL"))
    status
    )
  )

; Generate CDL
(defun CreateCDL
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        (RegenerateCDL t)        ; force regeneration of CDL
        (maxheap (nrGetMaxHeapSize)) ; max-heap-size
        )
  (let (TEMP CAST_PATH CDLFile CMD status)
    CAST_PATH = (ConfigFileGetValue TheCDSConfigTable "CAST_PATH")
    CDLFile = (sprintf nil "cdl/%s.cdl" CV->cellName)
    (when (or !(isFile CDLFile) RegenerateCDL)
      CMD = (sprintf nil
                     "%s/cast2cdl --cast-path=%s --cell=%s --cadence-name --output=%s --max-heap-size=%dM --name-map=%s/share/Fulcrum/lve/transistor.map --process-dependent-name --64"
                     (PackageGetBinRoot)
                     CAST_PATH
                     CV->cellName
                     CDLFile maxheap
                     (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT"))
      status = (shell CMD)
      (when !status (error "Unable to generate %s\n" CDLFile))
      )
    CDLFile
    )
  )
