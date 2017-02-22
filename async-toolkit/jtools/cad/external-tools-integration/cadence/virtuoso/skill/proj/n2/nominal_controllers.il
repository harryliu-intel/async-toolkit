; make nominal threshold new BD controllers from the UV1 threshold ones
(defun MakeNominalControllers ()
  (foreach cellName (list "lib.bd.source.bitgen.BDC_BG.0"
                          "lib.bd.standard.pipeline.CTRL.0"
                          "lib.bd.standard.pipeline.TOK_CTRL.0"
                          "lib.bd.standard.pipeline.ISOLATE_CTRL.0"
                          "lib.bd.standard.pipeline.ISOLATE_TOK_CTRL.0"
                          "lib.bd.standard.send.SEND_CTRL.0"
                          "lib.bd.standard.receive.RECV_CTRL.0"
                          "lib.bd.standard.split.SPLIT_CTRL-L2-R.0"
                          "lib.bd.standard.split.ALT_SPLIT_CTRL-L2-R.0"
                          "lib.bd.standard.merge.MERGE_CTRL-L2-R.0" 
                          "lib.bd.standard.merge.ALT_MERGE_CTRL-L2-R.0"
                          "lib.bd.metastable.arbiter.ARBITER_CTRL.0")
           (MakeNominalController cellName)
           )
  t
  )

; make nominal threshold new BD controller from the UV1 threshold one
(defun MakeNominalController (cellName)
  (let (libName CV newName map)
    libName = (GetCastLibName cellName)
    newName = (strcat cellName "n")

    ; edit views
    CV = (dbOpenCellViewByType libName newName "floorplan")
    (when CV (cdsp4edit ?CV CV))
    CV = (dbOpenCellViewByType libName newName "layout")
    (when CV (cdsp4edit ?CV CV))
    CV = (dbOpenCellViewByType libName newName "abstract")
    (when CV (cdsp4edit ?CV CV))
    
    ; copy floorplan view
    CV = (dbOpenCellViewByType libName cellName "floorplan")
    CV = (betterCopyCellView CV libName newName "floorplan" nil nil t)
    (UpdateFloorplan ?CV CV)
    (dbSave CV)
    (cdsp4add ?CV CV)

    ; copy abstract view
    CV = (dbOpenCellViewByType libName cellName "abstract")
    CV = (betterCopyCellView CV libName newName "abstract" nil nil t)
    (dbSave CV)
    (cdsp4add ?CV CV)

    ; replace physical cells with nominal threshold versions
    map = (makeTable "map" nil)
    map["vendor.intel.d04.spc00ln.z01"]="vendor.intel.d04.spc00nn.z01";
    map["vendor.intel.d04.spc00ln.z02"]="vendor.intel.d04.spc00nn.z02";
    map["vendor.intel.d04.spc00ln.z03"]="vendor.intel.d04.spc00nn.z03";
    map["vendor.intel.d04.spc00ln.z04"]="vendor.intel.d04.spc00nn.z04";
    map["vendor.intel.d04.tap02ld.z05"]="vendor.intel.d04.tap02nd.z05";

    ; copy layout view
    CV = (dbOpenCellViewByType libName cellName "layout")
    CV = (betterCopyCellView CV libName newName "layout" nil nil t)
    (UpdateFloorplan ?CV CV)
    (foreach sub CV->instances
             (when map[sub->cellName]
               sub->master = (dbOpenCellViewByType sub->libName map[sub->cellName] sub->viewName)
               )
             )
    (dbSave CV)
    (cdsp4add ?CV CV)
    )
  t
  )
