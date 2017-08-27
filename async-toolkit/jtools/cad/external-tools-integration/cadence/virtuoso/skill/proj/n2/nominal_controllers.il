uv1_cell_list = 
    (list
     ; gen 3 controllers
     "lib.bd.standard.nopipe.RECV_CTRL.0"
     "lib.bd.standard.nopipe.SEND_CTRL.0"
     "lib.bd.convert.qdi.BD_TO_QDI_CTRL.0"
     "lib.bd.convert.qdi.QDI_TO_BD_CTRL.0"
     "lib.synchronous.6T.sram.SRAM_SAMPLER.0"
     "lib.bd.standard.pipeline.PULSE_CTRL_CORE-Lfalse-R.0"
     "lib.bd.standard.pipeline.PULSE_CTRL_CORE-Ltrue-R.0"
     "lib.bd.standard.send.PULSE_SEND_CTRL_CORE-Lfalse-R.0"
     "lib.bd.standard.send.PULSE_SEND_CTRL_CORE-Ltrue-R.0"
     )

; CTREE
;(for i 0 3
;  uv1_cell_list = (cons (sprintf nil "lib.util.ctree.CTREE2.%d" i) uv1_cell_list)
;  uv1_cell_list = (cons (sprintf nil "lib.util.ctree.CTREE3.%d" i) uv1_cell_list)
;  )

; PULSE_CTRL/PULSE_SEND_CTRL
(for i 0 8
  (foreach s (list "false" "true")
    uv1_cell_list = (cons (sprintf nil "lib.bd.standard.pipeline.PULSE_CTRL-L%d_%s-R.0"  i s) uv1_cell_list)
    uv1_cell_list = (cons (sprintf nil "lib.bd.standard.send.PULSE_SEND_CTRL-L%d_%s-R.0" i s) uv1_cell_list)
    )
  )

; make additional BD_CONTROLLER's by copying and editing base ones
(defun MakeNominalControllers ()
  ; make 0n threshold versions
  (foreach cellName uv1_cell_list
           CV = (ReadCV (GetCastLibName cellName) cellName "layout")
           (MakeNominalController ?CV CV)
           )
  t
  )

; make nominal threshold new BD controller from the UV1 threshold one
(defun MakeNominalController (@key (CV (geGetEditCellView)))
  (let (libName cellName newName newCV pcre)
    libName = CV->libName
    cellName = CV->cellName
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
    (UpdateFloorplan ?CV CV ?recurse nil)
    (dbSave CV)
    (cdsp4add ?CV CV)

    ; copy abstract view
    CV = (dbOpenCellViewByType libName cellName "abstract")
    CV = (betterCopyCellView CV libName newName "abstract" nil nil t)
    (dbSave CV)
    (cdsp4add ?CV CV)

    ; copy layout view
    pcre = (pcreCompile "(vendor\\.intel\\.d04\\......)l(.\\....)")
    CV = (dbOpenCellViewByType libName cellName "layout")
    CV = (betterCopyCellView CV libName newName "layout" nil nil t)
    (UpdateFloorplan ?CV CV ?recurse nil)
    (foreach sub CV->instances
      (cond ((pcreExecute pcre sub->cellName)
             newName = (pcreSubstitute pcre "\\1n\\2")
             sub->master = (dbOpenCellViewByType sub->libName newName sub->viewName)
             )
            (t
             newName = (strcat sub->master->cellName "n")
             newCV = (ReadCV sub->master->libName newName sub->master->viewName)
             (when newCV sub->master=newCV)
             )
            )
      )
    (foreach x CV->shapes (when x->layerName=="NV1" || x->layerName=="PV1" (dbDeleteObject x)))
    (dbSave CV)
    (cdsp4add ?CV CV)
    )
  t
  )
