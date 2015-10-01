; draw NV1/PV1 in a custom fragment cell
(defun DrawUV1 (@key (CV (geGetEditCellView)))
  (let  (x0 x1)
    (cdsp4edit ?CV CV)
    x0 = (car (car  CV->prBoundary->bBox))
    x1 = (car (cadr CV->prBoundary->bBox))
    nuv1 = (list "NV1" "drawing")
    puv1 = (list "PV1" "drawing")
    (dbCreateRect CV nuv1 (list x0:0     x1:0.231))
    (dbCreateRect CV puv1 (list x0:0.231 x1:0.567))
    (dbCreateRect CV nuv1 (list x0:0.567 x1:0.798))
    (leMergeShapes (setof x CV->shapes x->lpp==nuv1))
    (leMergeShapes (setof x CV->shapes x->lpp==puv1))
    )
  CV
  )

; Change threshold of filler cells to fix NV1/PV1 DRC errors
(defun FixFillerCells (@key (CV (geGetEditCellView)))
  (let (spc00ln spc00nn bbox x0 y0 x1 y1 overlaps)
    spc00ln = (dbOpenCellViewByType "vendor.intel.d04" "vendor.intel.d04.spc00ln.z01" "layout")
    spc00nn = (dbOpenCellViewByType "vendor.intel.d04" "vendor.intel.d04.spc00nn.z01" "layout")

    ; convert nn/wn to ln
    (foreach inst (setof x CV->instances
                         x->cellName=="vendor.intel.d04.spc00nn.z01" ||
                         x->cellName=="vendor.intel.d04.spc00wn.z01")
             inst->master = spc00ln
             )

    ; turn some ln to nn if it doesn't overlap enough pv1
    (foreach inst (setof x CV->instances x->cellName=="vendor.intel.d04.spc00ln.z01")
             bbox = (dbTransformBBox spc00ln->prBoundary->bBox (dbGetInstTransform inst))
             x0 = (car (car bbox))
             y0 = (cadr (car bbox))
             x1 = (car (cadr bbox))
             y1 = (cadr (cadr bbox))
             y0 = y0 + MfgGrid
             y1 = y1 - MfgGrid
             bbox = (list x0:y0 x1:y1)
             overlaps = (dbGetOverlaps CV bbox (list "PV1" "drawing") 31)
             (when (length overlaps)==1 inst->master = spc00nn)
             )
    (dbSave CV)
    )
  CV
  )
