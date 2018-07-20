(load "reroute.il")


(defun RouteCTRL (cellName)
  (let (templatefp floorplanCV flattenCV layoutCV abstractCV)
    (printf "creating %s\n" cellName)
    libName = (GetCastLibName cellName)
    templatefp = (dbOpenCellViewByType "lib.bd.standard.pipeline" "lib.bd.standard.pipeline.CTRL-L9_false_false-R.0" "floorplan")

    ;floorplanCV = (betterCopyCellView templatefp libName cellName "floorplan" nil nil t)
    ;floorplanCV = (dbOpenCellViewByType libName cellName "floorplan" "maskLayout" "w")
    (geOpen ?lib libName ?cell cellName ?view "floorplan")
    floorplanCV = (geGetEditCellView)
    (UpdateFloorplan ?CV floorplanCV)

    flattenCV = (MakeFlatten ?CV floorplanCV ?topMetal 2 ?powerGrid nil ?colorGrid nil)
    (RedrawAllPins ?CV flattenCV ?include_supply t ?supply_topMetal 2)
    (dbSave flattenCV)
    (leCloseWindow)

    layoutCV = (betterCopyCellView flattenCV flattenCV->libName flattenCV->cellName "layout" nil nil t)
    abstractCV = (MakeAbstract ?CV layoutCV ?topMetal 2)
    ;(when floorplanCV (cdsp4add ?CV floorplanCV))
    ;(when layoutCV (cdsp4add ?CV layoutCV))
    ;(when abstractCV (cdsp4add ?CV abstractCV))

  )
)



(defun CopyCTRL (cell)
  (let (scv fcv lcv acv srcName)
    libName = "lib.bd.standard.pipeline"
    cellName = (sprintf nil "%s.%s" libName cell)
    (printf "creating %s\n" cellName)

    srcName = (sprintf nil "%s.4GBD_%s" libName cell)
    scv = (dbOpenCellViewByType libName srcName "floorplan")
    fcv = (betterCopyCellView scv libName cellName "floorplan" nil nil t)
    scv = (dbOpenCellViewByType libName srcName "layout")
    lcv = (betterCopyCellView scv libName cellName "layout" nil nil t)
    scv = (dbOpenCellViewByType libName srcName "abstract")
    acv = (betterCopyCellView scv libName cellName "abstract" nil nil t)

    (when fcv (cdsp4add ?CV fcv))
    (when lcv (cdsp4add ?CV lcv))
    (when acv (cdsp4add ?CV acv))
  )
)


(defun MakeCTRL ()
  (for i 1 8
    (CopyCTRL (sprintf nil "CTRL-L%d_false_false-R.0" i) )
    (CopyCTRL (sprintf nil "CTRL-L%d_false_false-R.0n" i) )
    (CopyCTRL (sprintf nil "CTRL-L%d_true_false-R.0" i) )
    (CopyCTRL (sprintf nil "CTRL-L%d_true_false-R.0n" i) )
  )

;  (for i 9 15
;    (RouteCTRL (sprintf nil "lib.bd.standard.pipeline.CTRL-L%d_false_false-R.0" i) )
;    (RouteCTRL (sprintf nil "lib.bd.standard.pipeline.CTRL-L%d_false_false-R.0n" i) )
;    (RouteCTRL (sprintf nil "lib.bd.standard.pipeline.CTRL-L%d_true_false-R.0" i) )
;    (RouteCTRL (sprintf nil "lib.bd.standard.pipeline.CTRL-L%d_true_false-R.0n" i) )
;  )
)
