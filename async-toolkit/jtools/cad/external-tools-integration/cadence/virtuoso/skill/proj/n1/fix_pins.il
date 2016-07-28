; draw inplace pins for VDDM
(defun DrawPinsVDDM (@key (CV (geGetEditCellView)))
  (foreach inst CV->instances
           (MidPinsInheritPin CV inst "VDDM" "VDDM")
           )
  t
  )

; Do a bunch of crap to regenerate correct pins on Proteus layout view
; Must provide a reference cell to copy pins from
(defun FixProteusPins (srcCellName @key (CV (geGetEditCellView)))
  (let (srcCV fig net)
    srcCV = (dbOpenCellViewByType (GetCastLibName srcCellName) srcCellName "floorplan")
    (unless srcCV (error "Can't read %s\n" srcCellName))
    (DeletePins ?CV CV)
    (foreach shape srcCV->shapes
             (when shape->pin
               fig = (dbCopyFig shape CV)
               net = (dbMakeNet CV shape->net->name)
               (dbCreatePin net fig)
               )
             )
    (DeleteLabels ?CV CV)
    (CanonicalizePins ?CV CV)
    (LabelPins ?CV CV)
    (MakePowerGridAbstract ?CV CV)
    (CreatePowerPins ?CV CV)
    (DrawPinsVDDM ?CV CV)
    )
  (dbSave CV)
  CV
  )
