; copy pins and labels from a different view
(defun ClonePins (srcCellName @key (CV (geGetEditCellView)) (srcView "flatten"))
  (let (srcCV net fig)
    (DeleteLabels ?CV CV)
    (DeletePins ?CV CV)
    srcCV = (dbOpenCellViewByType (GetCastLibName srcCellName) srcCellName srcView)
    (foreach shape srcCV->shapes
             (when shape->pin
               fig = (dbCopyFig shape CV)
               net = (dbMakeNet CV shape->net->name)
               (dbCreatePin net fig)
               ; (LabelPin CV fig)
               )
             )
    )
  t
  )

; draw inplace pins for VDDM
(defun DrawPinsVDDM (@key (CV (geGetEditCellView)))
  (foreach inst CV->instances
           (MidPinsInheritPin CV inst "VDDM" "VDDM")
           )
  t
  )
