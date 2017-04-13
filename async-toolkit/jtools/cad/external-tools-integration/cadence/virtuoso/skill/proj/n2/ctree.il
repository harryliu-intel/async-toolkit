; write GDS for all CTREEs from layout_tag view for DRC
(defun ExportCTREEs ()
  (for n 2 3
       (for i 0 3
            cellName = (sprintf nil "lib.util.ctree.CTREE%d.%d" n i)
            CV = (ReadCV "lib.util.ctree" cellName "layout")
            CV = (MakeLayoutTag ?CV CV ?doFill t)
            (ExportGDS ?CV CV)
            )
       )
  t
  )

; Copy UV1 CTREE to nominal CTREE, delete implant, update abstract
(defun MakeNominalCTREE (@key (CV (geGetEditCellView)))
  newName = (strcat CV->cellName "n")
  CV = (betterCopyCellView CV CV->libName newName "layout" nil nil t)
  (foreach x CV->shapes (when x->layerName=="NV1" || x->layerName=="PV1" (dbDeleteObject x)))
  (dbSave CV)
  (MakeLeafAbstract ?CV CV)
  )
