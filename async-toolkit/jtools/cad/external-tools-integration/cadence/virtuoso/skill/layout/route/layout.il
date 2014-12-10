; Create a layout view from layout_pg view

(defun MakeLayout
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        )
  (let (layoutCV tiehilo)
    (when CV->viewName=="layout_pg"
          layoutCV = (dbCopyCellView CV CV->libName CV->cellName "layout" nil nil t)
          (when !layoutCV (error "Unable to write layout view\n"))
          tiehilo = (dbFindAnyInstByName layoutCV "tiehilo")
          (when tiehilo (dbDeleteObject tiehilo))
          tiehilo = (dbFindMosaicByName layoutCV "tiehilo")
          (when tiehilo (dbDeleteObject tiehilo))
          (dbSave layoutCV)
          )
    layoutCV
    )
  )
