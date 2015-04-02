; A replacement for dbCopyCellView.  When the b_overwrite bit is true,
; it will dbReopen the destination CellView in "r" mode so that
; dbCopyCellView will overwrite it without error regardless of locking
; state.  Any other CV pointers to the destination remain valid,
; although the CV->mode always becomes "a".
(defun betterCopyCellView (d_srcCVId t_libName t_cellName t_viewName
                                     @optional (t_version nil) (g_contextId nil) (b_overwrite nil))
  (let (CV)
    (when b_overwrite
      (when (ddGetObj t_libName t_cellName t_viewName)
        CV = (dbOpenCellViewByType t_libName t_cellName t_viewName)
        (when CV (dbReopen CV "r"))
        )
      )
    CV = (dbCopyCellView d_srcCVId t_libName t_cellName t_viewName t_version g_contextId b_overwrite)
    (when CV (dbReopen CV "a"))
    CV
    )
  )
