(defun Place_E_CABLE (@key (CV (geGetEditCellView)) (pitch 18))
  pitch=pitch*1.68
  (geDeselectAll)
  (SelectInstRegExp "chanRW")
  (ArrayCells (list 0:0 -pitch:0))
  (geDeselectAll)
  (SelectInstRegExp "chanRE")
  (ArrayCells (list 0:0 pitch:0))
  (geDeselectAll)
  t
  )

(defun Place_N_CABLE (@key (CV (geGetEditCellView)) (pitch 20))
  pitch=pitch*1.596
  (geDeselectAll)
  (SelectInstRegExp "chanRN\\[")
  (ArrayCells (list 0:0 0:pitch))
  (geDeselectAll)
  (SelectInstRegExp "chanRS\\[")
  (ArrayCells (list 0:0 0:-pitch))
  (geDeselectAll)
  (SelectInstRegExp "chanRN_DFX")
  (ArrayCells (list 0:pitch))
  (geDeselectAll)
  (SelectInstRegExp "chanRS_DFX")
  (ArrayCells (list 0:-pitch))
  (geDeselectAll)
  t
  )
