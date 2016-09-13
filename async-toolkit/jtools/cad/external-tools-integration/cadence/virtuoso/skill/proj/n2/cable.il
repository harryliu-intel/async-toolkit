(defun Place_E_CABLE (@key (CV (geGetEditCellView)) (pitch 18))
  pitch=pitch*1.68
  (geDeselectAll)
  (SelectInstRegEx "chanRW")
  (ArrayCells (list 0:0 -pitch:0))
  (geDeselectAll)
  (SelectInstRegEx "chanRE")
  (ArrayCells (list 0:0 pitch:0))
  (geDeselectAll)
  t
  )

(defun Place_N_CABLE (@key (CV (geGetEditCellView)) (pitch 20))
  pitch=pitch*1.596
  (geDeselectAll)
  (SelectInstRegEx "chanRN\\[")
  (ArrayCells (list 0:0 0:pitch))
  (geDeselectAll)
  (SelectInstRegEx "chanRS\\[")
  (ArrayCells (list 0:0 0:-pitch))
  (geDeselectAll)
  (SelectInstRegEx "chanRN_DFX")
  (ArrayCells (list 0:pitch))
  (geDeselectAll)
  (SelectInstRegEx "chanRS_DFX")
  (ArrayCells (list 0:-pitch))
  (geDeselectAll)
  t
  )
