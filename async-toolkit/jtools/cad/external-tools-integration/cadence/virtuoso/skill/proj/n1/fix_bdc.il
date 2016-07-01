; replace bdc01's with our custom versions
(defun FixBDC (@key (CV (geGetEditCellView)))
  (let (old4 old8 new4 new8 n mapInstance)
    n=0
    old4 = (dbOpenCellViewByType "vendor.intel.d04" "vendor.intel.d04.bdc01ld.z04" "layout")
    old8 = (dbOpenCellViewByType "vendor.intel.d04" "vendor.intel.d04.bdc01ld.z08" "layout")
    new4 = (dbOpenCellViewByType "globals" "globals.bdc01ld.z04" "layout")
    new8 = (dbOpenCellViewByType "globals" "globals.bdc01ld.z08" "layout")
    mapInstance = (makeTable "mapInstance" nil)
    mapInstance[old4]=new4
    mapInstance[old8]=new8
    (foreach inst CV->instances
             (when mapInstance[inst->master] inst->master=mapInstance[inst->master] n++)
             )
    n
    )
)
