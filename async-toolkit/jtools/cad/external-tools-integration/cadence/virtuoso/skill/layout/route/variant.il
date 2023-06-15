; does this cell appear to use metal variants?
(defun HasLayoutVariants (@key (CV (geGetEditCellView)))
  (let (suffix)
    suffix = (substring CV->cellName (strlen CV->cellName)-1)
    (TechLibName=="1276.3" || TechLibName=="1276.4") &&
      (suffix=="v1" || suffix=="v2" || suffix=="v3" || suffix=="v4" || suffix=="v5" ||
               CV->libName=="vendor.intel.g1m" || CV->libName=="vendor.intel.g1i")
    )
  )

; Find layout CV with appropriate v1/v2/v3/v4/v5 variant of current CV
(defun FindLayoutVariant (CV v)
  (let (cellName suffix components subtype baseName)
    suffix = (substring CV->cellName (strlen CV->cellName)-1)
    (cond (CV->libName=="vendor.intel.g1m" || CV->libName=="vendor.intel.g1i" ; G1M/G1I naming convention
           components = (parseString CV->cellName ".")
           subtype = (car (last components))
           subtype = (sprintf nil "%d%s" v (substring subtype 2 5))
           components = (reverse components)
           components = (cdr components)
           components = (cons subtype components)
           components = (reverse components)
           cellName = (buildString components ".")
           )
          (suffix=="v1" || suffix=="v2" || suffix=="v3" || suffix=="v4" || suffix=="v5" ; 4GBD naming
           baseName = (substring CV->cellName 1 (strlen CV->cellName)-2)
           cellName = (sprintf nil "%sv%d" baseName v)
           )
          (t cellName=CV->cellName ; other cells
           )
          )
    (ReadCV CV->libName cellName "layout")
    )
  )

; Replace instance->master with aligned layout variant CV
(defun ChooseLayoutVariant (inst)
  (let (oldMaster aligned v lCV)
    oldMaster=inst->master
    aligned=nil
    v=1
    (while !aligned && v<=5
           lCV=(FindLayoutVariant inst->master v)
           (when lCV
             inst->master=lCV
             aligned=(CheckInstanceAlignment inst ?check_y nil ?check_parent_grid nil)
             )
           v=v+1
           )
    (unless aligned inst->master=oldMaster)
    aligned
    )
  )

; pick the right variant for instances in CV (useful for fill cells)
(defun ChooseLayoutVariants (@key (CV (geGetEditCellView)))
  (foreach inst CV->instances
           (when (HasLayoutVariants ?CV inst->master) (ChooseLayoutVariant inst))
           )
  )
