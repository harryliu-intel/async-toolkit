; Scripts to choose between 3 M1 layout varaints for 7nm

; Compute preferred variant number using instance's xy and orient
(defun LayoutVariantNumber (inst)
  (let (v x nx mx my)
    v=nil
    x=(car inst->xy)
    nx=(round x/GridPolyPitch)
    mx=(mod (mod nx 3)+3 3)
    my=inst->orient=="MY" || inst->orient=="R180"
    (when TechLibName=="1276"
          (cond (mx==1 && !my v=1)
                (mx==1 &&  my v=3)
                (mx==2 && !my v=3)
                (mx==2 &&  my v=1)
                (t            v=2)
                )
          )
    v
    )
  )

; Construct E08/G0M/G1M standard cell name
(defun VariantStdCellName (cellName v)
  (let (components subtype)
    components = (parseString cellName ".")
    subtype = (car (last components))
    subtype = (sprintf nil "%d%s" v (substring subtype 2 5))
    components = (reverse components)
    components = (cdr components)
    components = (cons subtype components)
    components = (reverse components)
    (buildString components ".")
    )
  )

; Pick layout variant CV given an instance
(defun ChooseLayoutVariant (inst)
  (let (v lCV stdcell)
    v=(LayoutVariantNumber inst)
    stdcell=inst->libName=="vendor.intel.g0m" ||
            inst->libName=="vendor.intel.g1m" ||
            inst->libName=="vendor.intel.e08"
    (cond (stdcell lCV=(ReadCV inst->libName (VariantStdCellName inst->cellName v) "layout"))
          (t
           lCV=(ReadCV inst->libName inst->cellName "layout") ; default if it exists!
           (when v && !lCV lCV=(ReadCV inst->libName (sprintf nil "%sv%d" inst->cellName v) "layout"))
           )
          )
    lCV
    )
  )
