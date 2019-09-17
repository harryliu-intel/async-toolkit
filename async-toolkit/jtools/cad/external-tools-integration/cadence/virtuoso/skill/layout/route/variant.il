; Scripts to choose between 3 M1 layout varaints for 7nm

; Compute preferred variant number using instance's xy and orient
(defun LayoutVariantNumber (inst)
  (let (v x nx mx my)
    v=nil
    x=(car inst->xy)
    my=inst->orient=="MY" || inst->orient=="R180"
    nx=(round (x-0.5*GridPolyPitch)/GridPolyPitch)
    mx=(mod (mod nx 3)+3 3)
    (when TechLibName=="1276"
          (cond (!my && mx==1 v=2)
                (!my && mx==2 v=1)
                (!my && mx==0 v=3)
                ( my && mx==1 v=2)
                ( my && mx==2 v=3)
                ( my && mx==0 v=1)
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
  (let (v lCV stdcell CopyExact)
    v=(LayoutVariantNumber inst)
    CopyExact=(dbGetPropByName inst->master "CopyExact")->value
    stdcell=CopyExact && ((strncmp "g0m" CopyExact 3)==0 || (strncmp "g1m" CopyExact 3)==0)
    (cond (stdcell lCV=(ReadCV inst->libName (VariantStdCellName inst->cellName v) "layout"))
          (t lCV=(ReadCV inst->libName inst->cellName "layout"))
          )
    lCV
    )
  )
