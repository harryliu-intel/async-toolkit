; Create abstracts using settings in wires.il
;
; Copyright 2011 Fulcrum Microsystems.  All rights reserved.

; make mid-level abstract using information from wires.il
(defun MakeAbstract
  (@key (CV (geGetEditCellView)))
  (let (win abstractPinCutout abstractPinLength abstractBloatM1
            abstractBloatM2 abstractBloatM3 abstractBloatM4
            abstractBloatM5 abstractBloatM6 abstractBloatM7
            abstractCutHolesAbovePins)
    (unless CV->viewName=="layout_pg"
            (printf "WARNING: recommend MakeAbstract from layout_pg\n")
            )

    ; load settings from wires.il
    (LoadWires)
    
    ; create abstract
    CV = (nrCreateAbstractCB CV
                             ?ERASE_POWERGRID t
                             ?PIN_CUTOUT abstractPinCutout
                             ?PIN_LENGTH abstractPinLength
                             ?M1BLOAT abstractBloatM1
                             ?M2BLOAT abstractBloatM2
                             ?M3BLOAT abstractBloatM3
                             ?M4BLOAT abstractBloatM4
                             ?M5BLOAT abstractBloatM5
                             ?M6BLOAT abstractBloatM6
                             ?M7BLOAT abstractBloatM7
                             ?M8BLOAT 0
                             ?useAbstractSubcells t
                             ?keepLayoutSubcells abstractKeepLayoutSubcells
                             ?NO_HOLES_ABOVE_PINS !abstractCutHolesAbovePins
                             ?targetViewName "abstract"
                             ?PowerNets nil
                             ?M4PowerNets nil
                             ?M5PowerNets nil
                             ?M6PowerNets nil
                             ?M7PowerNets nil)

    ; change to abstract view
    win = (hiGetCurrentWindow)
    (when win
      (geOpen ?window win
              ?lib  CV->libName
              ?cell CV->cellName
              ?view CV->viewName
              ?viewType "maskLayout"
              ?mode "a")
      )

    ; return abstract view
    CV
    )
  )

; default options for mid-cell abstract generation, called by LoadWires
(defun AbstractDefaults ()
  abstractPinCutout = 2*DefaultWiringSpacing
  abstractPinLength = 2*DefaultWiringWidth
  abstractBloatM1 = -1
  abstractBloatM2 = -1
  abstractBloatM3 = -1
  abstractBloatM4 = -1
  abstractBloatM5 = -1
  abstractBloatM6 = PowerGridPitch
  abstractBloatM7 = PowerGridPitch
  abstractCutHolesAbovePins = nil
  abstractKeepLayoutSubcells = nil
  t
  )

; make abstract for a leaf cell
(defun MakeLeafAbstract
  (@key (CV (geGetEditCellView))
        (pinCutout 0.36)
        (pinLength 2.88)
        (bloatM2 -1)
        (bloatM3  0)
        )
  (unless CV->viewName=="layout" (error "MakeLeafAbstract on a layout view\n"))
  (unless (setof sub CV->instances
            sub->libName!=TechLibName && sub->libName!="stack" && sub->libName!="gate")
    (printf "Creating abstract of %s\n" CV->cellName)
    (nrCreateAbstractCB CV
                        ?ERASE_POWERGRID nil
                        ?PIN_CUTOUT pinCutout
                        ?PIN_LENGTH pinLength
                        ?M1BLOAT -1
                        ?M2BLOAT bloatM2
                        ?M3BLOAT bloatM3
                        ?M4BLOAT 0
                        ?M5BLOAT 0
                        ?M6BLOAT 0
                        ?M7BLOAT 0
                        ?M8BLOAT 0
                        ?useAbstractSubcells nil
                        ?keepLayoutSubcells t
                        ?NO_HOLES_ABOVE_PINS nil
                        ?targetViewName "abstract"
                        ?PowerNets t
                        ?M4PowerNets nil
                        ?M5PowerNets nil
                        ?M6PowerNets nil
                        ?M7PowerNets nil)
    )
  )
