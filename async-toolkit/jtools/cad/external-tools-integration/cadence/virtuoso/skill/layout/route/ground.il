; Connect prerouted GND shields to power supply stripes by adding only
; vias.
;
; Copyright 2011 Fulcrum Microsystems.  All rights reserved.

; Connect as many GND shields as possible using only vias.
(defun ConnectGroundShields
  (@key (CV (geGetEditCellView)))
  (ConnectVerticalGroundShieldsToStripes ?CV CV)
  + (ConnectHorizontalGroundShields ?CV CV)
  )

; check for existing contact
(defun CheckForContactOverlap (xy lpps @key (CV (geGetEditCellView)))
  (let (overlaps ret)
    ret = nil
    (foreach lpp lpps
      overlaps = (dbGetOverlaps CV (list xy xy) (list lpp->layerName lpp->purpose) 1)
      (when (setof ovl overlaps !(atom ovl) && (car ovl)->libName==TechLibName)
        ret = t
        )
      )
    ret
    )
  )

; create a GND contact
(defun CreateGroundShieldContact (xy type @key (CV (geGetEditCellView)))
  (let (n contCV inst)
    n = 0
    contCV = (dbOpenCellViewByType TechLibName type "layout")
    (unless (CheckForContactOverlap xy contCV->lpps ?CV CV)
      inst = (dbCreateParamInst CV contCV (NextContactName CV) xy "R0" 0 nil)
      (dbReplaceProp inst "GroundShieldContact" "boolean" t)
      (dbCreateConnByName GND inst "pdd")
      (SetBusProp inst)
      n = 1
      )
    n
    )
  )

; Delete contacts added by ConnectGroundShields
(defun DeleteGroundShieldContacts
  (@key (CV (geGetEditCellView)))
  (foreach obj CV->instances
           (when (and obj->libName==TechLibName
                      (dbGetPropByName obj "GroundShieldContact")
                      (dbGetPropByName obj "GroundShieldContact")->value)
             (dbDeleteObject obj)
             )
           )
  t
  )

; Connect vertical GND shields to horizontal stripes on layer above.
(defun ConnectVerticalGroundShieldsToStripes
  (@key (CV (geGetEditCellView)))
  (let ((n 0) GND shields xy0 xy1 x0 y0 x1 y1 x y yg type)

    ; find vertical GND shields
    GND = (dbFindNetByName CV "GND")
    (unless GND (error "No GND net\n"))
    shields = (setof shape CV->shapes
                     (and shape->net==GND
                          (or (and shape->objType=="path" (length shape->points)==2)
                              shape->objType=="rect")
                          (or shape->layerName=="M2"
                              shape->layerName=="M4"
                              shape->layerName=="M6")))

    ; process each shield shape
    (foreach shield shields
      xy0 = (car  shield->bBox)
      xy1 = (cadr shield->bBox)
      x0 = (car  xy0)
      y0 = (cadr xy0)
      x1 = (car  xy1)
      y1 = (cadr xy1)
      x = (x0 + x1)/2
      y = (y0 + y1)/2
      yg = (round y/5.76)*5.76
      (when (and yg>=(min y0 y1)+DefaultWiringPitch yg<=(max y0 y1)-DefaultWiringPitch)
        (cond (shield->layerName=="M2" type = "M3_M2_2CUT_V")
              (shield->layerName=="M4" type = "M5_M4_2CUT_V")
              (shield->layerName=="M6" type = "M7_M6_2CUT_V")
              (t (error))
              )
        n=n+(CreateGroundShieldContact x:yg type ?CV CV)
        )
      )
    n
    )
  )

; Connect horizontal GND shields to all crossing vertical GND wires.
(defun ConnectHorizontalGroundShields
  (@key (CV (geGetEditCellView)) (overhang DefaultWiringPitch))
  (let ((n 0) GND shields xy0 xy1 x0 y0 x1 y1 x y oxy0 oxy1 ox0 oy0 ox1 oy1
        layers overlaps type)

    ; find vertical GND shields
    GND = (dbFindNetByName CV "GND")
    (unless GND (error "No GND net\n"))
    shields = (setof shape CV->shapes
                     (and shape->net==GND
                          (or (and shape->objType=="path" (length shape->points)==2)
                              shape->objType=="rect")
                          (or shape->layerName=="M3"
                              shape->layerName=="M5"
                              shape->layerName=="M7")))

    ; process each shield shape
    (foreach shield shields
      xy0 = (car  shield->bBox)
      xy1 = (cadr shield->bBox)
      x0 = (car  xy0)
      y0 = (cadr xy0)
      x1 = (car  xy1)
      y1 = (cadr xy1)
      y = (y0+y1)/2
      (cond (shield->layerName=="M3" layers = (list "M4" "M2"))
            (shield->layerName=="M5" layers = (list "M6" "M4"))
            (shield->layerName=="M7" layers = (list "M6"))
            (t (error))
            )
      (foreach layer layers
        (cond ((and shield->layerName=="M3" layer=="M2") type = "M3_M2min")
              ((and shield->layerName=="M3" layer=="M4") type = "M4_M3min")
              ((and shield->layerName=="M5" layer=="M4") type = "M5_M4min")
              ((and shield->layerName=="M5" layer=="M6") type = "M6_M5min")
              ((and shield->layerName=="M7" layer=="M6") type = "M7_M6min")
              (t (error))
              )
        overlaps = (dbGetOverlaps CV shield->bBox (list layer "drawing") 0)
        (foreach overlap overlaps
          oxy0 = (car  overlap->bBox)
          oxy1 = (cadr overlap->bBox)
          ox0 = (car  oxy0)
          oy0 = (cadr oxy0)
          ox1 = (car  oxy1)
          oy1 = (cadr oxy1)
          x = (ox0+ox1)/2
          (when (and overlap->net==GND
                     (or (and overlap->objType=="path"
                              (length overlap->points)==2)
                         overlap->objType=="rect")
                     x>=x0+overhang
                     x<=x1-overhang
                     y>=oy0+overhang
                     y<=oy1-overhang)
            n=n+(CreateGroundShieldContact x:y type ?CV CV)
            )
          )
        )
      )
    n
    )
  )
