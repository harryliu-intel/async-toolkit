; Expand rectangle pins to cover all touching metal
(defun ExpandPins (@key (CV (geGetEditCellView)))
  (let (tempCV overlaps fig dd_CV)

    ; flatten pcells and vias to temp view
    tempCV=(betterCopyCellView CV CV->libName CV->cellName "layout_temp" nil nil t)
    (when !tempCV (error "Couldn't edit layout_temp view for %s\n" CV->cellName))
    (foreach inst tempCV->instances (dbFlattenInst inst 32 t))
    (foreach inst tempCV->vias      (dbFlattenInst inst 32 t))

    ; copy pins to non-pin shapes
    (foreach shape tempCV->shapes
             (when shape->pin
               fig = (dbCopyFig shape tempCV)
               fig->lpp = (list (car fig->lpp) "drawing")
               )
             )
    (DeleteLabels ?CV tempCV)

    ; merge shapes
    (leMergeShapes tempCV->shapes)

    ; shapes that overlap original rectangle pins become new polygon pins
    (foreach shape CV->shapes
      (when shape->pin && (cadr shape->lpp)=="drawing" && shape->objType=="rect"
            overlaps = (dbGetTrueOverlaps tempCV shape->bBox shape->lpp 0)
            (foreach fig overlaps
              (when !fig->pin && fig->objType!="label"
                    fig = (dbCopyFig fig CV)
                    (leConvertShapeToPolygon fig)
                    (dbAddFigToPin shape->pin fig)
                    )
              )
            )
      )

    ; delete layout_temp view
    dd_CV = (ddGetObj tempCV->libName tempCV->cellName "layout_temp")
    (when dd_CV (ddDeleteObj dd_CV))
    )
  t
  )

; Delete polygon pins, such as those created by ExpandPins
(defun DeletePolygonPins (@key (CV (geGetEditCellView)))
  (foreach shape CV->shapes
           (when shape->pin && shape->objType=="polygon"
                 (dbDeleteObject shape)
                 )
           )
  t
  )
