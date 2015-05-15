; Delete shapes with autogen=t property
(defun DeleteAutoGen (@key (CV (geGetEditCellView)) (propName "autogen"))
  (foreach obj (setof x CV->shapes (dbGetPropByName x propName)->value)
       (dbDeleteObject obj)
       )
  (foreach obj (setof x CV->vias (dbGetPropByName x propName)->value)
       (dbDeleteObject obj)
       )
  )

; Chop fill to avoid other purposes
(defun ChopFill (layerName @key
                           (CV (geGetEditCellView))
                           (topMetal 6)
                           (xbloat 0)
                           (ybloat 0)
                           (levels 31)
                           )
  (let (cellName fillCV lpp_fill lpp_blk lpp_tmp rect bbox shapes new_shapes
                 overlaps fill_shapes new_fill_shapes
                 x y x0 y0 x1 y1 x0box y0box x1box y1box cols rows align) 

    ; find fill template
    cellName = (strcat CV->cellName "_fill_template")
    fillCV = (dbOpenCellViewByType CV->libName cellName CV->viewName)
    x0=0 y0=0 x1=0 y1=0 rows=1 cols=1
    align = 0:0
    (unless fillCV
      cellName = "globals.FILL_TEMPLATE.m0123456_fill"
      (printf "using default %s\n" cellName)
      fillCV = (dbOpenCellViewByType "globals" cellName "layout")
      align = gridAlignment[8]
      prb = CV->prBoundary
      x0box = 1.0*(round (leftEdge   prb->bBox)/MfgGrid)
      x1box = 1.0*(round (rightEdge  prb->bBox)/MfgGrid)
      y0box = 1.0*(round (bottomEdge prb->bBox)/MfgGrid)
      y1box = 1.0*(round (topEdge    prb->bBox)/MfgGrid)
      x0 = (floor   x0box/(round (car  align)/MfgGrid))
      y0 = (floor   y0box/(round (cadr align)/MfgGrid))
      x1 = (ceiling x1box/(round (car  align)/MfgGrid))
      y1 = (ceiling y1box/(round (cadr align)/MfgGrid))
      x0 = x0 * (car  align)
      y0 = y0 * (cadr align)
      x1 = x1 * (car  align)
      y1 = y1 * (cadr align)
      cols = (round (x1-x0)/(car  align))
      rows = (round (y1-y0)/(cadr align))
      )

    ; copy drawing, fill shapes at all levels to blockage shapes at top level
    lpp_fill = (list layerName "fill")
    lpp_blk  = (list layerName "blockage")
    lpp_tmp  = (list "y1" "drawing")
    (foreach purpose (list "drawing" "fill" "gnd" "vdd")
             overlaps = (dbGetOverlaps CV CV->bBox (list layerName purpose) levels)
             (foreach x overlaps
                      bbox = (TransformOverlapBBox x)
                      (when bbox
                        bbox = (list (car (car  bbox))-xbloat:(cadr (car  bbox))-ybloat
                                     (car (cadr bbox))+xbloat:(cadr (cadr bbox))+ybloat)
                        (dbCreateRect CV lpp_blk bbox)
                        )
                      )
             )

    ; copy from fill template
    fill_shapes = (setof x fillCV->shapes (and x->layerName==layerName x->purpose=="fill"))
    (for col 0 cols-1
         x=x0+col*(car  align)
         (for row 0 rows-1
              y=y0+row*(cadr align)
              (foreach shape fill_shapes
                       shape = (dbCopyFig shape CV (list x:y "R0"))
                       (dbReplaceProp shape "autogen" "boolean" t)
                       new_fill_shapes = (cons shape new_fill_shapes)
                       )
              )
         )

    ; chop fill template with blockage purpose
    prb = (dbCreatePolygon CV BoundaryLPP (GetPrboundPoints CV))
    (leLayerAndNot CV lpp_fill lpp_blk lpp_tmp)
    new_shapes = (leLayerAnd CV lpp_tmp BoundaryLPP lpp_fill)
    
    ; delete temporary shapes
    (dbDeleteObject prb)
    (foreach x new_fill_shapes (dbDeleteObject x))
    (foreach x (setof x CV->shapes (or x->lpp==lpp_blk x->lpp==lpp_tmp)) (dbDeleteObject x))
    (foreach x new_shapes (dbReplaceProp x "autogen" "boolean" t))
    t
    )
  )

; transform overlap shapes up to a top level bbox
(defun TransformOverlapBBox (a)
  (let (b xy mosaic)
    (cond ((atom a) && (a->objType=="rect" || a->objType=="polygon" ||
                        a->objType=="path" || a->objType=="pathSeg") a->bBox)
          (!(atom a) && (car a)->objType=="inst"
            b = (TransformOverlapBBox (cadr a))
            (if b (dbTransformBBox b (dbGetInstTransform (car a))) nil)
            )
          (!(atom a) && (car a)->objType=="mosaic"
            (printf "WARNING: ignoring shapes in mosaic %s\n" (car a)->name)
            nil
            )
          (t nil)
          )
    )
  )

; fill using a template cell
(defun TemplateFill (@key (CV (geGetEditCellView)) (topMetal 6) (levels 31))
  (when topMetal>=0 (ChopFill "m0" ?CV CV ?xbloat 0.054 ?levels levels))
  (when topMetal>=1 (ChopFill "m1" ?CV CV ?ybloat 0.042 ?levels levels))
  (when topMetal>=2 (ChopFill "m2" ?CV CV ?xbloat 0.056 ?levels levels))
  (when topMetal>=3 (ChopFill "m3" ?CV CV ?ybloat 0.056 ?levels levels))
  (when topMetal>=4 (ChopFill "m4" ?CV CV ?xbloat 0.056 ?levels levels))
  (when topMetal>=5 (ChopFill "m5" ?CV CV ?ybloat 0.056 ?levels levels))
  (when topMetal>=6 (ChopFill "m6" ?CV CV ?xbloat 0.080 ?levels levels))
  CV->modified=t
  CV
  )
