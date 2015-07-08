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
(defun ChopFill (layerNum @key
                          (CV (geGetEditCellView))
                          (xbloat nil)
                          (ybloat nil)
                          (levels 31)
                          )
  (let (layerName cellName mod2 fillCV lpp_fill lpp_blk lpp_tmp rect bbox shapes new_shapes
        overlaps fill_shapes new_fill_shapes
        x y x0 y0 x1 y1 x0box y0box x1box y1box cols rows align w h l del xb yb
        leftOverlaps rightOverlaps topOverlaps botOverlaps
        ) 
    ; options
    mod2 = (mod layerNum 2)
    layerName = Metal[layerNum]
    (unless xbloat xbloat = (car  MetalSpace[layerName])*(1-mod2))
    (unless ybloat ybloat = (cadr MetalSpace[layerName])*mod2)

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
                      (when bbox (dbCreateRect CV lpp_blk bbox))
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
                       new_fill_shapes = (cons shape new_fill_shapes)
                       )
              )
         )

    ; chop fill template with blockage purpose
    prb = (dbCreatePolygon CV BoundaryLPP
                           (betterGrowPoints (GetPrboundPoints CV) -xbloat/2:-ybloat/2))
    (leLayerAndNot CV lpp_fill lpp_blk lpp_tmp)
    new_shapes = (leLayerAnd CV lpp_tmp BoundaryLPP lpp_fill)

    ; trim fill shapes
    (foreach s new_shapes
      w=0
      h=0
      l=MetalMinLength[layerName]
      x0=0
      x1=0
      y0=0
      y1=0
      del=nil
      (cond (mod2==0
             w=(rightEdge s->bBox)-(leftEdge s->bBox)
             leftOverlaps = (dbGetOverlaps CV (list (leftEdge s->bBox):(bottomEdge s->bBox)
                                                    (leftEdge s->bBox):(topEdge s->bBox)) lpp_blk)
             rightOverlaps = (dbGetOverlaps CV (list (rightEdge s->bBox):(bottomEdge s->bBox)
                                                     (rightEdge s->bBox):(topEdge s->bBox)) lpp_blk)
             )
            (t
             h=(topEdge s->bBox)-(bottomEdge s->bBox)
             botOverlaps = (dbGetOverlaps CV (list (leftEdge s->bBox):(bottomEdge s->bBox)
                                                   (rightEdge s->bBox):(bottomEdge s->bBox)) lpp_blk)
             topOverlaps = (dbGetOverlaps CV (list (leftEdge s->bBox):(topEdge s->bBox)
                                                   (rightEdge s->bBox):(topEdge s->bBox)) lpp_blk)
             )
            )
      xb = (round xbloat/MfgGrid)
      yb = (round ybloat/MfgGrid)
      w = (round w/MfgGrid)
      h = (round h/MfgGrid)
      l = (round l/MfgGrid)
      (cond (mod2==0 && !leftOverlaps && !rightOverlaps
                     t
                     )
            (mod2==0 && leftOverlaps && rightOverlaps && w>=l+2*xb
                     x0=xbloat x1=-xbloat
                     )
            (mod2==0 && leftOverlaps && rightOverlaps && w>xb
                     x0=xbloat
                     )
            (mod2==0 && leftOverlaps && !rightOverlaps && w>l+xb
                     x0=xbloat
                     )
            (mod2==0 && !leftOverlaps && rightOverlaps && w>l+xb
                     x1=-xbloat
                     )
            (mod2==0 && (leftOverlaps!=nil)!=(rightOverlaps!=nil)
                     t
                     )
            (mod2==1 && !botOverlaps && !topOverlaps
                     t
                     )
            (mod2==1 && botOverlaps && topOverlaps && h>=l+2*yb
                     y0=ybloat y1=-ybloat
                     )
            (mod2==1 && botOverlaps && topOverlaps && h>yb
                     y0=ybloat
                     )
            (mod2==1 && botOverlaps && !topOverlaps && h>l+yb
                     y0=ybloat
                     )
            (mod2==1 && !botOverlaps && topOverlaps && h>l+yb
                     y1=-ybloat
                     )
            (mod2==1 && (botOverlaps!=nil)!=(topOverlaps!=nil)
                     t
                     )
            (t
             del=t
             )
            )
      (cond (del (dbDeleteObject s))
            (t
             s->bBox = (list (leftEdge  s->bBox)+x0:(bottomEdge s->bBox)+y0
                             (rightEdge s->bBox)+x1:(topEdge    s->bBox)+y1)
             (dbReplaceProp s "TemplateFill" "boolean" t)
             )
            )
      )
    
    ; delete temporary shapes
    (dbDeleteObject prb)
    (foreach x new_fill_shapes (dbDeleteObject x))
    (foreach x (setof x CV->shapes (or x->lpp==lpp_blk x->lpp==lpp_tmp)) (dbDeleteObject x))
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
  (DeleteAutoGen ?propName "TemplateFill")
  (when topMetal>=0 (ChopFill 0 ?CV CV ?levels levels))
  (when topMetal>=1 (ChopFill 1 ?CV CV ?levels levels))
  (when topMetal>=2 (ChopFill 2 ?CV CV ?levels levels))
  (when topMetal>=3 (ChopFill 3 ?CV CV ?levels levels))
  (when topMetal>=4 (ChopFill 4 ?CV CV ?levels levels))
  (when topMetal>=5 (ChopFill 5 ?CV CV ?levels levels))
  (when topMetal>=6 (ChopFill 6 ?CV CV ?levels levels))
  CV->modified=t
  CV
  )
