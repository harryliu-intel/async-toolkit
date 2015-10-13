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
                          (flatCV nil)
                          )
  (let (layerName cellName mod2 fillCV lpp_fill lpp_blk lpp_tmp rect bbox shapes new_shapes
        overlaps fill_shapes new_fill_shapes purposes
        x y x0 y0 x1 y1 x0box y0box x1box y1box cols rows align w h l del xb yb
        leftOverlaps rightOverlaps topOverlaps botOverlaps
        ) 
    ; options
    (unless flatCV flatCV=(MakeFlatView ?CV CV))
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
      cellName = "globals.FILL_TEMPLATE.m012345678_fill"
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
    purposes = (list "drawing" "fill" "gnd" "vdd")
    (foreach s flatCV->shapes
             (when s->objType!="label" && s->layerName==layerName && (member s->purpose purposes)!=nil
                   (dbCreateRect CV lpp_blk s->bBox)
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
                           (betterGrowPoints (GetPrboundPoints CV)
                                             (list -(car  MetalBoundarySpace[layerName])
                                                   -(cadr MetalBoundarySpace[layerName]))))
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
             leftOverlaps  = (dbGetOverlaps CV (list (leftEdge s->bBox):(bottomEdge s->bBox)
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
            (mod2==0 && leftOverlaps && !rightOverlaps && w>=l+xb
                     x0=xbloat
                     )
            (mod2==0 && !leftOverlaps && rightOverlaps && w>=l+xb
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
            (mod2==1 && botOverlaps && !topOverlaps && h>=l+yb
                     y0=ybloat
                     )
            (mod2==1 && !botOverlaps && topOverlaps && h>=l+yb
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
          (!(atom a) && ((car a)->objType=="inst" || (car a)->objType=="stdVia")
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
(defun TemplateFill (@key (CV (geGetEditCellView)) (botMetal 0) (topMetal 8))
  (let (flatCV)
    (DeleteAutoGen ?propName "TemplateFill" ?CV CV)
    flatCV = (MakeFlatView ?CV CV)
    (when topMetal>=0 && 0>=botMetal (ChopFill 0 ?CV CV ?flatCV flatCV))
    (when topMetal>=1 && 1>=botMetal (ChopFill 1 ?CV CV ?flatCV flatCV))
    (when topMetal>=2 && 2>=botMetal (ChopFill 2 ?CV CV ?flatCV flatCV))
    (when topMetal>=3 && 3>=botMetal (ChopFill 3 ?CV CV ?flatCV flatCV))
    (when topMetal>=4 && 4>=botMetal (ChopFill 4 ?CV CV ?flatCV flatCV))
    (when topMetal>=5 && 5>=botMetal (ChopFill 5 ?CV CV ?flatCV flatCV))
    (when topMetal>=6 && 6>=botMetal (ChopFill 6 ?CV CV ?flatCV flatCV))
    (when topMetal>=7 && 7>=botMetal (ChopFill 7 ?CV CV ?flatCV flatCV))
    (when topMetal>=8 && 8>=botMetal (ChopFill 8 ?CV CV ?flatCV flatCV))
    (ddDeleteObj (ddGetObj flatCV->libName flatCV->cellName flatCV->viewName))
    CV->modified=t
    CV
    )
  )

; TODO: with ICV layer processing
(defun MakeFlatView (@key (CV (geGetEditCellView)))
  CV = (betterCopyCellView CV CV->libName CV->cellName "flat" nil nil t)
  (foreach x CV->instances (dbFlattenInst x 31 t nil nil))
  (foreach x CV->vias (dbFlattenInst x 31 t nil nil))
  (dbSave CV)
  CV
  )

; utility to draw the fill shapes
(defun CreateFillTemplateCell ()
  (let (CV x y n)
    CV = (dbOpenCellViewByType "globals" "globals.FILL_TEMPLATE.m012345678_fill" "layout" "maskLayout" "w")
    x = (car  gridAlignment[8])
    y = (cadr gridAlignment[8])
    
    ; horizontal layers
    n = (round y/(cadr gridAlignment[0]))
    (DrawChannelArray nil power_m0 "" 0:n-1 pitch_m0 (list 0:0 x:0) ?CV CV)
    (for i 0 n*tracks_m0-1 (DrawChannel BusMetal01 (NodeChan 0 i) "" (list 0:0 x:0) ?CV CV))

    n = (round y/(cadr gridAlignment[2]))
    (DrawChannelArray nil power_m2 "" 0:n-1 pitch_m2 (list 0:0 x:0) ?CV CV)
    (for i 0 n*tracks_m2-1 (DrawChannel BusMetal23 (NodeChan 2 i) "" (list 0:0 x:0) ?CV CV))

    n = (round y/(cadr gridAlignment[4]))
    (DrawChannelArray nil power_m4 "" 0:n-1 pitch_m4 (list 0:0 x:0) ?CV CV)
    (for i 0 n*tracks_m4-1 (DrawChannel BusMetal45 (NodeChan 4 i) "" (list 0:0 x:0) ?CV CV))

    n = (round y/(cadr gridAlignment[6]))
    (DrawChannelArray nil power_m6 "" 0:n-1 pitch_m6 (list 0:0 x:0) ?CV CV)
    (for i 0 n*tracks_m6-1 (DrawChannel BusMetal67 (NodeChan 6 i) "" (list 0:0 x:0) ?CV CV))

    n = (round y/(cadr gridAlignment[8]))
    (DrawChannelArray nil power_m8 "" 0:n-1 pitch_m8 (list 0:0 x:0) ?CV CV)
    (for i 0 n*tracks_m8-1 (DrawChannel BusMetal78 (NodeChan 8 i) "" (list 0:0 x:0) ?CV CV))

    ; vertical layers
    n = (round x/(car gridAlignment[1]))
    (for i 0 n*tracks_m1 (DrawChannel BusMetal01 (NodeChan 1 i) "" (list 0:0 0:y) ?CV CV))

    n = (round x/(car gridAlignment[3]))
    (DrawChannelArray nil power_m3 "" 0:n-1 pitch_m3 (list 0:0 0:y) ?CV CV)
    (for i 0 n*tracks_m3-1 (DrawChannel BusMetal23 (NodeChan 3 i) "" (list 0:0 0:y) ?CV CV))

    n = (round x/(car gridAlignment[5]))
    (DrawChannelArray nil power_m5 "" 0:n-1 pitch_m5 (list 0:0 0:y) ?CV CV)
    (for i 0 n*tracks_m5-1 (DrawChannel BusMetal45 (NodeChan 5 i) "" (list 0:0 0:y) ?CV CV))

    n = (round x/(car gridAlignment[7]))
    (DrawChannelArray nil power_m7  "" 0:n-1 pitch_m7 (list 0:0 0:y) ?CV CV)
    (for i 0 n*tracks_m7-1  (DrawChannel BusMetal67 (NodeChan 7 i) "" (list 0:0 0:y) ?CV CV))

    ; convert all shapes to fill purpose and return CV
    (foreach x CV->shapes x->purpose="fill")
    (leMergeShapes CV->shapes)
    (dbSave CV)
    CV
    )
  )
