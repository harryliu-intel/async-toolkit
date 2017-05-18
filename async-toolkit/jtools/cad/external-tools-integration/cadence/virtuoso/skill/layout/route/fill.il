; Delete shapes with autogen=t property
(defun DeleteAutoGen (@key (CV (geGetEditCellView)) (propName "autogen"))
  (foreach obj (setof x CV->shapes (dbGetPropByName x propName)->value)
       (dbDeleteObject obj)
       )
  (foreach obj (setof x CV->vias (dbGetPropByName x propName)->value)
       (dbDeleteObject obj)
       )
  (foreach obj (setof x CV->instances (dbGetPropByName x propName)->value)
       (dbDeleteObject obj)
       )
  )

; Chop fill to avoid other purposes
(defun ChopFill (layerNum @key
                          (CV (geGetEditCellView))
                          (xbloat nil)
                          (ybloat nil)
                          (flatCV nil)
                          (chopAtBoundary t)
                          (ignoreFill nil)
                          )
  (let (layerName cellName mod2 fillCV lpp_fill lpp_blk rect bbox
        overlaps fill_shapes mc sp
        x y x0 y0 x1 y1 x0box y0box x1box y1box cols rows align w h l del xb yb
        leftOverlaps rightOverlaps topOverlaps botOverlaps
        ) 
    ; options
    (unless flatCV flatCV=(MakeFlatBlockageView ?botMetal layerNum ?topMetal layerNum
                                                ?CV CV ?ignoreFill ignoreFill))
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
    (foreach s flatCV->shapes (when s->lpp==lpp_blk (dbCopyFig s CV (list 0:0 "R0"))))

    ; protect old fill
    (foreach x CV->shapes
             (when x->lpp==lpp_fill
               x->lpp=(list (car x->lpp) "drawing")
               (dbReplaceProp x "protectFill" "boolean" t)
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
                       )
              )
         )

    ; chop fill template with blockage purpose
    fill_shapes = (setof x CV->shapes x->lpp==lpp_fill)
    (leLayerAndNot CV lpp_fill lpp_blk lpp_fill)
    (foreach x fill_shapes (dbDeleteObject x))

    ; trim fill shapes
    (foreach s (setof x CV->shapes x->lpp==lpp_fill)
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
             )
            )
      )
    
    ; chop fill with shrunken prBoundary
    prb = (dbCreatePolygon CV BoundaryLPP
                           (betterGrowPoints (GetPrboundPoints CV)
                                             (list -(car  MetalBoundarySpace[layerName])
                                                   -(cadr MetalBoundarySpace[layerName]))))
    (when chopAtBoundary
      fill_shapes = (setof x CV->shapes x->lpp==lpp_fill)
      (leLayerAnd CV lpp_fill BoundaryLPP lpp_fill)
      (foreach x fill_shapes (dbDeleteObject x))
      )

    ; set TemplateFill property on new fill shapes
    fill_shapes = (setof x CV->shapes x->lpp==lpp_fill)
    (foreach x fill_shapes (dbReplaceProp x "TemplateFill" "boolean" t))

    ; restore protectFill
    (foreach x CV->shapes
             (when (dbGetPropByName x "protectFill")->value
                   x->lpp=(list (car x->lpp) "fill")
                   (dbDeletePropByName x "protectFill")
                   )
             )

    ; delete temporary shapes
    (foreach x CV->shapes (when x->lpp==lpp_blk (dbDeleteObject x)))
    (dbDeleteObject prb)
    t
    )
  )

; fill using a template cell
(defun TemplateFill (@key (CV (geGetEditCellView)) (botMetal 0) (topMetal TopMetal) (deleteOld t)
                          (chopAtBoundary t) (ignoreFill nil))
  (let (flatCV)
    (when deleteOld (DeleteAutoGen ?propName "TemplateFill" ?CV CV))
    flatCV = (MakeFlatBlockageView ?botMetal botMetal ?topMetal topMetal ?CV CV ?ignoreFill ignoreFill)
    (for m botMetal topMetal (ChopFill m ?CV CV ?flatCV flatCV ?chopAtBoundary chopAtBoundary))
    (ddDeleteObj (ddGetObj flatCV->libName flatCV->cellName flatCV->viewName))
    CV->modified=t
    CV
    )
  )

; Flatten all real metal shapes to blockage purpose
(defun MakeFlatBlockageView (@key (botMetal 0) (topMetal TopMetal)
                                  (CV (geGetEditCellView)) (ignoreFill nil))
  (let (inLayerMap outLayerMap purposes flatCV name map) 
    flatCV = (dbOpenCellViewByType CV->libName CV->cellName "flat_blockage" "maskLayout" "w")
    (for m botMetal topMetal
         name = (sprintf nil "m%d" m)
         outLayerMap = (cons (list name (list Metal[m] "blockage")) outLayerMap)
         map = (list name)
         purposes = (list "drawing" "gnd" "vdd")
         (unless ignoreFill purposes = (cons "fill" purposes))
         (foreach purpose purposes
                  map = (append map (list (list name purpose)))
                  )
         (when m==0 || m>=2 && m<=5
               name = (sprintf nil "mc%d" m)
               (foreach purpose purposes
                        map = (append map (list (list name purpose)))
                        )
               )
         inLayerMap = (cons map inLayerMap)
         )
    (ICVLayerProcessorSimple CV flatCV "mergeShape" inLayerMap outLayerMap)
    (dbSave flatCV)
    flatCV
    )
  )

; utility to draw the fill shapes
(defun CreateFillTemplateCell (@key (powerTracks nil))
  (let (CV x y n)
    CV = (dbOpenCellViewByType "globals" "globals.FILL_TEMPLATE.m012345678_fill" "layout" "maskLayout" "w")
    x = (car  gridAlignment[8])
    y = (cadr gridAlignment[8])
    
    ; horizontal layers
    n = (round y/(cadr gridAlignment[0]))
    (when powerTracks (DrawChannelArray nil power_m0 "" 0:n-1 pitch_m0 (list 0:0 x:0) ?CV CV))
    (for i 0 n*tracks_m0-1 (DrawChannel BusMetal01 (NodeChan 0 i) "" (list 0:0 x:0) ?CV CV))

    n = (round y/(cadr gridAlignment[2]))
    (when powerTracks (DrawChannelArray nil power_m2 "" 0:n-1 pitch_m2 (list 0:0 x:0) ?CV CV))
    (for i 0 n*tracks_m2-1 (DrawChannel BusMetal23 (NodeChan 2 i) "" (list 0:0 x:0) ?CV CV))

    n = (round y/(cadr gridAlignment[4]))
    (when powerTracks (DrawChannelArray nil power_m4 "" 0:n-1 pitch_m4 (list 0:0 x:0) ?CV CV))
    (for i 0 n*tracks_m4-1 (DrawChannel BusMetal45 (NodeChan 4 i) "" (list 0:0 x:0) ?CV CV))

    n = (round y/(cadr gridAlignment[6]))
    (when powerTracks (DrawChannelArray nil power_m6 "" 0:n-1 pitch_m6 (list 0:0 x:0) ?CV CV))
    (for i 0 n*tracks_m6-1 (DrawChannel BusMetal67 (NodeChan 6 i) "" (list 0:0 x:0) ?CV CV))

    n = (round y/(cadr gridAlignment[8]))
    (when powerTracks (DrawChannelArray nil power_m8 "" 0:n-1 pitch_m8 (list 0:0 x:0) ?CV CV))
    (for i 0 n*tracks_m8-1 (DrawChannel BusMetal78 (NodeChan 8 i) "" (list 0:0 x:0) ?CV CV))

    ; vertical layers
    n = (round x/(car gridAlignment[1]))
    (for i 0 n*tracks_m1 (DrawChannel BusMetal01 (NodeChan 1 i) "" (list 0:0 0:y) ?CV CV))

    n = (round x/(car gridAlignment[3]))
    (when powerTracks (DrawChannelArray nil power_m3 "" 0:n-1 pitch_m3 (list 0:0 0:y) ?CV CV))
    (for i 0 n*tracks_m3-1 (DrawChannel BusMetal23 (NodeChan 3 i) "" (list 0:0 0:y) ?CV CV))

    n = (round x/(car gridAlignment[5]))
    (when powerTracks (DrawChannelArray nil power_m5 "" 0:n-1 pitch_m5 (list 0:0 0:y) ?CV CV))
    (for i 0 n*tracks_m5-1 (DrawChannel BusMetal45 (NodeChan 5 i) "" (list 0:0 0:y) ?CV CV))

    n = (round x/(car gridAlignment[7]))
    (when powerTracks (DrawChannelArray nil power_m7  "" 0:n-1 pitch_m7 (list 0:0 0:y) ?CV CV))
    (for i 0 n*tracks_m7-1  (DrawChannel BusMetal67 (NodeChan 7 i) "" (list 0:0 0:y) ?CV CV))

    ; convert all shapes to fill purpose and return CV
    (foreach x CV->shapes x->purpose="fill")
    (leMergeShapes CV->shapes)
    (dbSave CV)
    CV
    )
  )
