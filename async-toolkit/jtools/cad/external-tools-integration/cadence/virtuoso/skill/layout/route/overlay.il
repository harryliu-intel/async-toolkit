; Add various overlay mosaics, similar to MakePowerGrid

; Add COLOR_GRID avoiding "color":"block"
(defun MakeColorGrid (@key (CV (geGetEditCellView)) (overlayView "layout"))
  (MakeOverlayGrid "color" overlayView (list "color" "block")
                   (dbOpenCellViewByType "globals" "globals.COLOR_GRID.m012345_color" "layout")
                   ?CV CV)
  )

; Add CHECK_GRID avoiding "check":"block"
(defun MakeCheckGrid (@key (CV (geGetEditCellView)) (overlayView "layout"))
  (MakeOverlayGrid "check" overlayView (list "check" "block")
                   (dbOpenCellViewByType "globals" "globals.CHECK_GRID.0" "layout")
                   ?CV CV)
  )

; Add overlay cell with moasics within prBoundary but avoiding blockLPP
(defun MakeOverlayGrid
  (overlayName     ; cellName suffix for overlay
   overlayView     ; viewName for overlay
   blockLPP        ; LPP that blocks mosaic
   gridCV          ; CV to build the moasic
   @key (CV (geGetEditCellView)) ; target CV
   )
  (let (TEMP overlayCellName inst overlayCV xa ya align prb vertprb
        x1box x2box y1box y2box x1 y1 x2 x2 count rows cols)

    ; generated cell or file names
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    overlayCellName = (strcat CV->cellName "_" overlayName)

    ; delete old overlay grid instance if it exists
    inst = (dbFindAnyInstByName CV overlayName)
    (when inst (dbDeleteObject inst))
    (dbSave CV)

    ; clear overlay grid cell
    overlayCV = (dbOpenCellViewByType CV->libName overlayCellName overlayView "maskLayout" "w")
    (unless overlayCV (error "Can't write %s\n " overlayCellName))

    ; get alignment
    xa = (car  (cadr gridCV->prBoundary->bBox)) - (car  (car gridCV->prBoundary->bBox))
    ya = (cadr (cadr gridCV->prBoundary->bBox)) - (cadr (car gridCV->prBoundary->bBox))
    align = (list xa ya)

    ; get prBoundary boundary shapes
    prb = CV->prBoundary
    (unless prb (error "Bad prBoundary"))
    vertprb = (setof v prb->points (CheckOffGrid v align))
    (when vertprb (error "Not all prBoundary vertices are on grid: %L" vertprb))

    ; Layer processing to merge shapes recursively
    prbLPP = (list "prBoundary" "boundary")
    scratchLPP = (list "y2" "drawing")
    (dbCreatePRBoundary overlayCV prb->points)
    prb = (dbCreatePolygon overlayCV prbLPP prb->points)
    (ICVMergeShapes CV overlayCV (list blockLPP))
    overlay_shapes = (leLayerAndNot overlayCV prbLPP blockLPP scratchLPP)
    (dbLayerTile overlayCV scratchLPP overlay_shapes)

    ; delete temporary layers
    (foreach s overlay_shapes (dbDeleteObject s))
    (foreach s (setof x overlayCV->shapes x->lpp==blockLPP) (dbDeleteObject s))

    ; leave prBoundary as blockLPP
    prb->lpp = blockLPP

    ; replace block shapes with overlay mosaics
    count=0
    (foreach shape (setof s overlayCV->shapes s->lpp==scratchLPP)
      x1 = (leftEdge   shape->bBox)
      x2 = (rightEdge  shape->bBox)
      cols = (round (x2-x1)/xa)
      y1 = (bottomEdge shape->bBox)
      y2 = (topEdge    shape->bBox)

      ; half-height cells on bottom edge
      (when (round y1/ya)*2 != (round y1/(ya/2))
            topCV = (dbOpenCellViewByType gridCV->libName (strcat gridCV->cellName "_top") "layout")
            (unless topCV (error "Couldn't open top overlay"))
            y1 = y1-ya/2
            (dbCreateSimpleMosaic overlayCV topCV
                                  (sprintf nil "%s%d" overlayName count++) x1:y1 "R0" 1 cols ya xa)
            y1 = y1+ya
            )
      
      ; half-height cells on top edge
      (when (round y2/ya)*2 != (round y2/(ya/2))
            y2 = y2-ya/2
            botCV = (dbOpenCellViewByType gridCV->libName (strcat gridCV->cellName "_bot") "layout")
            (unless botCV (error "Couldn't open bot overlay"))
            (dbCreateSimpleMosaic overlayCV botCV
                                  (sprintf nil "%s%d" overlayName count++) x1:y2 "R0" 1 cols ya xa)
            )

      ; full-height cells
      rows = (round (y2-y1)/ya)
      (when rows>0
        (dbCreateSimpleMosaic overlayCV gridCV
                              (sprintf nil "%s%d" overlayName count++) x1:y1 "R0" rows cols ya xa)
        )

      (dbDeleteObject shape)
      )

    ; instatiate overlay, save and return
    (dbCreateInst CV overlayCV overlayName 0:0 "R0")
    (dbSave CV)
    (dbSave overlayCV)
    (dbClose overlayCV)
    overlayCV
    )
  )

; create half cells too
(defun CreateColorGridCells ()
  (let (CV botCV topCV)
    CV = (CreateColorGridCell)
    botCV = (betterCopyCellView CV CV->libName (strcat CV->cellName "_bot") "layout" nil nil t)
    topCV = (betterCopyCellView CV CV->libName (strcat CV->cellName "_top") "layout" nil nil t)
    bbox = (list -1:(cadr gridAlignment[5]) (car gridAlignment[8])+1:(cadr gridAlignment[8])+1)
    (foreach shape botCV->shapes
             (leChopShape shape (RectGetPoints bbox) t t)
             )
    (leChopShape botCV->prBoundary (RectGetPoints bbox) t t)
    (dbSave botCV)
    bbox = (list -1:-1 (car gridAlignment[8])+1:(cadr gridAlignment[5]))
    (foreach shape topCV->shapes
             (leChopShape shape (RectGetPoints bbox) t t)
             )
    (leChopShape topCV->prBoundary (RectGetPoints bbox) t t)
    (dbSave topCV)
    CV
    )
  )

; create a full lego color grid
(defun CreateColorGridCell ()
  (let (CV x y n sort_x sort_y paths)
    CV = (dbOpenCellViewByType "globals" "globals.COLOR_GRID.m012345_color" "layout" "maskLayout" "w")
    x = (car  gridAlignment[8])
    y = (cadr gridAlignment[8])
    (dbCreatePRBoundary CV (list 0:0 x:0 x:y 0:y))

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

    ; vertical layers
    n = (round x/(car gridAlignment[3]))
    (DrawChannelArray nil power_m3 "" 0:n-1 pitch_m3 (list 0:0 0:y) ?CV CV)
    (for i 0 n*tracks_m3-1 (DrawChannel BusMetal23 (NodeChan 3 i) "" (list 0:0 0:y) ?CV CV))

    n = (round x/(car gridAlignment[5]))
    (DrawChannelArray nil power_m5 "" 0:n-1 pitch_m5 (list 0:0 0:y) ?CV CV)
    (for i 0 n*tracks_m5-1 (DrawChannel BusMetal45 (NodeChan 5 i) "" (list 0:0 0:y) ?CV CV))

    ; merge duplicates
    (leMergeShapes CV->shapes)

    ; sort and delete every other track
    sort_x = (lambda (a b) (car  (car a->points))<(car  (car b->points)))
    sort_y = (lambda (a b) (cadr (car a->points))<(cadr (car b->points)))
    paths = (sort (setof y CV->shapes y->layerName=="m0") sort_y)
    (for i 0 (length paths)-1 (ChooseComplementOrBackbone (nth i paths) (mod i 2)==first_color_m0))
    paths = (sort (setof y CV->shapes y->layerName=="m2") sort_y)
    (for i 0 (length paths)-1 (ChooseComplementOrBackbone (nth i paths) (mod i 2)==first_color_m2))
    paths = (sort (setof y CV->shapes y->layerName=="m3") sort_x)
    (for i 0 (length paths)-1 (ChooseComplementOrBackbone (nth i paths) (mod i 2)==first_color_m3))
    paths = (sort (setof y CV->shapes y->layerName=="m4") sort_y)
    (for i 0 (length paths)-1 (ChooseComplementOrBackbone (nth i paths) (mod i 2)==first_color_m4))
    paths = (sort (setof y CV->shapes y->layerName=="m5") sort_x)
    (for i 0 (length paths)-1 (ChooseComplementOrBackbone (nth i paths) (mod i 2)==first_color_m5))

    ; save and return
    (dbSave CV)
    CV
    )
  )

(defun ChooseComplementOrBackbone (shape backbone)
  (cond (backbone shape->lpp = (list (car shape->lpp) "backBone"))
        (shape->layerName=="m0" shape->lpp = (list "mc0" "complement"))
        (shape->layerName=="m2" shape->lpp = (list "mc2" "complement"))
        (shape->layerName=="m3" shape->lpp = (list "mc3" "complement"))
        (shape->layerName=="m4" shape->lpp = (list "mc4" "complement"))
        (shape->layerName=="m5" shape->lpp = (list "mc5" "complement"))
        )
  t
  )
