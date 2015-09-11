; Add various overlay mosaics, similar to MakePowerGrid

; Add COLOR_GRID avoiding "color":"block"
(defun MakeColorGrid (@key (CV (geGetEditCellView)))
  (MakeOverlayGrid "color" (list "color" "block")
                   (dbOpenCellViewByType "globals" "globals.COLOR_GRID.m012345_color" "layout")
                   ?CV CV)
  )

; Add CHECK_GRID avoiding "check":"block"
(defun MakeCheckGrid (@key (CV (geGetEditCellView)))
  (MakeOverlayGrid "check" (list "check" "block")
                   (dbOpenCellViewByType "globals" "globals.CHECK_GRID.0" "layout")
                   ?CV CV)
  )

; Add overlay cell with moasics within prBoundary but avoiding blockLPP
(defun MakeOverlayGrid
  (overlayName     ; cellName suffix for overlay
   blockLPP        ; LPP that blocks mosaic
   gridCV          ; CV to build the moasic
   @key (CV (geGetEditCellView)) ; target CV
   )
  (let (TEMP overlayCellName inst overlayCV align prb vertprb
        x1box x2box y1box y2box x1 y1 x2 x2 count rows cols)

    ; generated cell or file names
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    overlayCellName = (strcat CV->cellName "_" overlayName)

    ; delete old overlay grid instance if it exists
    inst = (dbFindAnyInstByName CV overlayName)
    (when inst (dbDeleteObject inst))

    ; clear overlay grid cell
    overlayCV = (dbOpenCellViewByType CV->libName overlayCellName "layout" "maskLayout" "w")
    (unless overlayCV (error "Can't write %s\n " overlayCellName))

    ; get alignment
    align = (list (car  (cadr gridCV->prBoundary->bBox)) - (car  (car gridCV->prBoundary->bBox))
                  (cadr (cadr gridCV->prBoundary->bBox)) - (cadr (car gridCV->prBoundary->bBox)))

    ; get prBoundary boundary shapes
    prb = CV->prBoundary
    (unless prb (error "Bad prBoundary"))
    vertprb = (setof v prb->points (CheckOffGrid v align))
    (when vertprb (error "Not all prBoundary vertices are on grid: %L" vertprb))

    ; get bounding box as an integer multiple of MfgGrid
    x1box = 1.0*(round (leftEdge   prb->bBox)/MfgGrid)
    x2box = 1.0*(round (rightEdge  prb->bBox)/MfgGrid)
    y1box = 1.0*(round (bottomEdge prb->bBox)/MfgGrid)
    y2box = 1.0*(round (topEdge    prb->bBox)/MfgGrid)

    ; minimum sized overlay grid
    x1 = (floor   x1box/(round (car  align)/MfgGrid))
    y1 = (floor   y1box/(round (cadr align)/MfgGrid))
    x2 = (ceiling x2box/(round (car  align)/MfgGrid))
    y2 = (ceiling y2box/(round (cadr align)/MfgGrid))

    ; BUG: temporary hack to avoid Assura layer processing
    prbLPP = (list "prBoundary" "boundary")
    scratchLPP = (list "y2" "drawing")
    (dbCreatePRBoundary overlayCV prb->points)
    prb = (dbCreatePolygon overlayCV prbLPP prb->points)
    (RecursiveFlattenLPP CV overlayCV blockLPP)
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
      y1 = (bottomEdge shape->bBox)
      x2 = (rightEdge  shape->bBox)
      y2 = (topEdge    shape->bBox)
      cols = (round (x2-x1)/(car align))
      rows = (round (y2-y1)/(cadr align))
      (dbCreateSimpleMosaic overlayCV gridCV
                            (sprintf nil "%s%d" overlayName count) x1:y1 "R0"
                            rows cols (cadr align) (car align))
      count = count+1
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

; Copy desired LPP shapes from subcells.  Skip CV's discovered not to have that LPP.
(defun RecursiveFlattenLPP (CV dstCV lpp
                               @key (transform (list 0:0 "R0"))
                                    (emptyCells (makeTable "emptyCells" nil)))
  (let (e empty)
    empty = t
    (foreach lpps (setof x CV->lpps x->layerName==(car lpp) && x->purpose==(cadr lpp))
             (foreach shape lpps->shapes
                      (dbCopyShape shape dstCV transform)
                      empty = nil
                      )
             )
    (foreach inst CV->instances
             (unless emptyCells[inst->master]
               e = (RecursiveFlattenLPP inst->master dstCV lpp
                                        ?transform (dbConcatTransform (dbGetInstTransform inst) transform)
                                        ?emptyCells emptyCells
                                        )
               empty = (and e empty)
               )
             )
    (when empty emptyCells[CV]=t)
    empty
    )
  )
