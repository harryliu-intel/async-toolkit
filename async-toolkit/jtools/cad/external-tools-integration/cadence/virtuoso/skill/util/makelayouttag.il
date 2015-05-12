; Skill to create layout_tag view.  Instantiates layout view with
; optional orientation.  Promotes labels.  Adds power grid and color
; grid.  Labels nwell and psub and topMetal power supplies.
;
; Andrew Lines
;
; $Id$
; $DateTime$

; top-level called by gdsIIWrite.pl
(defun MakeLayoutTagCell (cellName @key
                                   (viewName "layout")
                                   (orientation "R0")
                                   (targetViewName "layout_tag")
                                   (CastPath (ConfigFileGetValue TheCDSConfigTable "CAST_PATH"))
                                   (TempDir  (ConfigFileGetValue TheCDSConfigTable "TEMP"))
                                   (topMetal 8) ; top of power grid in tag view
                                   )
  (let (libName CV tagCV trans ints prb fig align
                gridCV colorCV fillCV power2 prb2 shapes count
                x0box x1box y0box y1box x0 y0 x1 y1
                rows cols frows fcols fx0 fy0 fx1 fy1)
    ; read source CV
    libName = (GetCastLibName cellName)
    CV = (dbOpenCellViewByType libName cellName viewName)
    (unless CV (error "Unable to read %s %s %s\n" libName cellName viewName))

    ; write destination tagCV
    tagCV = (dbOpenCellViewByType libName cellName targetViewName "maskLayout" "w") 
    (unless tagCV (error "Unable to write %s %s %s\n" libName cellName targetViewName))

    ; instantiate layout cell
    trans = (list 0:0 orientation 1.0)
    inst = (dbCreateInst tagCV CV "inst_layout" 0:0 orientation);

    ; copy prb
    prb = (dbCreatePRBoundary tagCV CV->prBoundary->points)

    ; copy non-power labels
    (foreach label (setof l CV->shapes l->objType=="label" && l->theLabel!=GNDNetName && l->theLabel!=VddNetName)
             fig = (dbCopyFig label tagCV trans)
             ;(dbCreateRect tagCV label->lpp
             ;              (list (car fig->xy)-MfgGrid:(cadr fig->xy)-MfgGrid 
             ;                    (car fig->xy)+MfgGrid:(cadr fig->xy)+MfgGrid))
             )

    ; get bounding box as an integer multiple of MfgGrid
    x0box = 1.0*(round (leftEdge   prb->bBox)/MfgGrid)
    x1box = 1.0*(round (rightEdge  prb->bBox)/MfgGrid)
    y0box = 1.0*(round (bottomEdge prb->bBox)/MfgGrid)
    y1box = 1.0*(round (topEdge    prb->bBox)/MfgGrid)

    ; top-metal grid
    align = gridAlignment[topMetal]
    x0 = (floor   x0box/(round (car  align)/MfgGrid))
    y0 = (floor   y0box/(round (cadr align)/MfgGrid))
    x1 = (ceiling x1box/(round (car  align)/MfgGrid))
    y1 = (ceiling y1box/(round (cadr align)/MfgGrid))
    x0 = (x0-1) * (car  align)
    y0 = (y0-1) * (cadr align)
    x1 = (x1+1) * (car  align)
    y1 = (y1+1) * (cadr align)
    cols = (round (x1-x0)/(car  align))
    rows = (round (y1-y0)/(cadr align))

    ; add power grid
    gridCV = (dbOpenCellViewByType "globals" "globals.POWER_GRID.m2345678_pg" "layout")
    (dbCreateSimpleMosaic tagCV gridCV "pg" x0:y0 "R0" rows cols (cadr align) (car align))

    ; add color grid
    colorCV = (dbOpenCellViewByType "globals" "globals.COLOR_GRID.m012345_color" "layout")
    (dbCreateSimpleMosaic tagCV colorCV "cg" x0:y0 "R0" rows cols (cadr align) (car align))

    ; label topMetal power supplies
    (for r 0 rows-1
         y = y0+(cadr align)*r
         (CreatePowerPins tagCV x0:y topMetal)
         )

    ; switch to lower layer alignment grid
    align = gridAlignment[2]
    cols = (round (x1-x0)/(car  align))
    rows = (round (y1-y0)/(cadr align))

    ; add fill cells (TOOD: refactor this code with skill/layout/route/flatten.il perhaps)
    power2 = (dbCreateRect tagCV (list Metal[8] "block") (list x0:y0 x1:y1))
    prb2   = (dbCreatePolygon tagCV (list "prBoundary" "boundary") prb->points)
    shapes = (leLayerAndNot tagCV (list Metal[8] "block") (list "prBoundary" "boundary") (list Metal[2] "block"))
    (dbLayerTile tagCV (list Metal[2] "block") shapes)
    (dbDeleteObject prb2)
    (dbDeleteObject power2)
    (foreach obj shapes (dbDeleteObject obj))
    fillCV = (nrOpenCellViewReadable "globals" "globals.POWER_GRID.fill_pg" "layout")
    count = 0
    (foreach shape (setof s tagCV->shapes s->lpp==(list Metal[2] "block"))
             fx0 = (leftEdge   shape->bBox)
             fy0 = (bottomEdge shape->bBox)
             fx1 = (rightEdge  shape->bBox)
             fy1 = (topEdge    shape->bBox)
             fcols = (round (fx1-fx0)/(car  align))
             frows = (round (fy1-fy0)/(cadr align))
             (dbCreateSimpleMosaic tagCV fillCV
                                   (sprintf nil "fill%d" count) fx0:fy0 "R0"
                                   frows fcols (cadr align) (car align))
             count = count+1
             (dbDeleteObject shape)
             )

    ; label GND substrate (TODO: using pdkinfo.il)
    (for r 0 rows
         y = y0+(cadr align)*r
         (dbCreateLabel tagCV (list "pwellSubIso" "id") x0:y GNDNetName "centerCenter" "R0" "stick" 50*MfgGrid)
         )

    ; label Vdd substrate (TODO: using pdkinfo.il)
    (for r 0 rows-1
         y = y0+(cadr align)*(r+0.5)
         (dbCreateLabel tagCV (list "nwell" "drawing") x0:y VddNetName "centerCenter" "R0" "stick" 50*MfgGrid)
         )

    ; TODO: metal fill

    ; finish
    (dbSave tagCV)
    tagCV
    )
  )
