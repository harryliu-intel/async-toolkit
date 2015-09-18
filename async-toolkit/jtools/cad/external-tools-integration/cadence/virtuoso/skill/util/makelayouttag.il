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
                                   (topMetal nil) ; top of power grid in tag view
                                   )
  (let (libName CV tagCV powerCV trans ints prb align lpp
                gridCV colorCV checkCV fillCV power2 prb2 shapes count
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
    inst = (dbCreateInst tagCV CV "inst_layout" 0:0 orientation)

    ; copy prb
    (unless CV->prBoundary->points (error "No prBoundary"))
    prb = (dbCreatePRBoundary tagCV CV->prBoundary->points)

    ; copy non-power labels
    (foreach label (setof l CV->shapes l->objType=="label" && l->theLabel!=GNDNetName && l->theLabel!=VddNetName)
             lpp = (list (car label->lpp) "pin")
             (MakeLayoutTagLabel tagCV lpp (dbTransformPoint label->xy trans) label->theLabel)
             )

    ; get bounding box as an integer multiple of MfgGrid
    x0box = 1.0*(round (leftEdge   prb->bBox)/MfgGrid)
    x1box = 1.0*(round (rightEdge  prb->bBox)/MfgGrid)
    y0box = 1.0*(round (bottomEdge prb->bBox)/MfgGrid)
    y1box = 1.0*(round (topEdge    prb->bBox)/MfgGrid)

    ; power grid
    (unless topMetal topMetal = (dbGetPropByName CV "topMetal")->value)
    (unless topMetal topMetal = 2)
    powerCV = (MakePowerGrid ?CV tagCV ?topMetal topMetal ?powerCellView "layout_tag")

    ; align to topMetal grid for power pins
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

    ; label topMetal power supplies
    (for r 0 rows-1
         y = y0+(cadr align)*r
         (MakeLayoutTagPowerPins tagCV (list x0:y x1:y) topMetal)
         )

    ; align to m2 grid for filler cells
    align = gridAlignment[2]
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

    ; add fill cells (TODO: refactor this code with skill/layout/route/overlay.il perhaps)
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
         (MakeLayoutTagLabel tagCV (list "pwellSubIso" "id") x0+(car align)/2:y GNDNetName ?addrect nil)
         )

    ; label Vdd substrate (TODO: using pdkinfo.il)
    (for r 0 rows-1
         y = y0+(cadr align)*(r+0.5)
         (MakeLayoutTagLabel tagCV (list "nwell" "drawing") x0+(car align)/2:y VddNetName)
         )

    ; align to m8 for check and color grids
    align = gridAlignment[8]
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

    ; expand prBoundary
    (dbDeleteObject prb)
    prb = (dbCreatePRBoundary tagCV (list x0:y0 x1:y0 x1:y1 x0:y1))

    ;  add color and check grids
    colorCV = (MakeColorGrid ?CV tagCV ?overlayView "layout_tag")
    checkCV = (MakeCheckGrid ?CV tagCV ?overlayView "layout_tag")

    ; metal fill
    ; (TemplateFill ?CV tagCV ?topMetal topMetal)

    ; finish
    (dbSave tagCV)
    tagCV
    )
  )

; create a label on an optional tiny rectangle
(defun MakeLayoutTagLabel (cv lpp xy label @key (addrect t))
  (let (x y)
    x=(car xy)
    y=(cadr xy)
    (dbCreateLabel cv lpp xy label "centerCenter" "R0" "stick" 50*MfgGrid)
    (when addrect
      (dbCreatePin (dbMakeNet cv label)
                   (dbCreateRect cv lpp (list x-MfgGrid:y-MfgGrid x+MfgGrid:y+MfgGrid)))
      )
    )
  )

; Draw Vdd/GND pins as stripes on highest even metal layer
(defun MakeLayoutTagPowerPins (CV points topMetal)
  (let (pattern)
    (cond (topMetal==2 || topMetal==3 pattern=power_m2)
          (topMetal==4 || topMetal==5 pattern=power_m4)
          (topMetal==6 || topMetal==7 pattern=power_m6)
          (topMetal==8                pattern=power_m8)
          (t (error "Requires 2<=topMetal<=8"))
          )
    (DrawChannel nil pattern "" points ?isPin t ?CV CV)
    (foreach shape CV->shapes
             (when shape->purpose=="gnd" || shape->purpose=="vdd"
                   shape->lpp=(list (car shape->lpp) "pin") ; hack
                   (dbReplaceProp shape "PinType" "string" "Power")
                   )
             )
    )
  t
  )
