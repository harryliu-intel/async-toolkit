; Skill to create layout_tag view.  Instantiates layout view with
; optional orientation.  Promotes labels.  Adds power grid and color
; grid.  Labels nwell and psub and topMetal power supplies.
;
; Andrew Lines
;
; $Id$
; $DateTime$

; convenience wrapper
(defun MakeLayoutTag (@key (CV (geGetEditCellView))
                           (botMetal 0) (midMetal nil) (topMetal TopMetal) (doFill nil) (doHalo nil))
  (MakeLayoutTagCell CV->cellName ?doFill doFill ?botMetal botMetal ?midMetal midMetal ?topMetal topMetal
                     ?doHalo doHalo)
  )

; top-level called by gdsIIWrite.pl
(defun MakeLayoutTagCell (cellName @key
                                   (viewName "layout")
                                   (orientation "R0")
                                   (targetViewName "layout_tag")
                                   (CastPath (ConfigFileGetValue TheCDSConfigTable "CAST_PATH"))
                                   (TempDir  (ConfigFileGetValue TheCDSConfigTable "TEMP"))
                                   (botMetal 0) ; used for TemplateFill
                                   (midMetal nil) ; used for TemplateFill
                                   (topMetal TopMetal)
                                   (doPower t)
                                   (doCheck t)
                                   (doColor t)
                                   (doFill nil)
                                   (doHalo nil)
                                   )
  (let (libName CV tagCV trans prb lpp)
    ; read source CV
    libName = (GetCastLibName cellName)
    CV = (dbOpenCellViewByType libName cellName viewName)
    (unless CV (error "Unable to read %s %s %s\n" libName cellName viewName))

    ; write destination tagCV
    tagCV = (dbOpenCellViewByType libName cellName targetViewName "maskLayout" "w") 
    (unless tagCV (error "Unable to write %s %s %s\n" libName cellName targetViewName))

    ; instantiate layout cell
    trans = (list 0:0 orientation 1.0)
    (dbCreateInst tagCV CV "inst_layout" 0:0 orientation)

    ; copy prb
    (unless CV->prBoundary->points (error "No prBoundary"))
    prb = (dbCreatePRBoundary tagCV CV->prBoundary->points)

    ; copy non-power labels
    (foreach label (setof l CV->shapes l->objType=="label" &&
                          l->theLabel!=GNDNetName && l->theLabel!=VddNetName)
             lpp = (list (car label->lpp) "pin")
             (MakeLayoutTagLabel tagCV lpp (dbTransformPoint label->xy trans) label->theLabel)
             )

    ; either add CHIP_HALO or a bunch of filler cells
    (cond ((HasTopHalo ?CV tagCV)
           (AddChipHalo ?CV tagCV)
           doPower=nil
           doColor=nil
           doFill=nil
           )
          (t
           (FillTagCell ?CV tagCV ?topMetal topMetal ?doHalo doHalo)
           )
          )

    ;  add power, color, check grids
    (when doPower (MakePowerGrid ?CV tagCV ?topMetal topMetal ?powerCellView "layout_tag"))
    (when doColor (MakeColorGrid ?CV tagCV ?overlayView "layout_tag"))
    (when doCheck (MakeCheckGrid ?CV tagCV ?overlayView "layout_tag"))

    ;  add metal fill
    (cond (doFill && midMetal!=nil
           ; fill above CV's topMetal everywhere
           (TemplateFill ?CV tagCV ?botMetal midMetal+1 ?topMetal topMetal)
           ; fill outside CV at or below topMetal
           (leChopShape tagCV->prBoundary CV->prBoundary->points t t)
           (TemplateFill ?CV tagCV ?botMetal botMetal ?topMetal midMetal ?deleteOld nil)
           )
          (doFill && midMetal==nil
           ; just fill all layers from botMetal to topMetal
           (TemplateFill ?CV tagCV ?botMetal botMetal ?topMetal topMetal)
           )
          )

    ; finish
    (dbSave tagCV)
    tagCV
    )
  )

; Add d04 filler cells to a tag view outside the prBoundary
(defun FillTagCell (@key (CV geGetEditCellView) (topMetal TopMetal) (doHalo nil))
  (let (prb prb2 x0box x1box y0box y1box align x0 y0 x1 y1 ox0 ox1 y
            cols rows power2 shapes hookCV count fx0 fy0 fx1 fy1 frows)

    ; get prBoundary
    prb = CV->prBoundary
    prb2 = (dbCreatePolygon CV (list "prBoundary" "boundary") prb->points)

    ; get bounding box as an integer multiple of MfgGrid
    x0box = 1.0*(round (leftEdge   prb->bBox)/MfgGrid)
    x1box = 1.0*(round (rightEdge  prb->bBox)/MfgGrid)
    y0box = 1.0*(round (bottomEdge prb->bBox)/MfgGrid)
    y1box = 1.0*(round (topEdge    prb->bBox)/MfgGrid)

    ; align to m8 for power, color, check grids
    align = gridAlignment[topMetal]
    x0 = (floor   x0box/(round (car  align)/MfgGrid))
    y0 = (floor   y0box/(round (cadr align)/MfgGrid))
    x1 = (ceiling x1box/(round (car  align)/MfgGrid))
    y1 = (ceiling y1box/(round (cadr align)/MfgGrid))
    ox0 = x0 * (car align)
    ox1 = x1 * (car align)
    x0 = (x0-1) * (car  align)
    y0 = (y0-1) * (cadr align)
    x1 = (x1+1) * (car  align)
    y1 = (y1+1) * (cadr align)
    cols = (round (x1-x0)/(car  align))
    rows = (round (y1-y0)/(cadr align))

    ; expand prBoundary
    (dbDeleteObject prb)
    prb = (dbCreatePRBoundary CV (list x0:y0 x1:y0 x1:y1 x0:y1))

    ; label topMetal power supplies
    (for r 0 rows-1
         y = y0+(cadr align)*r
         (MakeLayoutTagPowerPins CV (list x0:y ox0:y) topMetal)
         (MakeLayoutTagPowerPins CV (list ox1:y x1:y) topMetal)
         )

    ; align to m2 grid for filler cells
    align = gridAlignment[2]
    cols = (round (x1-x0)/(car  align))
    rows = (round (y1-y0)/(cadr align))

    ; add fill cells (TODO: refactor this code with skill/layout/route/overlay.il perhaps)
    power2 = (dbCreateRect CV (list Metal[8] "block") (list x0:y0 x1:y1))
    shapes = (leLayerAndNot CV (list Metal[8] "block") (list "prBoundary" "boundary") (list Metal[2] "block"))
    (dbLayerTile CV (list Metal[2] "block") shapes)
    (dbDeleteObject prb2)
    (dbDeleteObject power2)
    (foreach obj shapes (dbDeleteObject obj))
    hookCV = (dbOpenCellViewByType "globals" "globals.POWER_HOOKUP.0" "layout")
    count = 0
    (foreach shape (setof s CV->shapes s->lpp==(list Metal[2] "block"))
             count = (FillBox shape->bBox ?CV CV ?n count ?doHalo doHalo)
             (when (isStandardCell CV) || (dbGetPropByName CV "LaygenVersion")
                   fx0 = (car (car shape->bBox))
                   fy0 = (cadr (car shape->bBox))
                   fx1 = (car (cadr shape->bBox))
                   fy1 = (cadr (cadr shape->bBox))
                   frows = (round (fy1-fy0)/(cadr gridAlignment[2]))
                   (dbCreateSimpleMosaic CV hookCV
                                         (sprintf nil "fill%d" count++) fx0:fy0 "R0"
                                         frows 1 (cadr align) 0)
                   (dbCreateSimpleMosaic CV hookCV
                                         (sprintf nil "fill%d" count++) fx1:fy0 "R0"
                                         frows 1 (cadr align) 0)
                   )
             (dbDeleteObject shape)
             )

    ; label GND substrate and Vdd nwell (TODO: using pdkinfo.il)
    (unless doHalo
      (for r 0 rows
           y = y0+(cadr align)*r
           (MakeLayoutTagLabel CV (list "pwellSubIso" "id") x0+(car align)/2:y GNDNetName ?addrect nil)
           )
      (for r 0 rows-1
           y = y0+(cadr align)*(r+0.5)
           (MakeLayoutTagLabel CV (list "nwell" "drawing") x0+(car align)/2:y VddNetName)
           )
      )
    )
  CV
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

; draw filler cells in a bBox
(defun FillBox (bbox @key (CV (geGetEditCellView)) (n 0) (doHalo nil))
  (let (fill x0 y0 x1 y1 xa ya xl w y s orient wdiv wmod hlh tap)
    fill = (makeTable "fill" nil)
    fill[1] = (dbOpenCellViewByType "vendor.intel.d04" "vendor.intel.d04.spc00nn.z01" "layout")
    fill[2] = (dbOpenCellViewByType "vendor.intel.d04" "vendor.intel.d04.spc00nn.z02" "layout")
    fill[3] = (dbOpenCellViewByType "vendor.intel.d04" "vendor.intel.d04.spc00nn.z03" "layout")
    tap = (dbOpenCellViewByType "vendor.intel.d04" "vendor.intel.d04.tap02nd.z05" "layout")
    hlv = (dbOpenCellViewByType "vendor.intel.d04" "vendor.intel.d04.hlv00nd.z00" "layout")
    x0 = (car  (car  bbox))
    y0 = (cadr (car  bbox))
    x1 = (car  (cadr bbox))
    y1 = (cadr (cadr bbox))
    xa = (car gridAlignment[2])
    ya = (cadr gridAlignment[2])/2
    x0 = (round x0/xa)
    y0 = (round y0/ya)
    x1 = (round x1/xa)
    y1 = (round y1/ya)
    w = x1-x0
    (for row y0 y1-1
         y = (if (mod row 2)==0 row*ya (row+1)*ya)
         s = w
         xl = x0
         (when doHalo && s>=8
               (when (mod row 2)==0
                     (while (dbFindAnyInstByName CV (sprintf nil "fill%d" n)) n++)
                     (dbCreateInst CV hlv (sprintf nil "fill%d" n++) x0*xa:y "R0")
                     (while (dbFindAnyInstByName CV (sprintf nil "fill%d" n)) n++)
                     (dbCreateInst CV hlv (sprintf nil "fill%d" n++) x1*xa:y "MY")
                     )
               s=s-8
               xl=xl+4
               )
         (when s>=5
           (when (mod row 2)==0
                 (while (dbFindAnyInstByName CV (sprintf nil "fill%d" n)) n++)
                 (dbCreateInst CV tap (sprintf nil "fill%d" n++) xl*xa:y "R0")
                 )
           s=s-5
           xl=xl+5
           )
         orient = (if (mod row 2)==0 "R0" "MX")
         wdiv = s/3
         wmod = (mod s 3)
         (when wdiv>0
           (while (dbFindAnyInstByName CV (sprintf nil "fill%d" n)) n++)
           (dbCreateSimpleMosaic CV fill[3] (sprintf nil "fill%d" n++)
                                 xl*xa:y orient 1 wdiv GridPitch 3*GridPolyPitch)
           )
         (when wmod>0
           (while (dbFindAnyInstByName CV (sprintf nil "fill%d" n)) n++)
           (dbCreateInst CV fill[wmod] (sprintf nil "fill%d" n++) (xl+3*wdiv)*xa:y orient)
           )
         )
    )
  n
  )
