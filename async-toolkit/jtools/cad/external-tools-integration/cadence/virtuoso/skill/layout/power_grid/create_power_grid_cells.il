; Script to create globals.POWER_GRID.m*_pg cells
;
; Gets all dimensions from PDK businfo.il, but the choice of stripes
; and staples is hard-coded for 1273.

; create a complete set of POWER_GRID cells
(defun CreatePowerGridCells (@key (botMetal 2) (topMetal 8) (noFill nil))
  (let (CV cellName)
    (for bot botMetal topMetal
         (for top bot topMetal
              cellName = (if noFill "globals.POWER_GRID_NO_FILL.m" "globals.POWER_GRID.m")
              (for m bot top cellName = (strcat cellName (sprintf nil "%d" m)))
              cellName = (strcat cellName "_pg")
              (printf "%s\n" cellName)
              CV = (dbOpenCellViewByType "globals" cellName "layout")
              (when CV (cdsp4edit ?CV CV))
              CV = (dbOpenCellViewByType "globals" cellName "layout" "maskLayout" "w")
              (DrawPowerGrid bot top ?CV CV ?noFill noFill)
              (cdsp4add ?CV CV)
              (when top==topMetal
                cellName = (strcat cellName "_half")
                (printf "%s\n" cellName)
                CV = (dbOpenCellViewByType "globals" cellName "layout")
                (when CV (cdsp4edit ?CV CV))
                CV = (dbOpenCellViewByType "globals" cellName "layout" "maskLayout" "w")
                (DrawPowerGrid bot top ?CV CV ?half t ?noFill noFill)
                (cdsp4add ?CV CV)
                )
              )
         )
    )
  t
  )

; draw power stripes and vias in a POWER_GRID cell
(defun DrawPowerGrid (botMetal topMetal @key (CV (geGetEditCellView)) (half nil) (noFill nil))
  (let (xalign yalign rows cols y s l h half_power_m8)
    (foreach x CV->shapes (dbDeleteObject x))
    xalign = (car  gridAlignment[topMetal])
    yalign = (cadr gridAlignment[topMetal])
    (when half yalign=yalign/2) ; for half-grids up to m8

    ; metal2
    (when botMetal<=2 && topMetal>=2
          h = powerOverhang[2]
          rows = (round yalign/(cadr gridAlignment[2]))
          (DrawChannelArray nil power_m2 "" 0:rows-1 pitch_m2 (list -h:0 xalign+h:0) ?CV CV)
          )
    
    ; metal 3
    (when botMetal<=3 && topMetal>=3
          h = powerOverhang[3]
          cols = (round xalign/(car gridAlignment[3]))
          (DrawChannelArray nil power_m3 "" 0:cols-1 pitch_m3 (list 0:-h 0:yalign+h) ?CV CV)
          )

    ; metal4
    (when botMetal<=4 && topMetal>=4
          h = powerOverhang[4]
          rows = (round yalign/(cadr gridAlignment[4]))
          (DrawChannelArray nil power_m4 "" 0:rows-1 pitch_m4 (list -h:0 xalign+h:0) ?CV CV)
          )
    
    ; metal 5
    (when botMetal<=5 && topMetal>=5
          h = powerOverhang[5]
          cols = (round xalign/(car gridAlignment[5]))
          (DrawChannelArray nil power_m5 "" 0:cols-1 pitch_m5 (list 0:-h 0:yalign+h) ?CV CV)
          )

    ; metal6
    (when botMetal<=6 && topMetal>=6
          h = powerOverhang[6]
          rows = (round yalign/(cadr gridAlignment[6]))
          (DrawChannelArray nil power_m6 "" 0:rows-1 pitch_m6 (list -h:0 xalign+h:0) ?CV CV)
          )

    ; metal7
    (when botMetal<=7 && topMetal>=7
          h = powerOverhang[7]
          cols = (round xalign/(car gridAlignment[7]))
          (DrawChannelArray nil power_m7 "" 0:cols-1 pitch_m7 (list 0:-h 0:yalign+h) ?CV CV)
          )

    ; metal8
    (when botMetal<=8 && topMetal>=8
          h = powerOverhang[8]
          (for i 0 ((length power_m8)/2)
               half_power_m8 = (append half_power_m8 (list (nth i power_m8)))
               )
          (DrawChannelArray nil (if half half_power_m8 power_m8) ""
                            0:0 pitch_m8 (list -h:0 xalign+h:0) ?CV CV)
          )

    ; merge duplicate shapes
    (leMergeShapes CV->shapes)

    ; vias
    (for m botMetal topMetal-1
         (DrawPowerGridVias "gnd" "gnd" Metal[m] Metal[m+1] Via[m] ViaSizeGND[m] ?CV CV)
         (DrawPowerGridVias "vdd" "vdd" Metal[m] Metal[m+1] Via[m] ViaSizeVdd[m] ?CV CV)
         (DrawPowerGridVias "vdd" "fill" Metal[m] Metal[m+1] Via[m] ViaSizeVddFill[m] ?CV CV)
         )

    ; convert to rectangles
    (foreach s CV->shapes
             (dbCreateRect CV s->lpp s->bBox)->net = s->net
             (dbDeleteObject s)
             )

    ; delete fill shapes
    (when noFill
      (foreach shape CV->shapes
               (when shape->purpose=="fill" (dbDeleteObject shape))
               )
      )

    ; save
    (dbSave CV)
    )
  CV
  )

; draw vias where power stripes cross
(defun DrawPowerGridVias (purpose1 purpose2 layer1 layer2 via_layer via_size @key (CV (geGetEditCellView)))
  (let (lpp1 lpp2 x y w h)
    lpp1 = (list layer1 purpose1)
    lpp2 = (list layer2 purpose2)
    (when via_size
      w = (car  via_size)/2
      h = (cadr via_size)/2
      (foreach shape1 (setof x CV->shapes x->lpp==lpp1)
               (foreach shape2 (dbGetOverlaps CV shape1->bBox lpp2 0)
                        x = nil
                        y = nil
                        (cond ((car (car shape1->points))==(car (cadr shape1->points))
                               x = (car (car shape1->points))
                               )
                              (t
                               y = (cadr (car shape1->points)))
                              )
                        (cond ((car (car shape2->points))==(car (cadr shape2->points))
                               x = (car (car shape2->points))
                               )
                              (t
                               y = (cadr (car shape2->points)))
                              )
                        (dbCreateRect CV (list via_layer purpose2) (list x-w:y-h x+w:y+h))
                        )
               )
      )
    )
  t
  )
