; Script to create globals.POWER_GRID.m*_pg cells
;
; Gets all dimensions from PDK businfo.il, but the choice of stripes
; and staples is hard-coded for 1273.

; create a complete set of POWER_GRID cells
(defun CreatePowerGridCells (@key (botMetal 2) (topMetal 8))
  (let (CV cellName)
    (for bot botMetal topMetal
         (for top bot topMetal
              cellName = "globals.POWER_GRID.m"
              (for m bot top cellName = (strcat cellName (sprintf nil "%d" m)))
              cellName = (strcat cellName "_pg")
              (printf "%s\n" cellName)
              CV = (dbOpenCellViewByType "globals" cellName "layout" "maskLayout" "w")
              (DrawPowerGrid bot top ?CV CV)
              (dbSave CV)
              (cdsp4add ?CV CV)
              )
         )
    )
  t
  )

; draw power stripes and vias in a POWER_GRID cell
(defun DrawPowerGrid (botMetal topMetal @key (CV (geGetEditCellView)))
  (let (xalign yalign rows cols h y)
    xalign = (car  gridAlignment[topMetal])
    yalign = (cadr gridAlignment[topMetal])

    ; metal2
    (when botMetal<=2 && topMetal>=2
          rows = (round yalign/(cadr gridAlignment[2]))
          (DrawChannelArray nil power_m2 "" 0:rows-1 pitch_m2 (list 0:0 xalign:0) ?CV CV)
          )
    
    ; metal 3
    (when botMetal<=3 && topMetal >=3
          h = power_overhang_m3
          rows = (round yalign/(cadr gridAlignment[3]))
          cols = (round xalign/(car  gridAlignment[3]))
          (for row 0 rows-1
               y=pitch_m2*row
               (DrawChannelArray nil power_m3g "" 0:cols-1 pitch_m3 (list 0:y        0:y+h)      ?CV CV)
               (DrawChannelArray nil power_m3v "" 0:cols-1 pitch_m3 (list 0:y+BP/2-h 0:y+BP/2+h) ?CV CV)
               (DrawChannelArray nil power_m3g "" 0:cols-1 pitch_m3 (list 0:y+BP-h   0:y+BP)     ?CV CV)
               )
          )

    ; metal4
    (when botMetal<=4 && topMetal>=4
          rows = (round yalign/(cadr gridAlignment[4]))
          (DrawChannelArray nil power_m4 "" 0:rows-1 pitch_m4 (list 0:0 xalign:0) ?CV CV)
          )
    
    ; metal 5
    (when botMetal<=5 && topMetal >=5
          h = power_overhang_m5
          rows = (round yalign/(cadr gridAlignment[5]))
          cols = (round xalign/(car  gridAlignment[5]))
          (for row 0 rows-1
               y=pitch_m4*row
               (DrawChannelArray nil power_m5g "" 0:cols-1 pitch_m5 (list 0:y-h      0:y+h)      ?CV CV)
               (DrawChannelArray nil power_m5v "" 0:cols-1 pitch_m5 (list 0:y+BP/2-h 0:y+BP/2+h) ?CV CV)
               (DrawChannelArray nil power_m5g "" 0:cols-1 pitch_m5 (list 0:y+BP-h   0:y+BP+h)   ?CV CV)
               )
          )

    ; metal6
    (when botMetal<=6 && topMetal>=6
          rows = (round yalign/(cadr gridAlignment[6]))
          (DrawChannelArray nil power_m6 "" 0:rows-1 pitch_m6 (list 0:0 xalign:0) ?CV CV)
          )

    ; metal7
    (when botMetal<=7 && topMetal>=7
          h = power_overhang_m7
          cols = (round xalign/(car gridAlignment[7]))
          (DrawChannelArray nil power_m7 "" 0:cols-1 pitch_m7 (list 0:-h 0:yalign+h) ?CV CV)
          )

    ; metal8
    (when botMetal<=8 && topMetal>=8
          rows = (round yalign/(cadr gridAlignment[8]))
          (DrawChannelArray nil power_m8 "" 0:rows-1 pitch_m8 (list 0:0 xalign:0) ?CV CV)
          )

    ; merge duplicate shapes
    (leMergeShapes CV->shapes)

    ; vias
    (for m botMetal topMetal-1
         (DrawPowerGridVias "gnd" Metal[m] Metal[m+1] Via[m] ViaSizeGND[m] ?CV CV)
         (DrawPowerGridVias "vdd" Metal[m] Metal[m+1] Via[m] ViaSizeVdd[m] ?CV CV)
         )
    )
  CV
  )

; draw vias where power stripes cross
(defun DrawPowerGridVias (purpose layer1 layer2 via_layer via_size @key (CV (geGetEditCellView)))
  (let (lpp1 lpp2 x y w h)
    lpp1 = (list layer1 purpose)
    lpp2 = (list layer2 purpose)
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
                      (dbCreateRect CV (list via_layer purpose) (list x-w:y-h x+w:y+h))
                      )
             )
    )
  t
  )
