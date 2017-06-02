; add v9+ fill mosaic, avoiding existing TM1
(defun FillV9UP (@key (CV (geGetEditCellView)) (y0 35*1.596) (col0 -584) (col1 584))
  (let (fillCV x y dy n inst tm1 old_type type todo)
    n=0
    y=y0
    (for row 0 8
         (cond (row==0 || row==8
                width = "wide"
                dy = 41.496
                )
               (t
                width = "narrow"
                dy = 15.96
                )
               )
         fillCV = (ReadCV "globals" (sprintf nil "globals.FILL_V9UP.%s_gnd_x1" width) "layout")
         tm1 = (car (setof x fillCV->shapes x->lpp==(list "tm1" "drawing")))->bBox
         tm1 = (BBoxExpandHorizontal tm1 5.5)
         tm1 = (BBoxExpandVertical tm1 5.5)
         fillCV = (ReadCV "globals" (sprintf nil "globals.FILL_V9UP.%s_sig_x1" width) "layout")
         tm1sig = (car (setof x fillCV->shapes x->lpp==(list "tm1" "drawing")))->bBox
         old_type = ""
         old_x = col0*6.672
         (for col col0 col1
              x = 6.72 * col
              bbox = (dbTransformBBox tm1 (list x:y "R0"))
              bboxsig = (dbTransformBBox tm1sig (list x:y "R0"))
              (cond (col==col1 type="")
                    ((dbGetOverlaps CV bbox (list "tm1" "drawing") 32)==nil
                     type="gnd"
                     )
                    ((dbGetOverlaps CV bboxsig (list "m9" "drawing") 32)==nil &&
                     (dbGetOverlaps CV bboxsig (list "v9" "drawing") 32)==nil &&
                     (dbGetOverlaps CV (list (car (car bboxsig)):(cadr (car  bboxsig))
                                             (car (car bboxsig)):(cadr (cadr bboxsig)))
                                    (list "tm1" "drawing") 32)!=nil &&
                     (dbGetOverlaps CV (list (car (cadr bboxsig)):(cadr (car  bboxsig))
                                             (car (cadr bboxsig)):(cadr (cadr bboxsig)))
                                    (list "tm1" "drawing") 32)!=nil
                     type="sig"
                     )
                    (t
                     type=""
                     )
                    )
              (when old_type!=type
                (when old_type!="" todo = (cons (list old_x x y width old_type) todo))
                old_x = x
                )
              old_type = type
              )
         y = y+dy
         )
    (foreach s todo
             n = (FillV9UP_Stripe n (car s) (cadr s) (caddr s) (nth 3 s) (nth 4 s) ?CV CV)
             )
    n
    )
  )

; Different defaults for SOUTH
(defun FillV9UP_SOUTH (@key (CV (geGetEditCellView)))
  (FillV9UP ?CV CV ?y0 -403.788 ?col0 -578 ?col1 577)
  )

(defun FillV9UP_Stripe (n x0 x1 y width type @key (CV (geGetEditCellView)) (xp 6.72))
  (let (cols cols3 cols2 cols1 x CV1 CV2 CV3)
    CV1 = (ReadCV "globals" (sprintf nil "globals.FILL_V9UP.%s_%s_x1" width type) "layout")
    CV2 = (ReadCV "globals" (sprintf nil "globals.FILL_V9UP.%s_%s_x2" width type) "layout")
    CV3 = (ReadCV "globals" (sprintf nil "globals.FILL_V9UP.%s_%s_x3" width type) "layout")
    cols = (round (x1-x0)/xp)
    cols3 = cols/3
    cols2 = 0
    cols1 = (mod cols 3)
    (when cols1==2 cols1=0 cols2=1)
    (when cols1==1 && cols3>0 cols3-- cols1=0 cols2=2) ; avoid cols1
    ; (printf "%g %g %g %s %s cols3=%d cols2=%d cols1=%d\n" x0 x1 y width type cols3 cols2 cols1)
    x = x0
    (when cols3>0
      inst = (dbCreateSimpleMosaic CV CV3 (sprintf nil "fill_tm1_%d" n) x:y "R0" 1 cols3 1.596 3*6.72)
      (dbReplaceProp inst "autogen" "boolean" t)
      x=x+3*cols3*xp
      n++
      )
    (when cols2>0
      inst = (dbCreateSimpleMosaic CV CV2 (sprintf nil "fill_tm1_%d" n) x:y "R0" 1 cols2 1.596 2*xp)
      (dbReplaceProp inst "autogen" "boolean" t)
      n++
      x=x+2*cols2*xp
      )
    (when cols1>0
      inst = (dbCreateSimpleMosaic CV CV1 (sprintf nil "fill_tm1_%d" n) x:y "R0" 1 cols1 1.596 xp)
      (dbReplaceProp inst "autogen" "boolean" t)
      n++
      x=x+cols1*xp
      )
    )
  n
  )
