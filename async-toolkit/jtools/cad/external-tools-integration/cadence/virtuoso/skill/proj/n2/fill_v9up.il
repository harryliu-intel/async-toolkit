; add v9+ fill mosaic, avoiding existing TM1
(defun FillV9UP (@key (CV (geGetEditCellView)) (y0 35*1.596))
  (let (fillCV x y dy n inst tm1 todo)
    n=0
    y=y0
    (for row 0 8
         (cond (row==0 || row==8
                       fillCV = (ReadCV "globals" "globals.FILL_V9UP.wide"   "layout")
                       dy = 41.496
                       )
               (t
                fillCV = (ReadCV "globals" "globals.FILL_V9UP.narrow" "layout")
                dy = 15.96
                )
             )
         tm1 = (car (setof x fillCV->shapes x->lpp==(list "tm1" "drawing")))->bBox
         tm1 = (BBoxExpandHorizontal tm1 5.5)
         tm1 = (BBoxExpandVertical tm1 5.5)
         (for col -584 583
              x = 6.72 * col
              bbox = (dbTransformBBox tm1 (list x:y "R0"))
              (unless (dbGetOverlaps CV bbox (list "tm1" "drawing") 32)
                todo = (cons (list fillCV x:y) todo)
                )
              )
         y = y+dy
         )
    (foreach s todo
             inst = (dbCreateInst CV (car s) (sprintf nil "fill_tm1_%d" n) (cadr s) "R0")
             (dbReplaceProp inst "autogen" "boolean" t)
             n=n+1
             )
    n
    )
  )
