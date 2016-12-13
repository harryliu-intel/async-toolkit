; utility derived from PowerV8 to add pins for m9 power
(defun LabelM9 (@key (CV (geGetEditCellView))
                     (xp 1.68)  ; horizontal pitch for potential v8
                     (xo 0.0)   ; horizontal offset (TODO: should be xp/2)
                     (yp 0.797)
                     (yo 0.014) 
                     (supplies (list "GND" "Vdd" "GND" "VDDM")) ; repeats endlessly at constant wire pitch
                     )
  (let (bbox o x0 x1 y0 y1 s sx sy name net)
    bbox = CV->prBoundary->bBox
    x0 = (car  (car  bbox))
    x1 = (car  (cadr bbox))
    y0 = (cadr (car  bbox))
    y1 = (cadr (cadr bbox))
    x0 = (round x0/xp)
    x1 = (round x1/xp)
    y0 = (round y0/yp)
    y1 = (round y1/yp)
    lpp = (list "m9" "pin")
    o = 0.42
    (for x x0 x1-1
         s    = (mod x-x0 (length supplies))
         sx   = x*xp+xo
         sy   = y0*yp+o
         name = (nth s supplies)
         (printf "labeling %s at %g:%g\n" name sx sy)
         net  = (dbMakeNet CV name)
         rect = (dbCreateRect CV lpp (list sx-o:sy-o sx+o:sy+o))
         pin  = (dbCreatePin net rect)
         (LabelPin CV rect)
         (dbReplaceProp rect "autogen" "boolean" t)
         )
    )
  t
  )
