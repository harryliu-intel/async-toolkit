(defun StretchPoint (xy xoff)
  (let (x y n)
    x=(car xy)
    y=(cadr xy)
    n=(round x/0.054)
    x=x+n*0.006+xoff
    x:y
    )
  )

(defun StretchPolyPitch (@key (CV (geGetEditCellView)))
  (let (points shrink xoff)
    (foreach xy CV->prBoundary->points points=(append points (list (StretchPoint xy 0))))
    (dbDeleteObject CV->prBoundary)
    (dbCreatePRBoundary CV points)
    (foreach obj CV->shapes
             shrink = obj->layerName=="vt" || obj->layerName=="tcn"
             xoff = (if shrink 0.003 0)
             (cond (obj->objType=="rect"
                                  obj->bBox=(list (StretchPoint (car  obj->bBox)  xoff)
                                                  (StretchPoint (cadr obj->bBox) -xoff)))
                   (obj->objType=="label"  obj->xy=(StretchPoint obj->xy -xoff))
                   )
             )
    )
  t
  )
