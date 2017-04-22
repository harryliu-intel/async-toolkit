(defun DrawCE1 (@key (CV (geGetEditCellView)))
  (leLayerSize CV (list "ce2" "drawing") -0.168 (list "ce1" "drawing"))
  t
  )

(defun AlignShapes (@key (CV (geGetEditCellView)) (align 0.84:0.798))
  (let (points)
    (foreach shape CV->shapes
             points = nil
             (cond (shape->objType=="polygon" || shape->objType=="path" || shape->objType=="pathSeg"
                                    (foreach pt shape->points
                                             points = (cons (AlignPoint pt align) points)
                                             )
                                    shape->points=points
                                    )
                   (shape->objType=="rect"
                                    (foreach pt shape->bBox
                                             points = (cons (AlignPoint pt align) points)
                                             )
                                    shape->bBox = points
                                    )
                   (shape->objType=="label"
                                    shape->xy = (AlignPoint shape->xy align)
                                    )
                   )
             )
    )
  t
  )

(defun AlignPoint (xy align)
  (round (car xy)/(car align))*(car align):(round (cadr xy)/(cadr align))*(cadr align)
  )
