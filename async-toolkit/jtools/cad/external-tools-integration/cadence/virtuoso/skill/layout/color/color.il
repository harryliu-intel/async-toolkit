
(defun ColorizeCell (@key (CV (geGetEditCellView)))
  (let (color shapes track)
    (foreach metal ColorableLayers
       shapes = (setof s CV->shapes (car s->lpp)==metal)
       (foreach shape shapes
          (when shape->objType=="rect"
	     track = (GetTrack (MetalNumber ?layer metal) (car shape->bBox))
	     )
	  (when shape->objType=="path"
             track = (GetTrack (MetalNumber ?layer metal) (car shape->points))
	     )
	  (unless shape->objType=="label"
             color = (GetTrackColor (MetalNumber ?lpp shape->lpp) track)
	     (ColorizeShape shape color)
	     )
          )
       )
    (dbSave CV)
    t
    )
  )

(defun ColorizeShape (shape mptcolor)
  (let ()
    ;(printf "Creating MPT Color for %s %s %f %s\n" shape->objType (car shape->lpp) (cadar shape->bBox) mptcolor)
    (dbSetShapeColor shape mptcolor)
    )
  )
