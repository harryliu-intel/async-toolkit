; always color metals based on track position since this is invariable in a fixed track pattern
; via colors are assigned by ICC during routing, so don't want to change those unless they are uncolored
; v1 and v2 are colored based on m2 track, v3 and v4 are assumed to be colored by Qfill

(defun Colorize (@key (CV (geGetEditCellView)) (doVias t))
  (let (color shapes track)
    (foreach metal ColorableMetals
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
    (when doVias
       (foreach via ColorableVias
          shapes = (setof s CV->shapes (car s->lpp)==via)
	  (foreach shape shapes
             (when shape->objType=="rect"
	        track = (GetTrack (ViaMetalNumber ?layer via) (car shape->bBox))
	        color = (GetViaTrackColor (ViaMetalNumber ?layer via) track)
		oldcolor = (dbGetShapeColor shape)
	        (when oldcolor=="grayColor"
		   (ColorizeShape shape color)
		   )
	        )
	     )
	  )
       )
    (dbSave CV)
    t
    )
  )

(defun ColorizeShape (shape mptcolor)
  (let ()
    (printf "Creating MPT Color for %s %s %f %s\n" shape->objType (car shape->lpp) (cadar shape->bBox) mptcolor)
    (dbSetShapeColor shape mptcolor)
    )
  )

(defun ColorizeVia (shape mptcolor mptcolor_b mptcolor_t)
  (let ()
    (printf "Creating MPT Color for %s %s %f %s\n" shape->objType (car shape->lpp) (cadar shape->bBox) mptcolor)
    (dbSetShapeColor shape mptcolor)
    )
  )
