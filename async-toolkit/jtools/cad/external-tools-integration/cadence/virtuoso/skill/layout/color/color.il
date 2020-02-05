; always color metals based on track position since this is invariable in a fixed track pattern
; via colors are assigned by ICC during routing, so don't want to change those unless they are uncolored
; v1 and v2 are colored based on m2 track, v3 and v4 are assumed to be colored by Qfill

(defun Colorize (@key (CV (geGetEditCellView)) (doVias t))
  (let (color shapes track oldcolor shiftorg track1 track2 color1 color2 colorc)
    (foreach metal ColorableMetals
       shapes = (setof s CV->shapes (car s->lpp)==metal)
       (foreach shape shapes
          (when shape->objType=="rect"
	     track = (GetTrack (MetalNumber ?layer metal) (car shape->bBox))
	     )
	  (when shape->objType=="path"
             track = (GetTrack (MetalNumber ?layer metal) (car shape->bBox))
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
       (foreach via CV->vias
          ; some hard coded hacks in here right now
	  ; the way this is structured, it will not edit the metal colors of an already colored via cut
          oldcolor = (dbGetViaCutLayerControl via)
	  (if oldcolor!=vcolor_control[0] then
	     (printf "Not changing via that is already colored %s %s.\n" via->viaHeader->master->cellName oldcolor)
          else
	     (when via->viaHeader->master->cellName=="via1"
	        (unless (GetTrackWidth 2 via->origin)
		   (error "Can't color with m2 off-track via %f:%f\n" (car via->origin) (cadr via->origin))
		   )
	        shiftorg = (list (car via->origin) (cadr via->origin)-(GetTrackWidth 2 via->origin)/2)
	        track = (GetTrack 2 shiftorg)
	        color1 = nil
	        color2 = (GetTrackColorNum 2 track)
	        colorc = (GetViaTrackControl 2 track)
	        (ColorizeVia via colorc color1 color2)
	        )
             (when via->viaHeader->master->cellName=="via2"
	        (unless (GetTrackWidth 2 via->origin)
		   (error "Can't color with m2 off-track %s %f:%f\n" via->viaHeader->master->cellName (car via->origin) (cadr via->origin))
		   )
	        (unless (GetTrackWidth 3 via->origin)
		   (error "Can't color with m3 off-track %s %f:%f\n" via->viaHeader->master->cellName (car via->origin) (cadr via->origin))
		   )
	        shiftorg = (list (car via->origin)-(GetTrackWidth 3 via->origin)/2 (cadr via->origin)-(GetTrackWidth 2 via->origin)/2)
	        track1 = (GetTrack 2 shiftorg)
	        track2 = (GetTrack 3 shiftorg)
	        color1 = (GetTrackColorNum 2 track1)
	        color2 = (GetTrackColorNum 3 track2)
	        colorc = (GetViaTrackControl 2 track1)
	        (ColorizeVia via colorc color1 color2)
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
    ;(printf "Creating MPT Color for %s %s %f %s\n" shape->objType (car shape->lpp) (cadar shape->bBox) mptcolor)
    (dbSetShapeColor shape mptcolor)
    )
  )

(defun ColorizeVia (via mptcolor mptcolor_b mptcolor_t)
  (let ()
    ;(printf "Creating MPT Color for %s %s %f %s\n" shape->objType (car shape->lpp) (cadar shape->bBox) mptcolor)
    (dbSetViaCutLayerControl via mptcolor)
    (when mptcolor_b
       (dbSetViaLayer1Control via vcolor_control[mptcolor_b])
       )
    (when mptcolor_t
       (dbSetViaLayer2Control via vcolor_control[mptcolor_t])
       )
    )
  )
