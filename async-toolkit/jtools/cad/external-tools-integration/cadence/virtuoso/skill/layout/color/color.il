; always color metals based on track position since this is invariable in a fixed track pattern
; via colors are assigned by ICC during routing, so don't want to change those unless they are uncolored

(defun Colorize (@key (CV (geGetEditCellView)) (doVias t) (override nil))
  (let (color shapes track oldcolor track1 track2 color1 color2 colorc layernum cutbbox)
    (foreach metal ColorableMetals
       shapes = (setof s CV->shapes (car s->lpp)==metal)
       (foreach shape shapes
          (when shape->objType=="rect"
	     track = (GetTrack (MetalNumber ?layer metal) shape->bBox)
	     )
	  (when shape->objType=="path"
             track = (GetTrack (MetalNumber ?layer metal) shape->bBox)
	     )
	  (unless shape->objType=="label" || (cadr shape->lpp)=="AABBregion"
	     (if !track then
	        (printf "ERROR: No color track. Probably a shape off-grid on %s at %f:%f\n"
			(car shape->lpp) (caar shape->bBox) (cadar shape->bBox))
   	     else
                color = (GetTrackColor (MetalNumber ?lpp shape->lpp) track)
	        (ColorizeShape shape color)
	        )
             )
	  )
       )
    (when doVias
       (foreach via ColorableVias
          shapes = (setof s CV->shapes (car s->lpp)==via)
	  (foreach shape shapes
             (when shape->objType=="rect"
	        track = (GetTrack (ViaMetalNumber ?layer via) shape->bBox)
	        color = (GetViaTrackColor (ViaMetalNumber ?layer via) track)
		oldcolor = (dbGetShapeColor shape)
		(when override || oldcolor=="grayColor"
                      (ColorizeShape shape color)
                      )
	        )
	     )
	  )
       (foreach via CV->vias
          ; some hard coded hacks in here right now
	  ; the way this is structured, it will not edit the metal colors of an already colored via cut
	  track1 = nil
	  track2 = nil
	  color1 = nil
	  color2 = nil
	  colorc = nil
          oldcolor = (dbGetViaCutLayerControl via)
	  (when override || oldcolor==vcolor_control[0]
             (when (rexMatchp "via1" via->viaHeader->master->cellName) ||
	     	   (rexMatchp "via2" via->viaHeader->master->cellName) ||
		   (rexMatchp "via3" via->viaHeader->master->cellName)
		layernum = (ViaMetalNumber ?layer via_master_table[via->viaHeader->master->cellName])
	        cutbbox = (GetCutBBox via)
	        track1 = (GetTrack layernum-1 cutbbox)
	        track2 = (GetTrack layernum cutbbox)
	        (when track1 color1 = (GetTrackColorNum layernum-1 track1))
	        (when track2 color2 = (GetTrackColorNum layernum track2))
	        (when track1 && track2 colorc = (GetViaTrackControl layernum track2))
		(if colorc then
		  (ColorizeVia via colorc color1 color2)
		else
		  (printf "ERROR: Can't color off-track %s at %f:%f\n" via->viaHeader->master->cellName (car via->origin) (cadr via->origin))
		  )
	        )
              ; special case for via0/4/5 because the cut is uncolored but the metals are colored
              (when (rexMatchp "via0" via->viaHeader->master->cellName) ||
	       	    (rexMatchp "via4" via->viaHeader->master->cellName) ||
		    (rexMatchp "via5" via->viaHeader->master->cellName)
	        layernum = (ViaMetalNumber ?layer via_master_table[via->viaHeader->master->cellName])
		cutbbox = (GetCutBBox via)
	        track1 = (GetTrack layernum-1 cutbbox)
	        track2 = (GetTrack layernum cutbbox)
	        (when track1 color1 = (GetTrackColorNum layernum-1 track1))
	        (when track2 color2 = (GetTrackColorNum layernum track2))
	        (when track1 && track2 colorc = (GetViaTrackControl layernum track2))
		(if track1 && track2 then
		  (ColorizeVia via nil color1 color2)
		else
		  (printf "ERROR: Can't color off-track %s at %f:%f\n" via->viaHeader->master->cellName (car via->origin) (cadr via->origin))
		  )
	        )
		

	     )
          )
       )
    (dbSave CV)
    t
    )
  )

(defun GetCutBBox (via)
  (let (params cutw cuth bbox)
    bbox = nil
    cutw = 0
    cuth = 0
    cutw = (nth 1 via->viaHeader->viaDef->params)
    cuth = (nth 2 via->viaHeader->viaDef->params)
    params = via->viaHeader->overrideParams
    (foreach p params
      (when (car p)=="cutWidth"  cutw = (cadr p))
      (when (car p)=="cutHeight" cuth = (cadr p))
      )
    (when cutw && cuth    
      bbox = (list (list (car via->origin)-cutw/2 (cadr via->origin)-cuth/2)
      	     	   (list (car via->origin)+cutw/2 (cadr via->origin)+cuth/2))
      ;this wouldn't be necessary except that some vias are indented
      indent = via_indent_table[via->viaHeader->master->cellName]
      bbox = (list (list (caar bbox)-(car indent) (cadar bbox)-(cadr indent))
      	     	   (list (caadr bbox)+(car indent) (cadadr bbox)+(cadr indent)))
      )
    bbox
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
    (when mptcolor
      (dbSetViaCutLayerControl via mptcolor)
      )
    (when mptcolor_b
      (dbSetViaLayer1Control via vcolor_control[mptcolor_b])
      )
    (when mptcolor_t
      (dbSetViaLayer2Control via vcolor_control[mptcolor_t])
      )
    )
  )
