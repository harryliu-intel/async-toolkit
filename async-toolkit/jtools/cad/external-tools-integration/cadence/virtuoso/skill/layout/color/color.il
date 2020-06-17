; always color metals based on track position since this is invariable
; in a fixed track pattern via colors are assigned by ICC during
; routing, so don't want to change those unless they are uncolored
(defun Colorize (@key (CV (geGetEditCellView)) (doVias t) (override nil))
  (let (offset bbox color shapes track oldcolor track1 track2 color1 color2 colorc layernum cutbbox)
    offset=(cadr (GetCellAlignment ?CV CV))
    (foreach metal ColorableMetals
       shapes = (setof s CV->shapes (car s->lpp)==metal)
       (foreach shape shapes
          bbox=(dbTransformBBox shape->bBox (list -(car offset):-(cadr offset) "R0"))      
          (when shape->objType=="rect"
	     track = (GetTrack (MetalNumber ?layer metal) bbox)
	     )
	  (when shape->objType=="path"
             track = (GetTrack (MetalNumber ?layer metal) bbox)
	     )
	  (unless shape->objType=="label" || (cadr shape->lpp)=="AABBregion"
	     (if !track then
	        (printf "ERROR: No color track. Probably a shape off-grid on %s at %f:%f\n"
			(car shape->lpp) (caar bbox) (cadar bbox))
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
             bbox=(dbTransformBBox shape->bBox (list -(car offset):-(cadr offset) "R0"))      
             (when shape->objType=="rect"
	        track = (GetTrack (ViaMetalNumber ?layer via) bbox)
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
  (dbSetShapeColor shape mptcolor)
  )

(defun ColorizeVia (via mptcolor mptcolor_b mptcolor_t)
  (when mptcolor   (dbSetViaCutLayerControl via mptcolor))
  (when mptcolor_b (dbSetViaLayer1Control via vcolor_control[mptcolor_b]))
  (when mptcolor_t (dbSetViaLayer2Control via vcolor_control[mptcolor_t]))
  )

; flip color of specified metal layer number
(defun FlipColor (m @key (CV (geGetEditCellView)))
  (let (color)
    (foreach shape CV->shapes
             (when shape->objType!="label" && shape->layerName==Metal[m]
                   color = (dbGetShapeColor shape)
                   (cond (color=="mask1Color" color="mask2Color")
                         (color=="mask2Color" color="mask1Color")
                         )
                   (ColorizeShape shape color)
                   )
             )
    )
  t
  )

; Assign maskColor using overlay layers, then optionally delete overlay layers
(defun ColorizeByOverlay (@key (CV (geGetEditCellView)) (delete nil))
  (let (layer color overlaps lpp via colorName)
    (foreach s CV->shapes
             (cond (s->lpp==(list "m0"  "backBone")   layer="m0" color=1)
                   (s->lpp==(list "m0c" "complement") layer="m0" color=2)
                   (s->lpp==(list "m0s" "spacer")     layer="m0" color=3)
                   (s->lpp==(list "m2"  "backBone")   layer="m2" color=1)
                   (s->lpp==(list "m2c" "complement") layer="m2" color=2)
                   (s->lpp==(list "m3"  "backBone")   layer="m3" color=1)
                   (s->lpp==(list "m3c" "complement") layer="m3" color=2)
                   (s->lpp==(list "m4"  "backBone")   layer="m4" color=1)
                   (s->lpp==(list "m4c" "complement") layer="m4" color=2)
                   (s->lpp==(list "m5"  "backBone")   layer="m5" color=1)
                   (s->lpp==(list "m5c" "complement") layer="m5" color=2)
                   (s->lpp==(list "m6"  "backBone")   layer="m6" color=1)
                   (s->lpp==(list "m6c" "complement") layer="m6" color=2)
                   (s->lpp==(list "v1a" "colorA")     layer="v1" color=1)
                   (s->lpp==(list "v1b" "colorB")     layer="v1" color=2)
                   (s->lpp==(list "v1c" "colorC")     layer="v1" color=3)
                   (s->lpp==(list "v2a" "colorA")     layer="v2" color=1)
                   (s->lpp==(list "v2b" "colorB")     layer="v2" color=2)
                   (s->lpp==(list "v2c" "colorC")     layer="v2" color=3)
                   (s->lpp==(list "v3a" "colorA")     layer="v3" color=1)
                   (s->lpp==(list "v3b" "colorB")     layer="v3" color=2)
                   (t                                 layer=nil  color=nil)
                   )
             (when layer
               colorName = (sprintf nil "mask%dColor" color)
               overlaps=nil
               (foreach purpose (list "drawing" "pin" "fill" "actFill" "vdd" "gnd")
                        lpp = (list layer purpose)
                        overlaps=(append overlaps (dbGetOverlaps CV s->bBox lpp 0:1))
                        )
               (foreach o overlaps
                        (cond (!(atom o) && (car o)->objType=="stdVia"
                                o=(car o)
                                via=o->viaHeader->viaDef
                                (cond (via->layer1->name==layer (ColorizeVia o nil color nil))
                                      (via->layer2->name==layer (ColorizeVia o nil nil color))
                                      (t (ColorizeVia o vcolor_control[color] nil nil))
                                      )
                                )
                              ((atom o) && o->objType!="label"
                                   (ColorizeShape o colorName)
                                   )
                              )
                        )
               (when delete (dbDeleteObject s))
               )
             )
    )
  t
  )
