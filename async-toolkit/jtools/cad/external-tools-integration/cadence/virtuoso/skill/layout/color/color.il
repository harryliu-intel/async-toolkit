; always color metals based on track position since this is invariable
; in a fixed track pattern via colors are assigned by ICC during
; routing, so don't want to change those unless they are uncolored
(defun Colorize (@key (CV (geGetEditCellView)) (doVias t) (override nil))
  (let (offset bbox color shapes track oldcolor track1 track2 color1 color2 colorc viaNum)
    offset=(cadr (GetCellAlignment ?CV CV))
    (foreach metal ColorableMetals
       shapes = (setof s CV->shapes (car s->lpp)==metal)
       (foreach shape shapes
          bbox=(dbTransformBBox shape->bBox (list -(car offset):-(cadr offset) "R0"))
          (when shape->objType=="rect"
	     track = (GetTrack (MetalNumber metal) bbox)
	     )
	  (when shape->objType=="path"
             track = (GetTrack (MetalNumber metal) bbox)
	     )
	  (unless shape->objType=="label" || (cadr shape->lpp)=="AABBregion"
	     (if !track then
	        (printf "ERROR: No color track. Probably a shape off-grid on %s at %f:%f\n"
			(car shape->lpp) (caar bbox) (cadar bbox))
   	     else
                color = (GetTrackColor (MetalNumber shape->layerName) track)
	        (ColorizeShape shape color)
	        )
             )
	  )
       )
    (when doVias
       (foreach via ColorableVias
                viaNum = (ViaNumber via)
                (unless viaNum (error))
                shapes = (setof s CV->shapes (car s->lpp)==via)
                (foreach shape shapes
                         bbox=(dbTransformBBox shape->bBox (list -(car offset):-(cadr offset) "R0"))
                         (when shape->objType=="rect"
                               track1 = (GetTrack viaNum   bbox)
                               track2 = (GetTrack viaNum+1 bbox)
                               track = (if (mod viaNum 2)==0 track1 track2)
                               ; (printf "viaNum=%d track=%d\n" viaNum track)
                               color = (GetViaTrackColor viaNum track)
                               oldcolor = (GetShapeColor shape)
                               (when override || oldcolor==0
                                     (ColorizeShape shape color)
                                     )
                               )
                         )
                )
       (foreach via CV->vias
          ; some hard coded hacks in here right now
	  track1 = nil
	  track2 = nil
	  color1 = nil
	  color2 = nil
	  colorc = nil
          oldcolor = (dbGetViaCutLayerControl via)
	  (when override || oldcolor==via_color_name[0]
                (cond ((strncmp via->viaHeader->master->cellName "via0" 4)==0 viaNum=0)
                      ((strncmp via->viaHeader->master->cellName "via1" 4)==0 viaNum=1)
                      ((strncmp via->viaHeader->master->cellName "via2" 4)==0 viaNum=2)
                      ((strncmp via->viaHeader->master->cellName "via3" 4)==0 viaNum=3)
                      ((strncmp via->viaHeader->master->cellName "via4" 4)==0 viaNum=4)
                      ((strncmp via->viaHeader->master->cellName "via5" 4)==0 viaNum=5)
                      (t viaNum=nil)
                      )
                (when viaNum==1 || viaNum==2 || viaNum==3
                      bbox = (GetCutBBox via)
                      bbox = (dbTransformBBox bbox (list -(car offset):-(cadr offset) "R0"))
                      track1 = (GetTrack viaNum   bbox)
                      track2 = (GetTrack viaNum+1 bbox)
                      (when track1 color1 = (GetTrackColor viaNum   track1))
                      (when track2 color2 = (GetTrackColor viaNum+1 track2))
                      track = (if (mod viaNum 2)==0 track1 track2)
                      ; (printf "viaNum=%d track=%d\n" viaNum track)
                      colorc = (GetViaTrackColor viaNum track)
                      (if colorc
                        (ColorizeVia via colorc color1 color2)
                        (printf "ERROR: Can't color off-track %s at %f:%f\n"
                                via->viaHeader->master->cellName
                                (car via->origin) (cadr via->origin))
                        )
                      )
                ; for via0/4/5 cut is uncolored but the metals are colored
                (when viaNum==0 || viaNum==4 || viaNum==5
                      bbox = (GetCutBBox via)
                      bbox = (dbTransformBBox bbox (list -(car offset):-(cadr offset) "R0"))
                      track1 = (GetTrack viaNum   bbox)
                      track2 = (GetTrack viaNum+1 bbox)
                      (when track1 color1 = (GetTrackColor viaNum   track1))
                      (when track2 color2 = (GetTrackColor viaNum+1 track2))
                      track = (if (mod viaNum 2)==0 track1 track2)
                      ; (printf "viaNum=%d track=%d\n" viaNum track)
                      colorc = (GetViaTrackColor viaNum track)
                      (if track1 && track2
                          (ColorizeVia via nil color1 color2)
                          (printf "ERROR: Can't color off-track %s at %f:%f\n"
                                  via->viaHeader->master->cellName
                                  (car via->origin) (cadr via->origin))
                          )
                      )
                )
          )
       )
    t
    )
  )

; Return the bbox of the via layer in a symbolic via
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
      )
    bbox
    )
  )

; Get the color number of a shape
(defun GetShapeColor (shape)
  (let (color)
    color=(dbGetShapeColor shape)
    (cond (color==metal_color_name[0] color=0)
          (color==metal_color_name[1] color=1)
          (color==metal_color_name[2] color=2)
          (color==metal_color_name[3] color=3)
          (t color=nil)
          )
    color
    )
  )

; set color number of a shape
(defun ColorizeShape (shape color)
  (dbSetShapeColor shape metal_color_name[color])
  )

; set color numbers of a symbolic via
(defun ColorizeVia (via color color_b color_t)
  (when color   (dbSetViaCutLayerControl via via_color_name[color]))
  (when color_b (dbSetViaLayer1Control   via via_color_name[color_b]))
  (when color_t (dbSetViaLayer2Control   via via_color_name[color_t]))
  )

; flip color of specified metal layer number
(defun FlipColor (m @key (CV (geGetEditCellView)))
  (let (color)
    (foreach shape CV->shapes
             (when shape->objType!="label" && shape->layerName==Metal[m]
                   color = (GetShapeColor shape)
                   (cond (color==1 color=2)
                         (color==2 color=1)
                         )
                   (ColorizeShape shape color)
                   )
             )
    )
  t
  )

; Assign maskColor using overlay layers, then optionally delete overlay layers
(defun ColorizeByOverlay (@key (CV (geGetEditCellView)) (delete nil))
  (let (layer color overlaps lpp via)
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
                                      (t (ColorizeVia o color nil nil))
                                      )
                                )
                              ((atom o) && o->objType!="label"
                                   (ColorizeShape o color)
                                   )
                              )
                        )
               (when delete (dbDeleteObject s))
               )
             )
    )
  t
  )
