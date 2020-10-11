; always color metals based on track position since this is invariable
; in a fixed track pattern via colors are assigned by ICC during
; routing, so don't want to change those unless they are uncolored
(defun Colorize (@key (CV (geGetEditCellView)) (doVias t) (override nil) (offset nil))
  (let (bbox color shapes track oldcolor track1 track2 color1 color2 colorc viaNum)
    (unless offset offset=(cadr (GetCellAlignment ?CV CV)))
    offset = -(car offset):-(cadr offset) ; reverse offset
    (foreach metal ColorableMetals
       shapes = (setof s CV->shapes s->layerName==metal && s->purpose!="bus")
       (foreach shape shapes
          bbox=(dbTransformBBox shape->bBox (list offset "R0"))
          (when shape->objType=="rect"
	     track = (GetTrack (MetalNumber metal) bbox)
	     )
	  (when shape->objType=="path" || shape->objType=="pathSeg"
             track = (GetTrack (MetalNumber metal) bbox)
	     )
	  (unless shape->objType=="label" || (cadr shape->lpp)=="AABBregion"
	     (if !track then
               (printf "ERROR: Can't color off-track %s at %f:%f\n"
                       (car shape->lpp) (caar bbox) (cadar bbox))
   	     else
                color = (GetTrackColor (MetalNumber shape->layerName) track)
	        (when color (SetShapeColor shape color))
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
                        bbox=(dbTransformBBox shape->bBox (list offset "R0"))
                        (when shape->objType=="rect"
                              track1 = (GetTrack viaNum   bbox ?exact nil)
                              track2 = (GetTrack viaNum+1 bbox ?exact nil)
                              track = (if (mod viaNum 2)==0 track1 track2)
                              ; (printf "viaNum=%d track=%d\n" viaNum track)
                              color = (GetViaTrackColor viaNum track)
                              oldcolor = (GetShapeColor shape)
                              (when color && (override || oldcolor==0)
                                    (SetShapeColor shape color)
                                    )
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
             oldcolor = (GetViaColor via)
             (when override || oldcolor==0
                   (cond ((strncmp via->viaHeader->master->cellName "via0" 4)==0 viaNum=0)
                         ((strncmp via->viaHeader->master->cellName "via1" 4)==0 viaNum=1)
                         ((strncmp via->viaHeader->master->cellName "via2" 4)==0 viaNum=2)
                         ((strncmp via->viaHeader->master->cellName "via3" 4)==0 viaNum=3)
                         ((strncmp via->viaHeader->master->cellName "via4" 4)==0 viaNum=4)
                         ((strncmp via->viaHeader->master->cellName "via5" 4)==0 viaNum=5)
                         (t viaNum=nil)
                         )
                   (when viaNum==1 || viaNum==2 || viaNum==3
                         bbox = (GetViaOverlapBBox via)
                         bbox = (dbTransformBBox bbox (list offset "R0"))
                         track1 = (GetTrack viaNum   bbox)
                         track2 = (GetTrack viaNum+1 bbox)
                         (when track1 color1 = (GetTrackColor viaNum   track1))
                         (when track2 color2 = (GetTrackColor viaNum+1 track2))
                         track = (if (mod viaNum 2)==0 track1 track2)
                         ; (printf "viaNum=%d track=%d\n" viaNum track)
                         colorc = (GetViaTrackColor viaNum track)
                         (SetViaColor via (if doVias colorc nil) color1 color2)
                         (unless colorc
                           (printf "ERROR: Can't color off-track %s at %f:%f\n"
                                   via->viaHeader->master->cellName
                                   (car via->origin) (cadr via->origin))
                           )
                         )
                   ; for via0/4/5 cut is uncolored but the metals are colored
                   (when viaNum==0 || viaNum==4 || viaNum==5
                         bbox = (GetViaOverlapBBox via)
                         bbox = (dbTransformBBox bbox (list offset "R0"))
                         track1 = (GetTrack viaNum   bbox)
                         track2 = (GetTrack viaNum+1 bbox)
                         (when track1 color1 = (GetTrackColor viaNum   track1))
                         (when track2 color2 = (GetTrackColor viaNum+1 track2))
                         track = (if (mod viaNum 2)==0 track1 track2)
                                        ; (printf "viaNum=%d track=%d\n" viaNum track)
                         colorc = (GetViaTrackColor viaNum track)
                         (if track1 && track2
                             (SetViaColor via nil color1 color2)
                             (printf "ERROR: Can't color off-track %s at %f:%f\n"
                                     via->viaHeader->master->cellName
                                     (car via->origin) (cadr via->origin))
                             )
                         )
                   )
             )
    t
    )
  )

; Change all shapes and vias to uncolored
(defun UnColorize (@key (CV (geGetEditCellView)))
  (foreach shape CV->shapes (SetShapeColor shape 0))
  (foreach via   CV->vias   (SetViaColor   via 0 0 0))
  t
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

; Return the bbox of the overlap between top and bottom layers of a via
(defun GetViaOverlapBBox (via)
  (let (bbox overhang1 overhang2 bbox1 bbox2)
    bbox = (GetCutBBox via)
    overhang1 = (nth 6 via->viaHeader->viaDef->params)
    overhang2 = (nth 7 via->viaHeader->viaDef->params)
    bbox1 = (BBoxExpandHorizontal bbox  (car  overhang1))
    bbox1 = (BBoxExpandVertical   bbox1 (cadr overhang1))
    bbox2 = (BBoxExpandHorizontal bbox  (car  overhang2))
    bbox2 = (BBoxExpandVertical   bbox2 (cadr overhang2))
    bbox  = (BBoxAnd bbox1 bbox2)
    )
  )

; get the color number of a shape
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
(defun SetShapeColor (shape color)
  (dbSetShapeColor shape metal_color_name[color])
  )

; get the color number of the via cut
(defun GetViaColor (via)
  (let (color)
    color = (dbGetViaCutLayerControl via)
    (cond (color==via_color_name[0] color=0)
          (color==via_color_name[1] color=1)
          (color==via_color_name[2] color=2)
          (color==via_color_name[3] color=3)
          (t color=nil)
          )
    color
    )
  )

; set color numbers of a symbolic via
(defun SetViaColor (via color color_b color_t)
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
                   (SetShapeColor shape color)
                   )
             )
    )
  t
  )

; Assign maskColor using overlay layers, then optionally delete overlay layers
(defun ColorizeByOverlay (@key (CV (geGetEditCellView)) (doVias t))
  (let (n layer color isvia keep overlaps ov via to_delete)
    n=0
    (foreach s CV->shapes
      isvia=nil
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
            (doVias && s->lpp==(list "v1a" "colorA") layer="v1" color=1 isvia=t)
            (doVias && s->lpp==(list "v1b" "colorB") layer="v1" color=2 isvia=t)
            (doVias && s->lpp==(list "v1c" "colorC") layer="v1" color=3 isvia=t)
            (doVias && s->lpp==(list "v2a" "colorA") layer="v2" color=1 isvia=t)
            (doVias && s->lpp==(list "v2b" "colorB") layer="v2" color=2 isvia=t)
            (doVias && s->lpp==(list "v2c" "colorC") layer="v2" color=3 isvia=t)
            (doVias && s->lpp==(list "v3a" "colorA") layer="v3" color=1 isvia=t)
            (doVias && s->lpp==(list "v3b" "colorB") layer="v3" color=2 isvia=t)
            (t layer=nil color=nil)
            )
      (when layer
        n++
        (when (n&0x3FF)==0 (printf "processed %d color overlay shapes\n" n))
        keep=nil
        (foreach purpose (list "drawing" "pin" "fill" "actFill")
          overlaps = (dbGetOverlaps CV s->bBox (list layer purpose) (if isvia 0:31 0:1))
          (foreach o overlaps
            (cond ((atom o) && o->objType!="label"
                   ; (printf "Colorize shape on %s\n" layer)
                   (SetShapeColor o color)
                   )
                  (!(atom o) && (car o)->objType=="stdVia"
                   ; (printf "Colorize via on %s\n" layer)
                   ov=(car o)
                   via=ov->viaHeader->viaDef
                   (cond (isvia                    (SetViaColor ov color nil nil))
                         (via->layer1->name==layer (SetViaColor ov nil color nil))
                         (via->layer2->name==layer (SetViaColor ov nil nil color))
                         )
                   )
                  (isvia && (GetOverlapObjType o)!="label" && (GetOverlapColor o layer)!=color ; keep via overlay
                   ; (printf "Keep via overlay on %s\n" layer)
                   keep=t
                   )
                  )
            )
          )
        (unless keep to_delete=(cons s to_delete)) ; NOTE: defer delete runs faster!
        )
      )
    (foreach s to_delete (dbDeleteObject s))
    )
  t
  )

; recursively figure out color of an overlap from dbGetOverlaps
(defun GetOverlapColor (overlap layer)
  (let (via c color)
    (cond ((atom overlap) (GetShapeColor overlap))
          (!(atom overlap) && (car overlap)->objType=="stdVia" ; NOTE: vias are tricky!
            via=(car overlap)
            (cond (via->viaHeader->viaDef->layer1->name==layer c=(dbGetViaLayer1Control   via))
                  (via->viaHeader->viaDef->layer2->name==layer c=(dbGetViaLayer2Control   via))
                  (t                                           c=(dbGetViaCutLayerControl via))
                  )
            (for i 0 3 (when via_color_name[i]==c color=i))
            color
            )
          (t (GetOverlapColor (cadr overlap) layer))
          )
    )
  )

; recursively figure out objType of an overalp from dbGetOverlaps
(defun GetOverlapObjType (overlap)
  (cond ((atom overlap) overlap->objType)
        (t (GetOverlapObjType (cadr overlap)))
        )
  )

; Recolor V1C or V2C to A or B to to avoid C spacing conflicts
(defun ColorizeFixViaC (@key (CV (geGetEditCellView)))
  (let (bbox bbox1 bbox2 lpp overlaps colors c viaName layerName conflict)
    (for v 1 2
         layerName=(sprintf nil "v%d" v)
         viaName=(sprintf nil "via%d" v)

         ; fix symbolic vias
         (foreach via (setof x CV->vias
                             (strncmp x->viaHeader->master->cellName viaName 4)==0 &&
                             (GetViaColor x)==3)
                  bbox  = (GetCutBBox via)
                  bbox1 = (BBoxExpandHorizontal bbox  0.07) ; V1_29
                  bbox1 = (BBoxExpandVertical   bbox1 0.03)
                  bbox2 = (BBoxExpandHorizontal bbox  0.03)
                  bbox2 = (BBoxExpandVertical   bbox2 0.11) ; V1_128
                  overlaps=nil
                  (foreach purpose (list "drawing" "fill")
                           lpp = (list layerName purpose)
                           overlaps=(append overlaps (dbGetOverlaps CV bbox1 lpp 0:31))
                           overlaps=(append overlaps (dbGetOverlaps CV bbox2 lpp 0:31))
                           )
                  colors=nil
                  conflict=nil
                  (foreach o overlaps
                           c=(GetOverlapColor o layerName)
                           colors=(cons c colors)
                           (when c==3 && o!=via conflict=t)
                      )
                  (cond (!conflict t)
                        ((member 1 colors)==nil (SetViaColor via 1 nil nil))
                        ((member 2 colors)==nil (SetViaColor via 2 nil nil))
                        )
                  )

         ; fix via shapes too
         (foreach via (setof x CV->shapes x->lpp==(list (sprintf nil "v%d" v) "drawing") &&
                             (GetShapeColor x)==3)
                  bbox  = via->bBox
                  bbox1 = (BBoxExpandHorizontal bbox  0.07) ; V1_29
                  bbox1 = (BBoxExpandVertical   bbox1 0.03)
                  bbox2 = (BBoxExpandHorizontal bbox  0.03)
                  bbox2 = (BBoxExpandVertical   bbox2 0.11) ; V1_128
                  overlaps=nil
                  (foreach purpose (list "drawing" "fill")
                           lpp = (list layerName purpose)
                           overlaps=(append overlaps (dbGetOverlaps CV bbox1 lpp 0:31))
                           overlaps=(append overlaps (dbGetOverlaps CV bbox2 lpp 0:31))
                           )
                  colors=nil
                  conflict=nil
                  (foreach o overlaps
                           c=(GetOverlapColor o layerName)
                           colors=(cons c colors)
                           (when c==3 && o!=via conflict=t)
                           )
                  (cond (!conflict t)
                        ((member 1 colors)==nil (SetShapeColor via 1))
                        ((member 2 colors)==nil (SetShapeColor via 2))
                        )
                  )
         )
    )
  t
  )
