; Version of MakeLayoutTag for custom leaf cell extraction for 1278.3
(defun MakeLeafTag (@key (CV (geGetEditCellView))
                         (targetViewName "layout_tag"))
  (let (tagCV CopyExact lpp z th pcre fillCV abstractCV bbox rows cols
              ete0 ete1 ete2 w h x0 y0 x1 y1 fill x y fig rect points l0 l1 l2 pins)
    (printf "MakeLeafTag %s\n" CV->cellName)

    ; find abstract view
    abstractCV=(ReadCV CV->libName CV->cellName "abstract")
    (unless abstractCV (error "abstract view not found"))

    ; get CopyExact, threshold, and strength
    CopyExact = (dbGetPropByName CV "CopyExact")->value
    (unless CopyExact (error "no CopyExact"))
    z=(if (pcreMatchp "i0s............x1" CopyExact) 1 2)
    (cond ((pcreMatchp "i0s.......a......" CopyExact) th="a")
          ((pcreMatchp "i0s.......b......" CopyExact) th="b")
          ((pcreMatchp "i0s.......c......" CopyExact) th="c")
          )

    ; write destination tagCV
    tagCV = (dbOpenCellViewByType CV->libName CV->cellName targetViewName "maskLayout" "w")
    (unless tagCV (error "Unable to write %s %s %s\n" CV->libName CV->cellName targetViewName))
    (dbReplaceProp tagCV "CopyExact" "string" CopyExact)

    ; instantiate layout cell
    (dbCreateInst tagCV CV "dut" 0:0 "R0")

    ; copy alignment properties
    align=(GetCellAlignment ?CV CV)
    (SetCellAlignment (car align) (cadr align) ?CV tagCV)

    ; copy prb
    (unless CV->prBoundary->points (error "No prBoundary"))
    bbox=CV->prBoundary->bBox
    bbox=(BBoxExpandHorizontal bbox 2*GridPolyPitch)
    bbox=(BBoxExpandVertical   bbox TrackPitch)
    (dbCreatePRBoundary tagCV (BBoxToPoints bbox))

    ; add fill cells on all sides
    fillCV=(ReadCV "vendor.intel.i0s"
                   (sprintf nil "vendor.intel.i0s.zdsd%d%da%s.1n01x5" z z th)
                   "layout")
    bbox=CV->prBoundary->bBox
    x0=(car  (car  bbox))
    x1=(car  (cadr bbox))
    y0=(cadr (car  bbox))
    y1=(cadr (cadr bbox))
    cols=(round (x1-x0)/GridPolyPitch)
    rows=(round (y1-y0)/TrackPitch)
    fill=0
    (for row -1 rows
         (for col -2 cols+1
              x=x0+col*GridPolyPitch
              y=y0+row*TrackPitch
              (when col<0 || col>=cols || row<0 || row>=rows
                    inst=(dbCreateInst tagCV fillCV (sprintf nil "fill%d" fill) x:y "R0")
                    (LegalizeShortInstance inst)
                    fill++
                    )
              )
         )

    ; draw regular grid of m0, m1, m2
    x=x0-2*GridPolyPitch
    y=y0-TrackPitch
    points=(list x:y x1+2*GridPolyPitch:y)
    ete0=MetalEteSpace[Metal[0]]
    ete1=MetalEteSpace[Metal[1]]
    ete2=MetalEteSpace[Metal[2]]
    (DrawChannel BusMetal0 (NodeArrayChan 0 (rows+2)*tracks_m0+1) "metal0_zone_" points ?CV tagCV)
    (DrawChannel BusMetal1 (NodeArrayChan 1 (cols+4)*tracks_m1)   "metal1_zone_" (list x:y0+ete1/2 x:y1-ete1/2) ?CV tagCV)
    (DrawChannel BusMetal2 (NodeArrayChan 2 (rows+2)*tracks_m2+1) "metal2_zone_" (list x0+ete2/2:y x1-ete2/2:y) ?CV tagCV)

    ; chop m0 over cell
    points=(BBoxToPoints (BBoxExpandVertical (BBoxExpandHorizontal bbox ete0/2) 0.016))
    (foreach s2 tagCV->shapes
             (when s2->layerName=="m0" (leChopShape s2 points t t))
             )

    ; chop fill by abstract shapes
    (foreach s1 abstractCV->shapes
             (cond (s1->layerName=="m0" rect=(BBoxExpandHorizontal s1->bBox MetalEteSpace[Metal[0]]))
                   (s1->layerName=="m1" rect=(BBoxExpandVertical   s1->bBox MetalEteSpace[Metal[1]]))
                   (s1->layerName=="m2" rect=(BBoxExpandHorizontal s1->bBox MetalEteSpace[Metal[2]]))
                   (t rect=nil)
                   )
             (when rect
               points=(BBoxToPoints rect)
               (foreach s2 tagCV->shapes
                        (when s2->layerName==s1->layerName (leChopShape s2 points t t))
                        )
               )
             )

    ; chop fill by abstract blockages
    (foreach s1 abstractCV->blockages
             (cond (s1->layer=="m0" rect=(BBoxExpandHorizontal s1->bBox MetalEteSpace[Metal[0]]))
                   (s1->layer=="m1" rect=(BBoxExpandVertical   s1->bBox MetalEteSpace[Metal[1]]))
                   (s1->layer=="m2" rect=(BBoxExpandHorizontal s1->bBox MetalEteSpace[Metal[2]]))
                   (t rect=nil)
                   )
             (when rect
               points=(BBoxToPoints rect)
               (foreach s2 tagCV->shapes
                        (when s2->layerName==s1->layer (leChopShape s2 points t t))
                        )
               )
             )

    ; delete small fill shapes
    l0=(round 0.084/MfgGrid) ; M0_L_81
    l1=(round 0.042/MfgGrid) ; M1_L_61
    l2=(round 0.085/MfgGrid) ; M2_L_81
    (foreach s tagCV->shapes
             w=(car  (cadr s->bBox))-(car  (car s->bBox))
             h=(cadr (cadr s->bBox))-(cadr (car s->bBox))
             w=(round w/MfgGrid)
             h=(round h/MfgGrid)
             (cond (s->layerName=="m0" && w<l0 (dbDeleteObject s))
                   (s->layerName=="m1" && h<l1 (dbDeleteObject s))
                   (s->layerName=="m2" && w<l2 (dbDeleteObject s))
                   )
             )

    ; add pins on fill shapes too
    (foreach s tagCV->shapes
             (when s->purpose=="drawing"
                   fig=(dbCreateRect tagCV (list s->layerName "pin") s->bBox)
                   (dbCreatePin s->net fig)
                   (LabelPin tagCV fig)
                   )
             )

    ; tiny in-place pins and labels M2, M1, M0 removing duplicate pins
    pins=(makeTable "pins" nil)
    (foreach layerName (list "m2" "m1" "m0")
             (foreach label (setof l CV->shapes l->objType=="label" && l->layerName==layerName &&
                                   l->theLabel!=VddNetName && l->theLabel!=VssNetName &&
                                   pins[l->theLabel]==nil)
                      pins[label->theLabel]=t
                      lpp = (list (car label->lpp) "pin")
                      (MakeLayoutTagLabel tagCV lpp label->xy label->theLabel)
                      )
             )

    ; copy entire pins on BM0
    (foreach s CV->shapes
             (when s->pin && s->layerName=="bm0"
                   fig=(dbCopyFig s tagCV)
                   fig->net=(dbMakeNet tagCV s->net->name)
                   (dbCreatePin fig->net fig)
                   )
             )

    ; save and return
    (dbSave tagCV)
    tagCV
    )
  )
