; Version of MakeLayoutTag for custom leaf cell extraction for 1278.3
(defun MakeLeafTag (@key (CV (geGetEditCellView))
                         (targetViewName "layout_tag"))
  (let (tagCV CopyExact prb lpp z th pcre fillCV bbox rows cols x0 y0 x1 y1 fill x y fig rect points)
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
    (unless tagCV (error "Unable to write %s %s %s\n" libName cellName targetViewName))

    ; instantiate layout cell
    (dbCreateInst tagCV CV "dut" 0:0 "R0")

    ; copy alignment properties
    align=(GetCellAlignment ?CV CV)
    (SetCellAlignment (car align) (cadr align) ?CV tagCV)

    ; copy prb
    (unless CV->prBoundary->points (error "No prBoundary"))
    prb = (dbCreatePRBoundary tagCV CV->prBoundary->points)

    ; add fill cells on all sides
    fillCV=(ReadCV "vendor.intel.i0s"
                   (sprintf nil "vendor.intel.i0s.zdsd%d%da%s.1n01x5" z z th)
                   "layout")
    bbox=prb->bBox
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
    (DrawChannel BusFill0 (NodeArrayChan 0 (rows+2)*tracks_m0+1) "fill0" points ?CV tagCV)
    (DrawChannel BusFill1 (NodeArrayChan 1 (cols+4)*tracks_m1)   "fill1" (list x:y x:y1+TrackPitch) ?CV tagCV)
    (DrawChannel BusFill2 (NodeArrayChan 2 (rows+2)*tracks_m2+1) "fill2" points ?CV tagCV)

    ; chop m0 over cell
    points=(BBoxToPoints (BBoxExpandVertical (BBoxExpandHorizontal bbox MetalEteSpace[Metal[0]]/2) 0.016))
    (foreach s2 tagCV->shapes
             (when s2->layerName=="m0" (leChopShape s2 points t t))
             )

    ; chop fill by subcell wires
    flatCV=(MakeFlatBlockageView ?botMetal 0 ?topMetal 2 ?CV CV)
    (foreach s1 flatCV->shapes
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

    ; tiny in-place pins and labels on non-BM0
    (foreach label (setof l CV->shapes l->objType=="label" && l->layerName!="bm0")
             lpp = (list (car label->lpp) "pin")
             (MakeLayoutTagLabel tagCV lpp label->xy label->theLabel)
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
