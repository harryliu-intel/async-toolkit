; Print closest reset net for unconnected reset pins of subcells.
;
; Copyright 2011 Fulcrum Microsystems.  All rights reserved.

; find bounding box of everything connected to a net
(defun FindNetBBox (net)
  (let (bbox pins)
    bbox = nil
    (foreach pin net->pins bbox = (BBoxCombine bbox pin->fig->bBox))
    (foreach fig net->figs bbox = (BBoxCombine bbox fig->bBox))
    pins = (nrFindPinShapes net MetalLPPs)
    (foreach entry pins
             (foreach rect (cadr entry)
                      rect = (BBoxCanonicalize rect)
                      bbox = (BBoxCombine bbox rect)
                      )
             )
    (unless pins
      (foreach term net->instTerms
               bbox = (BBoxCombine bbox term->inst->bBox))
      )
    bbox
    )
  )

; Distance between two bbox's.  Zero if bbox'es overlap.
(defun DistanceBetweenBBoxes (bbox1 bbox2 @key (scaleX 1) (scaleY 1))
  (let (bbox dx dy)
    bbox = (BBoxCombine bbox1 bbox2)
    dx = (BBoxGetWidth  bbox) - (BBoxGetWidth  bbox1) - (BBoxGetWidth  bbox2)
    dy = (BBoxGetHeight bbox) - (BBoxGetHeight bbox1) - (BBoxGetHeight bbox2)
    dx = (max dx 0)
    dy = (max dy 0)
    scaleX*dx + scaleY*dy
    )
  )

; Find output nets of all RAMP_RESET cells
(defun FindRampResetNets
  (@key (CV (geGetEditCellView))
        (ramptype "standard.reset.RAMP_RESET"))
  (let (ramps resets)
    ramps = (setof inst CV->instances
                   (strncmp inst->cellName ramptype (strlen ramptype))==0)
    resets = nil
    (foreach inst ramps
      (foreach term inst->instTerms
               (when term->name=="_RES" resets = (cons term->net resets))
               )
      )
    resets
    )
  )

; Find unconnected reset pins of subcells
(defun FindUnconnectedResetNets (@key (CV (geGetEditCellView)) (resets nil))
  (let (unconnected)
    (unless resets resets = (FindRampResetNets ?CV CV))
    unconnected = nil
    (foreach net (setof net CV->nets net->term==nil && !(memq net resets))
      (when (setof term net->instTerms
                   ((strncmp term->name "_reset" 6)==0 ||
                    (strncmp term->name "_RESET" 6)==0))
        unconnected = (cons net unconnected)
        )
      )
    unconnected
    )
  )

; Print CAST connecting all subcells with unconnected _reset or _RESET
; pins to the closest RAMP_RESET output
(defun FindClosestResets
  (@key (CV (geGetEditCellView))
        (filename "closest_reset.cast")
        (ramptype "standard.reset.RAMP_RESET")
        (scaleX 1)
        (scaleY 1)
        )
  (let (file ramps resets best_reset best_distance distance unconnected
             net_bBox reset_bBox lines name1 name2 sub_dist best_sub_dist)
    (unless CV->viewName=="prelayout" || CV->viewName=="floorplan"
            (error "Run on floorplan or prelayout view\n"))
    file = (outfile filename)
    (unless file (error "Can't write %s\n" filename))
    (fprintf file "    // connect reset using (FindClosestResets)\n")

    ; find available RAMP_RESET output nets
    resets = (FindRampResetNets ?CV CV ?ramptype ramptype)

    ; find unconnected local reset nets
    unconnected = (FindUnconnectedResetNets ?CV CV ?resets resets)

    ; connect each to closest reset
    (rexCompile "\\]\\[") ; convert ][ to ,
    lines = nil
    (foreach net unconnected
      net_bBox = (FindNetBBox net)
      best_reset = nil
      best_distance = 0
      best_sub_dist = 0
      (foreach reset resets
        reset_bBox = (FindNetBBox reset)
        ; primary metric is the distance between bounding boxes
        distance = (DistanceBetweenBBoxes net_bBox reset_bBox
                                          ?scaleX scaleX ?scaleY scaleY)
        ; tie-breaker metric is half perimeter of the reset_bBox
        sub_dist = scaleX*(BBoxGetWidth  reset_bBox) +
                   scaleY*(BBoxGetHeight reset_bBox)
        ; track best reset choice
        (when !best_reset || distance<best_distance ||
              distance==best_distance && sub_dist<best_sub_dist
              best_reset = reset
              best_distance = distance
              best_sub_dist = sub_dist
              )
        )
      ; print connection to best reset
      (when best_reset
        name1 = (rexReplace best_reset->name "," 0)
        name2 = (rexReplace net->name "," 0)
        lines = (cons (sprintf nil "    %s = %s;\n" name1 name2) lines)
        )
      )

    ; finish file
    lines = (sort lines nil)
    (foreach line lines (fprintf file "%s" line))
    (close file)
    (printf "Wrote %s\n" filename)
    t
    )
  )

; Check if two sets of pin shapes overlap
(defun DoPinShapesConnect (pins1 pins2)
  (let (layer1 layer2 overlap)
    overlap = nil
    (foreach shapes1 pins1
      layer1 = (car shapes1)
      (foreach bbox1 (cadr shapes1)
        (foreach shapes2 pins2
          layer2 = (car shapes2)
          (when layer1==layer2
            (foreach bbox2 (cadr shapes2)
              (when (BBoxAnd bbox1 bbox2) overlap=t)
              )
            )
          )
        )
      )
    overlap
    )
  )

; Print CAST connecting resets that are already shorted in the layout.
; Used for ARRAY.
(defun FindShortedResets
  (@key (CV (geGetEditCellView))
        (filename "shorted_reset.cast")
        (ramptype "standard.reset.RAMP_RESET")
        (pin_lpps MetalLPPs)
        )
  (let (file resets unconnected net1 net2 pins1 pins2 name1 name2)
    (unless CV->viewName=="prelayout" || CV->viewName=="floorplan"
            (error "Run on floorplan or prelayout view\n"))
    file = (outfile filename)
    (unless file (error "Can't write %s\n" filename))
    (fprintf file "    // connect reset using (FindShortedResets)\n")

    ; find available RAMP_RESET output nets
    resets = (FindRampResetNets ?CV CV ?ramptype ramptype)

    ; find unconnected local reset nets
    unconnected = (FindUnconnectedResetNets ?CV CV ?resets resets)

    ; report overlapping pins
    (rexCompile "\\]\\[") ; convert ][ to ,
    (for i 0 (length unconnected)-1
      net1 = (nth i unconnected)
      pins1 = (nrFindPinShapes net1 pin_lpps)
      (for j i+1 (length unconnected)-1
        net2 = (nth j unconnected)
        pins2 = (nrFindPinShapes net2 pin_lpps)
        (when net1!=net2 && (DoPinShapesConnect pins1 pins2)
          name1 = (rexReplace net1->name "," 0)
          name2 = (rexReplace net2->name "," 0)
          (fprintf file "    %s = %s;\n" name1 name2)
          )
        )
      )

    ; close file
    (close file)
    (printf "Wrote %s\n" filename)
    t
    )
  )
