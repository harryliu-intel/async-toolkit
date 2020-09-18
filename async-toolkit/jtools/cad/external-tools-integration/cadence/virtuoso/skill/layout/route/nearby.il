; Modernized rewrite of nrCreateNeighborNetConnector
; plus functionality to strap intersecting pins with vias

; route nearby nets with short paths or vias only
(defun RouteNearby (@key (CV (geGetEditCellView)) (nets nil) (topShapes t) (subShapes t) (offGrid t) (verbose nil))
  (let (n c pct)
    n=0
    (unless nets nets=(setof a CV->nets (member a->name PowerGroundNets)==nil))
    (foreach net nets
             n+=(RouteNearbyNet net ?CV CV ?topShapes topShapes ?subShapes subShapes
                                ?offGrid offGrid ?verbose verbose)
             )
    (printf "Note that RouteNearby only connects abutting pins between locked instances.")
    n
    )
  )

; connect subcells pins of a net with short paths or vias
(defun RouteNearbyNet (net @key (CV (geGetEditCellView)) (topShapes t) (subShapes t) (offGrid t) (verbose nil))
  (let (lpps tech n MasterRectList layerName pinRectList layerNum space len
             e1 e2 inst1 inst2 net1 net2 rect1 rect2 overlap rect horz
             r1x0 r1x1 r1y0 r1y1 r2x0 r2x1 r2y0 r2y1
             viaDef via list1 list2 xy wb wt viaName viaDef via)
    tech = (techGetTechFile CV)
    n=0
    (for m 0 TopMetal
         lpps = (cons MetalDrawing[m] lpps)
         lpps = (cons MetalPin[m] lpps)
         )
    MasterRectList = (nrFindPinShapesWithNet net lpps ?topShapes topShapes ?subShapes subShapes)

    ; add paths between subcell pins that align at e2e spacing
    (foreach pinRectList MasterRectList
      layerName=(car pinRectList)
      pinRectList=(cadr pinRectList)
      layerNum = MetalNum[layerName]
      (unless MetalHaloBoundarySpace[layerName]
        (error "No MetalHaloBoundarySpace defined for layer %s\n" layerName)
        )
      space = 2 * MetalHaloBoundarySpace[layerName]
      horz = (mod layerNum 2)==0
      list1=pinRectList
      (while list1
        e1    = (car list1)
        list1 = (cdr list1)
        inst1 = (car   e1)
        net1  = (cadr  e1)
        rect1 = (caddr e1)
        rect1 = (BBoxCanonicalize rect1)
        list2 = list1
        (while list2
          e2    = (car list2)
          list2 = (cdr list2)
          inst2 = (car   e2)
          net2  = (cadr  e2)
          rect2 = (caddr e2)
          rect2 = (BBoxCanonicalize rect2)
          (when net1!=nil && net2!=nil && (net1!=net2 || inst1!=inst2) && !(BBoxAnd rect1 rect2)
            overlap=nil
            r1x0=(car  (car  rect1))
            r1x1=(car  (cadr rect1))
            r2x0=(car  (car  rect2))
            r2x1=(car  (cadr rect2))
            r1y0=(cadr (car  rect1))
            r1y1=(cadr (cadr rect1))
            r2y0=(cadr (car  rect2))
            r2y1=(cadr (cadr rect2))
            (cond (horz && (round (r1y0-r2y0)/MfgGrid)==0 && (round (r1y1-r2y1)/MfgGrid)==0 ; horizontal aligned
                   (cond ((round (r1x1-r2x0)/MfgGrid)<0 && (round (r2x0-r1x1-space)/MfgGrid)<=0
                          overlap=(list r1x1:r1y0 r2x0:r1y1))
                         ((round (r2x1-r1x0)/MfgGrid)<0 && (round (r1x0-r2x1-space)/MfgGrid)<=0
                          overlap=(list r2x1:r1y0 r1x0:r1y1))
                         )
                   )
                  (!horz && (round (r1x0-r2x0)/MfgGrid)==0 && (round (r1x1-r2x1)/MfgGrid)==0 ; vertical aligned
                   (cond ((round (r1y1-r2y0)/MfgGrid)<0 && (round (r2y0-r1y1-space)/MfgGrid)<=0
                          overlap=(list r1x0:r1y1 r1x1:r2y0))
                         ((round (r2y1-r1y0)/MfgGrid)<0 && (round (r1y0-r2y1-space)/MfgGrid)<=0
                          overlap=(list r1x0:r2y1 r1x1:r1y0))
                         )
                   )
                  )
            (when overlap
              rect = (dbCreateRect CV (list layerName "drawing") overlap)
              rect->routeStatus="locked"
              (dbAddFigToNet rect net)
              (dbReplaceProp rect "autogen" "string" "RouteNearby")
              (when verbose (printf "%s on %s\n" layerName net->name))
              n++
              )
            )
          )
        )
      )

    ; add vias between subcell pins or top-level shapes that overlap
    (for v 0 TopMetal-1
      list1 = (setof a MasterRectList (car a)==Metal[v])
      list2 = (setof a MasterRectList (car a)==Metal[v+1])
      list1 = (cadr (car list1))
      list2 = (cadr (car list2))
      (foreach e1 list1
        inst1 = (car   e1)
        net1  = (cadr  e1)
        rect1 = (BBoxCanonicalize (caddr e1))
        (foreach e2 list2
          inst2 = (car  e2)
          net2  = (cadr e2)
          rect2 = (BBoxCanonicalize (caddr e2))
          overlap = (BBoxAnd rect1 rect2)
          (when overlap && (net1!=net2 || inst1!=inst2 || net1==nil && net2==nil) &&
                (dbGetOverlaps CV overlap (list Via[v] "drawing") 0:31)==nil
            xy = (BBoxGetCenter overlap)
            wb = (if offGrid (GetWireWidth v rect1) (GetTrackWidth v xy))
            (unless wb (error "no track width for %d %g:%g\n" v (car xy) (cadr xy)))
            wt = (if offGrid (GetWireWidth v+1 rect2) (GetTrackWidth v+1 xy))
            (unless wt (error "no track width for %d %g:%g\n" v+1 (car xy) (cadr xy)))
            wb = (round wb/MfgGrid)
            wt = (round wt/MfgGrid)
            viaName = viaTable[(list v wb wt)]
            (unless viaName (error "no viaTable for %d %d %d\n" v wb wt))
            (cond ((atom viaName)
                   viaDef = (techFindViaDefByName tech viaName)
                   params = nil
                   )
                  (t
                   viaDef = (techFindViaDefByName tech (car viaName))
                   params = (cdr viaName)
                   viaName = (car viaName)
                   )
                  )
            via = (dbCreateVia CV viaDef xy "R0" params)
            via->routeStatus="locked"
            via->net = net
            (dbReplaceProp via "autogen" "string" "RouteNearby")
            (when verbose (printf "%s on %s\n" Via[v] net->name))
            n++
            )
          )
        )
      )
    n
    )
  )

; get the width of a wire from its layer number and bBox
(defun GetWireWidth (m bbox)
  (if (mod m 2)==0
    (cadr (cadr bbox))-(cadr (car bbox)) ; horizontal wire
    (car  (cadr bbox))-(car  (car bbox)) ; vertical wire
    )
  )
