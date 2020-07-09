; Modernized rewrite of nrCreateNeighborNetConnector
; plus functionality to strap intersecting pins with vias

; route nearby nets with short paths or vias only
(defun RouteNearby (@key (CV (geGetEditCellView)) (nets nil) (topShapes t) (offGrid t) (verbose nil))
  (let (n)
    n=0
    (unless nets nets=(setof a CV->nets (member a->name PowerGroundNets)==nil))
    (foreach net nets n=n+(RouteNearbyNet net ?CV CV ?topShapes topShapes ?offGrid offGrid ?verbose verbose))
    (printf "Note that RouteNearby only connects abutting pins between locked instances.")
    n
    )
  )

; connect subcells pins of a net with short paths or vias
(defun RouteNearbyNet (net @key (CV (geGetEditCellView)) (topShapes t) (offGrid t) (verbose nil))
  (let (lpps tech n MasterRectList layerName pinRectList layerNum space len
             inst1 inst2 net1 net2 rect1 rect2 overlap rect
             a0 b0 a1 b1 x0 x1 y0 y1
             viaDef via list1 list2 xy wb wt viaName viaDef via)
    tech = (techGetTechFile CV)
    n=0
    (for m 0 TopMetal
         lpps = (cons MetalDrawing[m] lpps)
         lpps = (cons MetalPin[m] lpps)
         )
    MasterRectList = (nrFindPinShapesWithNet net lpps ?topShapes topShapes)

    ; add paths between subcell pins that align at e2e spacing
    (foreach pinRectList MasterRectList
             layerName=(car pinRectList)
             pinRectList=(cadr pinRectList)
             layerNum = MetalNum[layerName]
             (unless MetalHaloBoundarySpace[layerName]
               (error "No MetalHaloBoundarySpace defined for layer %s\n" layerName)
               )
	     space = 2 * MetalHaloBoundarySpace[layerName]
             len = (length pinRectList)
             (for a 0 len-1
               inst1 = (car   (nth a pinRectList))
               net1  = (cadr  (nth a pinRectList))
               rect1 = (caddr (nth a pinRectList))
               rect1 = (BBoxCanonicalize rect1)
               (for b a+1 len-1
                  inst2 = (car   (nth b pinRectList))
                  net2  = (cadr  (nth b pinRectList))
                  rect2 = (caddr (nth b pinRectList))
                  rect2 = (BBoxCanonicalize rect2)
                  overlap = nil
                  (cond ((mod layerNum 2)==0
                         a0=(car  (car  rect1))
                         a1=(car  (cadr rect1))
                         b0=(car  (car  rect2))
                         b1=(car  (cadr rect2))
                         y0=(cadr (car  rect1))
                         y1=(cadr (cadr rect1))
                         (cond ((round (a1-b0)/MfgGrid)<0 && (round (b0-a1-space)/MfgGrid)<=0
                                overlap=(list a1:y0 b0:y1))
                               ((round (b1-a0)/MfgGrid)<0 && (round (a0-b1-space)/MfgGrid)<=0
                                overlap=(list b1:y0 a0:y1))
                               )
                         )
                        (t
                         a0=(cadr (car  rect1))
                         a1=(cadr (cadr rect1))
                         b0=(cadr (car  rect2))
                         b1=(cadr (cadr rect2))
                         x0=(car  (car  rect1))
                         x1=(car  (cadr rect1))
                         (cond ((round (a1-b0)/MfgGrid)<0 && (round (b0-a1-space)/MfgGrid)<=0
                                overlap=(list x0:a1 x1:b0))
                               ((round (b1-a0)/MfgGrid)<0 && (round (a0-b1-space)/MfgGrid)<=0
                                overlap=(list x0:b1 x1:a0))
                               )
                         )
                        )
                  (when overlap && !(BBoxAnd rect1 rect2)
                        && net1!=nil && net2!=nil && (net1!=net2 || inst1!=inst2)
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
          (when overlap && (net1==nil || (net1!=net2 || inst1!=inst2))
            xy = (BBoxGetCenter overlap)
            wb = (if offGrid (GetWireWidth v rect1) (GetTrackWidth v   xy))
            (unless wb (error "no track width for %d %g:%g\n" v (car xy) (cadr xy)))
            wt = (if offGrid (GetWireWidth v+1 rect2) (GetTrackWidth v+1 xy))
            (unless wt (error "no track width for %d %g:%g\n" v+1 (car xy) (cadr xy)))
            wb = (round wb/MfgGrid)
            wt = (round wt/MfgGrid)
            (when (dbGetOverlaps CV overlap (list Via[v] "drawing") 0:31)==nil
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
