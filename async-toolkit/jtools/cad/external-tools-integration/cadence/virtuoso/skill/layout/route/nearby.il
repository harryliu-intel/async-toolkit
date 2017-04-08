; Modernized rewrite of nrCreateNeighborNetConnector
; plus functionality to strap intersecting pins with vias

; route nearby nets with short paths or vias only
(defun RouteNearby (@key (CV (geGetEditCellView)) (nets nil) (topShapes nil) (offGrid nil))
  (let (n)
    n=0
    (unless nets nets=(setof a CV->nets (member a->name PowerGroundNets)==nil))
    (foreach net nets n=n+(RouteNearbyNet net ?CV CV ?topShapes topShapes ?offGrid offGrid))
    n
    )
  )

; connect subcells pins of a net with short paths or vias
(defun RouteNearbyNet (net @key (CV (geGetEditCellView)) (topShapes nil) (offGrid nil))
  (let (lpps tech n MasterRectList layerName pinRectList layerNum space len
          net1 net2 rect1 rect2 bbox1 bbox2 overlap rect
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
             space = MetalSpace[layerName]
             (unless space (error "No MetalSpace defined for layer %s\n" layerName))
             len = (length pinRectList)
             (for a 0 len-1
               (for b a+1 len-1
                  net1 = (car  (nth a pinRectList))
                  net2 = (car  (nth b pinRectList))
                  rect1 = (cadr (nth a pinRectList))
                  rect2 = (cadr (nth b pinRectList))
                  (cond ((mod layerNum 2)==0
                         bbox1 = (BBoxExpandHorizontal rect1 (car space))
                         bbox2 = (BBoxExpandHorizontal rect2 (car space))
                         )
                        (t
                         bbox1 = (BBoxExpandVertical rect1 (cadr space))
                         bbox2 = (BBoxExpandVertical rect2 (cadr space))
                         )
                        )
                  overlap = (BBoxAnd bbox1 bbox2)
                  (when (net1==nil || net1!=net2) && overlap && !(BBoxAnd rect1 rect2)
                    rect = (dbCreateRect CV (list layerName "drawing") overlap)
                    rect->routeStatus="locked"
                    (dbAddFigToNet rect net)
                    (dbReplaceProp rect "autogen" "string" "RouteNearby")
                    (printf "%s on %s\n" layerName net->name)
                    n++
                    )
                  )
               )
             )

    ; add vias between subcell pins that overlap
    (for v 0 TopMetal-1
      list1 = (setof a MasterRectList (car a)==Metal[v])
      list2 = (setof a MasterRectList (car a)==Metal[v+1])
      list1 = (cadr (car list1))
      list2 = (cadr (car list2))
      (foreach e1 list1
        (foreach e2 list2
          net1  = (car  e1)
          rect1 = (BBoxCanonicalize (cadr e1))
          net2  = (car  e2)
          rect2 = (BBoxCanonicalize (cadr e2))
          overlap = (BBoxAnd rect1 rect2)
          (when overlap && (net1==nil || net1!=net2)
            xy = (BBoxGetCenter overlap)
            wb = (if offGrid (GetWireWidth v rect1) (GetTrackWidth v   xy))
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
            (printf "%s on %s\n" Via[v] net->name)
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
