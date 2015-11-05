; Modernized rewrite of nrCreateNeighborNetConnector
; plus functionality to strap intersecting pins with vias

; route nearby nets with short paths or vias only
(defun RouteNearby (@key (CV (geGetEditCellView)) (nets nil))
  (let (n)
    n=0
    (unless nets nets=(setof a CV->nets (member a->name PowerGroundNets)==nil))
    (foreach net nets n=n+(RouteNearbyNet net ?CV CV))
    n
    )
  )

; connect subcells pins of a net with short paths or vias
(defun RouteNearbyNet (net @key (CV (geGetEditCellView)) (topShapes nil))
  (let (n MasterRectList layerName pinRectList layerNum space len
          rect1 rect2 bbox1 bbox2 overlap rect
          viaDef via list1 list2 xy wb wt viaName viaDef via)
    n=0
    MasterRectList = (nrFindPinShapes net MetalLPPs ?topShapes topShapes)

    ; add paths between subcell pins that align at e2e spacing
    (foreach pinRectList MasterRectList
             layerName=(car pinRectList)
             pinRectList=(cadr pinRectList) 
             layerNum = MetalNum[layerName]
             space = MetalSpace[layerName]
             len = (length pinRectList)
             (for a 0 len-1
               (for b a+1 len-1
                  rect1 = (nth a pinRectList)
                  rect2 = (nth b pinRectList)
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
                  (when overlap
                    rect = (dbCreateRect CV (list layerName "drawing") overlap)
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
      (foreach rect1 list1
        (foreach rect2 list2
          overlap = (BBoxAnd rect1 rect2)
          (when overlap && (dbGetOverlaps CV overlap ViaDrawing[v] 2)==nil
            xy = (BBoxGetCenter overlap)
            wb = (GetTrackWidth v   xy)
            (unless wb (error "no track width for %d %g:%g\n" v (car xy) (cadr xy)))
            wt = (GetTrackWidth v+1 xy)
            (unless wt (error "no track width for %d %g:%g\n" v+1 (car xy) (cadr xy)))
            viaName = viaTable[(list v wb wt)]
            (unless viaName (error "no viaName for %d %g %g\n" v wb wt))
            viaDef = (techFindViaDefByName (techGetTechFile CV) viaName)
            via = (dbCreateVia CV viaDef xy "R0")
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
