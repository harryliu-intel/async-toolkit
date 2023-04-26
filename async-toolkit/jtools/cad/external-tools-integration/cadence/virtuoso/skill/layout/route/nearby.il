; Modernized rewrite of nrCreateNeighborNetConnector
; plus functionality to strap intersecting pins with vias

; route nearby nets with short paths or vias only
(defun RouteNearby (@key (CV (geGetEditCellView)) (nets nil) (topShapes t) (subShapes t) (offGrid t) (verbose nil)
                         (protect nil) (distance nil))
  (let (n c pct protect_map)
    n=0
    protect_map=(makeTable "protect_map" nil)
    (unless nets nets=(setof a CV->nets (member a->name PowerGroundNets)==nil))
    (foreach net nets
             n+=(RouteNearbyNet net ?CV CV ?topShapes topShapes ?subShapes subShapes
                                ?offGrid offGrid ?verbose verbose ?protect_map protect_map
                                ?distance distance)
             )
    (when protect (RouteNearbyProtect protect_map ?CV CV))
    (printf "Note that RouteNearby only connects abutting pins between locked instances.\n")
    n
    )
  )

; connect subcells pins of a net with short paths or vias
(defun RouteNearbyNet (net @key (CV (geGetEditCellView)) (topShapes t) (subShapes t) (offGrid t) (verbose nil)
                           (protect_map nil) (distance nil))
  (let (layerNames tech n MasterRectList layerName pinRectList layerNum space len
             e1 e2 inst1 inst2 net1 net2 rect1 rect2 overlap gap rect rects horz
             r1x0 r1x1 r1y0 r1y1 r2x0 r2x1 r2y0 r2y1
             x0 y0 x1 y1 x y key pitch
             viaDef via list1 list2 xy wb wt viaName viaDef via)
    tech = (techGetTechFile CV)
    n=0
    (for m 0 TopMetal layerNames = (cons Metal[m] layerNames))
    MasterRectList = (nrFindPinShapesWithNet net layerNames ?topShapes topShapes ?subShapes subShapes)

    ; add paths between subcell pins that align at e2e spacing
    (foreach pinRectList MasterRectList
      layerName=(car pinRectList)
      pinRectList=(cadr pinRectList)
      layerNum = MetalNum[layerName]
      (unless MetalHaloBoundarySpace[layerName]
        (error "No MetalHaloBoundarySpace defined for layer %s\n" layerName)
        )
      horz = (layerNum&1)==0
      list1=pinRectList
      space=2*MetalHaloBoundarySpace[layerName]
      (cond (distance &&  horz space+=(car  distance))
            (distance && !horz space+=(cadr distance))
            )
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
          (when (inst1==nil || inst2==nil || net1!=net2 || inst1!=inst2) && !(BBoxAnd rect1 rect2)
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
                          overlap=(list r1x0:r1y0 r2x1:r1y1)
                          gap=    (list r1x1:r1y0 r2x0:r1y1)
                          )
                         ((round (r2x1-r1x0)/MfgGrid)<0 && (round (r1x0-r2x1-space)/MfgGrid)<=0
                          overlap=(list r2x0:r1y0 r1x1:r1y1)
                          gap=    (list r2x1:r1y0 r1x0:r1y1)
                          )
                         )
                   )
                  (!horz && (round (r1x0-r2x0)/MfgGrid)==0 && (round (r1x1-r2x1)/MfgGrid)==0 ; vertical aligned
                   (cond ((round (r1y1-r2y0)/MfgGrid)<0 && (round (r2y0-r1y1-space)/MfgGrid)<=0
                          overlap=(list r1x0:r1y0 r1x1:r2y1)
                          gap=    (list r1x0:r1y1 r1x1:r2y0)
                          )
                         ((round (r2y1-r1y0)/MfgGrid)<0 && (round (r1y0-r2y1-space)/MfgGrid)<=0
                          overlap=(list r1x0:r2y0 r1x1:r1y1)
                          gap=    (list r1x0:r2y1 r1x1:r1y0)
                          )
                         )
                   )
                  )

            ; make sure not to overlap other shapes
            (when overlap
              rect=(if horz (BBoxExpandHorizontal gap -MfgGrid) (BBoxExpandVertical gap -MfgGrid))
              (when ((dbGetOverlaps CV rect (list layerName "drawing") 0:31)!=nil ||
                     (dbGetOverlaps CV rect (list layerName "fill")    0:31)!=nil)
                overlap=nil
                )
              )

            ; draw shape to connect nearby pins
            (when overlap
              rect = (DrawPathFromRect CV layerNum overlap)
              rect->routeStatus="locked"
              (dbAddFigToNet rect net)
              (dbReplaceProp rect "autogen" "string" "RouteNearby")
              rects = (cons rect rects)
              (when protect_map
                x0=(car  (car  gap))
                y0=(cadr (car  gap))
                x1=(car  (cadr gap))
                y1=(cadr (cadr gap))
                x=(x0+x1)/2
                y=(y0+y1)/2
                pitch=pitch_m[layerNum]/2 ; HACK: assumes half-pitch is symmetric
                (cond (horz
                       y=(floor y/pitch)*pitch
                       key=(list layerNum (round x0/MfgGrid) (round y/MfgGrid) (round x1/MfgGrid) (round y/MfgGrid))
                       )
                      (t
                       x=(floor x/pitch)*pitch
                       key=(list layerNum (round x/MfgGrid) (round y0/MfgGrid) (round x/MfgGrid) (round y1/MfgGrid))
                       )
                      )
                protect_map[key]=t
                )
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
          (when overlap && (inst1==nil || inst2==nil || net1!=net2 || inst1!=inst2) &&
                (dbGetOverlaps CV overlap (list Via[v] "drawing") 0:31)==nil
            xy = (BBoxGetCenter overlap)
            wb = (if offGrid (GetWireWidth v rect1) (GetTrackWidth v xy))
            (unless wb (error "no track width for %d %g:%g\n" v (car xy) (cadr xy)))
            wt = (if offGrid (GetWireWidth v+1 rect2) (GetTrackWidth v+1 xy))
            (unless wt (error "no track width for %d %g:%g\n" v+1 (car xy) (cadr xy)))
            wb = (round wb/MfgGrid)
            wt = (round wt/MfgGrid)
            viaName = viaTable[(list v wb wt)]
            (unless viaName (error "no viaTable for %d %d %d at %f %f\n" v wb wt (car xy) (cadr xy)))
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
    (leMergeShapes rects) ; merge overlapping route nearby rectangles
    n
    )
  )

; get the width of a wire from its layer number and bBox
(defun GetWireWidth (m bbox)
  (if (m&1)==0
    (cadr (cadr bbox))-(cadr (car bbox)) ; horizontal wire
    (car  (cadr bbox))-(car  (car bbox)) ; vertical wire
    )
  )

; draw a path instead of a rectangle
(defun DrawPathFromRect (CV m rect)
  (let (x0 y0 x1 y1 w)
    x0=(car  (car  rect))
    x1=(car  (cadr rect))
    y0=(cadr (car  rect))
    y1=(cadr (cadr rect))
    (cond ((m&1)==0 ; horizontal
           w=y1-y0
           y0=(y0+y1)/2
           y1=y0
           )
          (t ; vertical
           w=x1-x0
           x0=(x0+x1)/2
           x1=x0
           )
          )
    (dbCreatePath CV (list Metal[m] "drawing") (list x0:y0 x1:y1) w)
    )
  )

; draw fill shapes to protect jumpers from ETE drc
; HACK: makes assumptions outside of the PDK parameters
(defun RouteNearbyProtect (protect_map @key (CV (geGetEditCellView)))
  (let (power pattern layers space wires key layerNum ix0 ix1 iy0 iy1 ipitch x0 y0 x1 y1 is pitch ete)
    ; pre-define fill patterns and layers
    power  =(makeTable "power"   nil)
    pattern=(makeTable "pattern" nil)
    layers =(makeTable "layers"  nil)
    space  =(makeTable "space"   nil)
    (for m 0 10
         wires=nil
         (for n 0 tracks_m[m]/2-1
              wires=(cons (list "" (NodeChan m n) 0) wires)
              )
         pattern[m]=(DefChan wires)
         power[m] = (list (list "" (cadr (car power_m[m])) (caddr (car power_m[m]))))
         layers[m]=(list (list Metal[m] "fill") (list Metal[m] "fill"))
         space[m] = (round MetalHaloBoundarySpace[Metal[m]]/MfgGrid)*2
         )

    ; draw fill shapes at all protect_map locations
    (foreach s (tableToList protect_map)
             key=(car s)
             layerNum=(car key)
             ix0=(nth 1 key)
             iy0=(nth 2 key)
             ix1=(nth 3 key)
             iy1=(nth 4 key)
             is=space[layerNum]
             x0=ix0*MfgGrid
             y0=iy0*MfgGrid
             x1=ix1*MfgGrid
             y1=iy1*MfgGrid
             pitch=pitch_m[layerNum]/2 ; HACK: assumes half-pitch is symmetric
             ipitch=(round pitch/MfgGrid)
             (cond ((layerNum&1)==0 && (abs ix1-ix0)==is
                    (DrawChannel layers[layerNum] pattern[layerNum] nil (list x0:y0 x1:y1) ?CV CV)
                    (when layerNum>=5 && layerNum<=6 && protect_map[(list layerNum ix0 iy0-ipitch ix1 iy1-ipitch)]
                          ete=(car MetalSpace[Metal[layerNum]])
                          (DrawChannel layers[layerNum] power[layerNum] nil (list x0+ete:y0 x1-ete:y1) ?CV CV)
                          )
                    )
                   ((layerNum&1)==1 && (abs iy1-iy0)==is
                    (DrawChannel layers[layerNum] pattern[layerNum] nil (list x0:y0 x1:y1) ?CV CV)
                    (when layerNum>=5 && layerNum<=6 && protect_map[(list layerNum ix0-ipitch iy0 ix1-ipitch iy1)]
                          ete=(cadr MetalSpace[Metal[layerNum]])
                          (DrawChannel layers[layerNum] power[layerNum] nil (list x0:y0+ete x1:y1-ete) ?CV CV)
                          )
                    )
                   )
             )

    ; delete fill that overlaps top-level drawing
    (foreach s CV->shapes
             (when s->purpose=="fill" && (dbGetOverlaps CV s->bBox (list s->layerName "drawing") 0:0)!=nil
                   (dbDeleteObject s)
                   )
             )
    )
  t
  )
