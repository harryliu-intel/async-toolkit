; compare two contacts for electrical connection
(defun ContactsConnected (obj1 obj2)
  (let (match)
    match = nil
    (foreach lpp1 obj1->master->lpps
             (foreach lpp2 obj2->master->lpps
                      (cond (lpp1->layerName==lpp2->layerName match=t))
                      )
             )
    match
    )
  )

; compare a path and a contact for electrical connection
(defun PathAndContactConnected (path inst)
  (let (match bbox x y)
    match = nil

    ; first check if contact's center overlaps path
    bbox = path->bBox
    x = (car  inst->xy)
    y = (cadr inst->xy)
    (cond ((and x>=(leftEdge bbox)   x<=(rightEdge bbox)
                y>=(bottomEdge bbox) y<=(topEdge bbox))
           ; check VIA names
           (for N 1 7
                (rexCompile (sprintf nil ".*%s%d%d.*" ViaLayerPrefix N N+1))
                (cond ((and (rexExecute inst->cellName)
                            (or (path->layerName==(sprintf nil "%s%d" MetalLayerPrefix N))
                                (path->layerName==(sprintf nil "%s%d" MetalLayerPrefix N+1))))
                       match=t
                       )
                      )
                )
           
           ; check symbolic vias
           (foreach lpp inst->master->lpps
                    (cond (lpp->layerName==path->layerName match=t))
                    )
           )
          )
    match
    )
  )

; check if two overlapping objects are electrically connected
(defun areOverlappingObjectsConnected (obj1 obj2)
  (cond ((and obj1->objType!="inst" obj2->objType!="inst")
         (obj1->layerName==obj2->layerName))
        ((and obj1->objType!="inst" obj2->objType=="inst")
         (PathAndContactConnected obj1 obj2))
        ((and obj1->objType=="inst" obj2->objType!="inst")
         (PathAndContactConnected obj2 obj1))
        ((and obj1->objType=="inst" obj2->objType=="inst")
         (ContactsConnected obj1 obj2))
        )
  )

; recursively identify connected wiring, clear BusMark as you go
(defun ConnectedWiring (view obj)
  (let (connected overlaps bbox lpps o)
    connected = nil
    (when (GetProp obj "BusMark" nil)
      (dbDeletePropByName obj "BusMark")
      connected = (list obj)
      bbox = obj->bBox
      lpps = (list obj->lpp)
      (when obj->objType=="inst"
        bbox = (list obj->xy obj->xy)
        lpps = nil
        (foreach lpp obj->master->lpps
          lpps = (cons (list lpp->layerName lpp->purpose) lpps)
          )
        )
      (foreach lpp lpps
        overlaps = (dbGetOverlaps view bbox lpp 1)
        (foreach overlap overlaps
                 o = (if (atom overlap) overlap (car overlap))
                 (when (areOverlappingObjectsConnected obj o)
                   connected = (append connected (ConnectedWiring view o))
                   )
                 )
        )
      )
    connected
    )
  )

; total length of all paths in a list of shapes
(defun TotalPathLength (shapes pinLength)
  (let (length points p0 p1 x0 y0 x1 y1)
    length = 0
    (foreach shape shapes
             (cond (shape->pin length = length + pinLength)
                   ((isWiringPath shape)
                    points = shape->points
                    (for i 0 (length points)-2
                         p0 = (nth i   points)
                         p1 = (nth i+1 points)
                         x0 = (car  p0)
                         y0 = (cadr p0)
                         x1 = (car  p1)
                         y1 = (cadr p1)
                         length = length+(abs x1-x0)+(abs y1-y0)
                         )
                    )
                   )
             )
    length
    )
  )

; Select all shapes and contacts connected to selected objects
(defun SelectConnectedWiring ()
  (let (view wiring)
    view = (geGetEditCellView)
    (SetBusMark view)
    (foreach obj (geGetSelSet)
             wiring = (ConnectedWiring view obj)
             (foreach fig wiring (geSelectObject fig))
             )
    (ClearBusMark view)
    )
  t
  )

; return list of shapes connected to argument object
(defun GetConnectedWires (wire)
  (let (view wiring wires)
    view = wire->cellView
    (SetBusMark view)
    wiring = (ConnectedWiring view wire)
    (ClearBusMark view)
    (foreach obj wiring
      (when (isWiringPath obj)
        wires = (cons obj wires)
      )
    )
    wires
  )
)

; return set of shapes connected to each input shape in a list
(defun GetConnectedWireSets (CV objects)
  (let (groups connected)
    groups = nil
    (SetBusMark CV) ; mark all nets only once
    (foreach shape objects
      connected = (ConnectedWiring CV shape)
      (when connected groups = (cons connected groups))
      )
    (ClearBusMark CV) ; clear remaining markings
    groups
    )
  )

; mark all long wires
(defun FindLongWires (length pinLength)
  (let (view connected)
    view = (geGetEditCellView)
    (geDeleteAllMarker view)
    (SetBusMark view)

    ; test length of all shapes that still have BusMark property
    (foreach shape view->shapes
             (cond ((GetProp shape "BusMark" nil)
                    connected = (ConnectedWiring view shape)
                    (cond ((TotalPathLength connected pinLength)>length
                           (MarkWires connected)))))
             )
    )
  t
  )




(defun IsWiringCell (cell)
  (let (iswiring)
   iswiring = nil
   (foreach part (parseString cell->cellName "." )
     (when part=="wires" iswiring=t )
   )
   (when cell->name=="wires" iswiring=t)
   iswiring
))


(defun IsGlobalsCell (cell)
  (let (isglobals)
   (if cell->libName=="globals" then
     isglobals=t
   else
     islgobals=nil
   )
   isglobals
))


(defun IsCustomWiringCell (cell)
  (let (iscustom)
   (if (IsWiringCell cell) && !(IsGlobalsCell cell) 
         && !(IsGuideInst cell) && !(IsPowerGridCell cell) then
     iscustom=t
   else
     iscustom=nil
   )
  iscustom
))


; return paths on a given net,
;  also return paths with no connectivity if argument is nil
(defun GetPathsForNet (net)
  (let (paths cv)
    cv=(geGetEditCellView)
    (foreach shape cv->shapes
      (when shape->objType=="path" 
         && shape->net==net 
         && (cadr shape->lpp)=="drawing"
        paths=(cons shape paths)
      )
    )
    paths
  )
)

(defun GetPathsForNetName (netname)
  (let (paths cv)
    cv=(geGetEditCellView)
    (foreach shape cv->shapes
      (when shape->objType=="path" 
         && shape->net->name==netname 
         && (cadr shape->lpp)=="drawing"
        paths=(cons shape paths)
      )
    )
    paths
  )
)

; same as above, but only if they were drawn by bus script
(defun GetBusScriptPathsForNet (net)
  (let (paths cv)
    cv=(geGetEditCellView)
    (foreach shape cv->shapes
      (when shape->objType=="path" 
         && shape->net==net 
         && (cadr shape->lpp)=="drawing"
         && (IsBusObject shape)
        paths=(cons shape paths)
      )
    )
    paths
  )
)

(defun GetBusScriptPathsForNetName (netname)
  (let (paths cv)
    cv=(geGetEditCellView)
    (foreach shape cv->shapes
      (when shape->objType=="path" 
         && shape->net->name==netname
         && (cadr shape->lpp)=="drawing"
         && (IsBusObject shape)
        paths=(cons shape paths)
      )
    )
    paths
  )
)


; collection of functions to assist in doing
; leaf cells by hand
(defun SetHandLayoutProp (obj)
  (when obj
    (dbReplaceProp obj "HandLayoutObject" "boolean" t)
  ) 
)

(defun IsHandLayoutObject (obj)
 (let (ishand)
  ishand=nil
  (when obj
    ishand=(dbGetPropByName obj "HandLayoutObject")
  )
  ishand
 )
)

(defun SetHandLayoutSelectedObjects ()
 (let (cv ss)
  cv=(geGetEditCellView)
  ss=(geGetSelectedSet cv)
  (foreach obj ss
    (SetHandLayoutProp obj)
  )
 )
)

(defun SetHandLayoutAllObjects ()
 (let (cv set)
  cv=(geGetEditCellView)
  set=cv->shapes
  (foreach obj set
    (SetHandLayoutProp obj)
  )
  set=cv->instances
  (foreach obj set
    (SetHandLayoutProp obj)
  )
  set=cv->vias
  (foreach obj set
    (SetHandLayoutProp obj)
  )
 )
)

(defun DeleteObjectsNotHandLayout ()
 (let (cv set)
  cv=(geGetEditCellView)
  set=cv->instances
  (foreach obj set
    (when !(IsHandLayoutObject obj)
        && obj->libName!="gate"
        && obj->libName!="stack"
      (dbDeleteObject obj)
    )
  )
  set=cv->shapes
  (foreach obj set
    (when !(IsHandLayoutObject obj) 
        && !obj->pin 
        && (car obj->lpp)!="prBoundary"
      (dbDeleteObject obj)
    )
  )
  set=cv->vias
  (foreach obj set
    (when !(IsHandLayoutObject obj) 
        && !obj->pin 
        && (car obj->lpp)!="prBoundary"
      (dbDeleteObject obj)
    )
  )
 )
)

