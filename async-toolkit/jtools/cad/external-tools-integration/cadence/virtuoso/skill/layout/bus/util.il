; Copyright 2003 Fulcrum Microsy\stems.  All rights reserved.
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/layout/bus/util.il#1 $
; $DateTime: 2009/08/13 08:18:26 $
; $Author: stevemc $


; check if two coordinates are equal to rounding precision
(defun almostEqual (x0 x1)
  (abs x1-x0)<0.001
  )

; make a net unless name is empty
(defun MakeNet (view name)
  (cond (name!="" (dbMakeNet view name))
        (name=="" nil)
        )
  )

; attach a label to a pin
(defun LabelPin (view fig @key (bBox nil))
  (let (x0 y0 x1 y1 x y rotation height)
    (unless bBox bBox = fig->bBox)
    x0 = (car  (car  bBox))
    y0 = (cadr (car  bBox))
    x1 = (car  (cadr bBox))
    y1 = (cadr (cadr bBox))
    x = (x0+x1)/2
    y = (y0+y1)/2
    x = MfgGrid * (round x/MfgGrid) 
    y = MfgGrid * (round y/MfgGrid)
    (cond (x1-x0>y1-y0 rotation = "R0"  height = y1-y0)
          (t           rotation = "R90" height = x1-x0)
          )
    height = MfgGrid * (round height/MfgGrid/3) ; adjust height
    label = (dbCreateLabel view fig->layerName x:y fig->net->name
                           "centerCenter" rotation "stick" height) ; draw the label
    label->parent = fig ; label moves with figure
    )
  t
  )

; relabel all pins
(defun LabelPins (@key (CV (geGetEditCellView)))
  (let (term pin)
    (foreach term CV->terminals
      (foreach pin term->pins
        (when pin->fig && pin->fig->objType=="rect" (LabelPin CV pin->fig))
        )
      )
    )
  t
  )

; transform xy coordinates using transform
(defun busTransformPoint (xy guideInst)
  (cond (guideInst==nil xy)
        (guideInst!=nil (geTransformUserPoint xy guideInst->transform))
        )
  )

; choose next available contact name -- faster than using nil name!
(defun NextContactName (view)
  (let (name duplicate)
    duplicate = t
    (while duplicate
      name = (sprintf nil "IC%d" busContactNum)
      duplicate = (dbFindAnyInstByName view name)
      busContactNum = (if duplicate busContactNum*2 busContactNum+1)
      )
    name
    )
  )

; get a property or default value
(defun GetProp (obj prop default)
  p = (dbGetPropByName obj prop)
  (cond ((and p!=nil p->valueType=="boolean") p->value=="TRUE")
        (p p->value)
        (t default)
        )
  )

; select wires/contacts/subcells by net name
(defun SelectNet
  (name
   @key (CV (geGetEditCellView))
        (net nil)
        (pins t)
        (wires t)
        (contacts t)
        (subcells nil)
        )
  (let (found)
    found = 0
    (unless net net = (dbFindNetByName CV name))
    (when pins
      (foreach pin net->pins
               (geSelectObject pin->fig)
               found = found+1
               )
      )
    (when wires
      (foreach fig net->figs
               (geSelectObject fig)
               found = found+1
               )
      )
    (foreach term net->instTerms
             (when (or (and contacts term->inst->libName==TechLibName)
                       (and subcells term->inst->libName!=TechLibName))
               (geSelectObject term->inst)
               found = found+1
               )
             )
    found
    )
  )

; select wires/contacts/subcells by net name with regular expression matching
(defun SelectNetRegExp 
  (regexp
   @key (CV (geGetEditCellView))
        (pins t)
        (wires t)
        (contacts t)
        (subcells nil))
  (let (found)
    found = 0
    (rexCompile regexp)
    (foreach net (setof net CV->nets (rexExecute net->name))
             found = found + 
             (SelectNet nil ?net net ?CV CV
                        ?pins pins ?wires wires ?contacts contacts ?subcells subcells)
             )
    found
    )
  )

; test for wiring shapes
(defun isWiringPath (obj)
  (let ((iswire nil))
    (when (or obj->objType=="path" obj->objType=="polygon" obj->objType=="rect") && ((isMetalDrawing obj->layerName obj->purpose) || (isMetalPin obj->layerName obj->purpose))
      iswire=t
    )
    (when obj->objType=="pin"
      (when (isMetalDrawing obj->fig->layerName obj->fig->purpose) || (isMetalPin obj->fig->layerName obj->fig->purpose)
        iswire=t
      )
    )
  iswire
  )
)

; test for wiring contacts
(defun isWiringContact (obj)
  (or (and obj->objType=="inst" obj->libName==TechLibName)
      obj->objType=="stdVia")
  )

; delete contacts only
(defun DeleteContacts (@key (CV (geGetEditCellView)))
  busContactNum = 0
  (let ()
    (foreach obj CV->instances
             (cond ((isWiringContact obj) (dbDeleteObject obj))
                   )
             )
    (foreach obj CV->vias
             (cond ((isWiringContact obj) (dbDeleteObject obj))
                   )
             )
    )
  t
  )

; delete metal paths and contacts
(defun DeleteWires (@key (CV (geGetEditCellView)))
  (let ()
    (foreach obj CV->shapes
             (cond ((isWiringPath obj) (dbDeleteObject obj))
                   )
             )
    )
  (DeleteContacts)
  t
  )

; delete metal blockages
(defun DeleteBlockages (@key (CV (geGetEditCellView)))
  (let ()
    (foreach obj CV~>blockages
      (dbDeleteObject obj)
     )
   )
  t
  )

; delete both
(defun DeleteWiresAndKeepout ()
  (DeleteWires)
  (DeleteKeepout)
  (DeleteBlockages)
  t
  )

; delete shapes and vias that have BusScriptObject property
(defun DeleteBusWires (@key (CV (geGetEditCellView)))
  (let (shapes vias blockages)
    shapes = (setof obj CV->shapes (IsBusObject obj))
    vias = (GetBusScriptVias CV)
    blockages = (setof obj CV->blockages (IsBusObject obj))
    (foreach shape shapes (dbDeleteObject shape))
    (foreach via vias (dbDeleteObject via))
    (foreach blockage blockages (dbDeleteObject blockage))
    )
  t
  )

;check for scripted wire
(defun DoesBusWireExist (net @key (CV (geGetEditCellView)))
 (let (exist)
  exist=nil
  (foreach shape CV->shapes
    (when shape->net==net && (IsBusObject shape)
      exist=t
    )
  )
  exist
 )
)

; utility to turn drawing shapes into keepout
(defun ConvertWiresToKeepout (@key (CV (geGetEditCellView)))
  (let ()
    (foreach obj CV->shapes
             (cond ((isMetalDrawing obj->layerName obj->purpose)
                    obj->purpose="boundary"
                    )
                   )
             )
    )
  t
  )

; delete the warning markers
(defun DeleteMarkers (@key (CV (geGetEditCellView)))
  (geDeleteAllMarker CV)
  )

; delete unused nets
(defun DeleteUnusedNets (@key (CV (geGetEditCellView)))
  (let (cnt)
    cnt = 0
    (foreach net CV->nets
             (when (and net->instTermCount==0 net->pins==nil net->figs==nil)
               (dbDeleteObject net)
               cnt=cnt+1
               )
             )
    cnt
    )
  )

; check if the cell has bus guides
(defun HasGuides (view)
 (let ((guides nil))
   (foreach inst view->instances
     (when (IsGuideInst inst)
       guides = t
     )
   )
   (foreach shape view->shapes
     (when (cadr shape->lpp)=="bus"
       guides = t
     )
   )
   guides
 )
)

; delete bus guides
(defun DeleteGuides (view)
  (foreach inst view->instances
    (when (IsGuideInst inst)
      (dbDeleteObject inst)
    )
  )
  (foreach shape view->shapes
    (when (cadr shape->lpp)=="bus"
      (dbDeleteObject shape)
    )
  )
)

(defun DeleteGuideShapes (view)
  (foreach shape view->shapes
    (when (cadr shape->lpp)=="bus"
      (dbDeleteObject shape)
    )
  )
)

; copy bus guides
(defun CopyGuides (srcview dstview)
      (foreach inst srcview->instances
        (when (IsGuideInst inst)
          (dbCreateInstByMasterName dstview inst->libName 
                inst->cellName inst->viewName inst->name (list 0 0) "R0")
        )
      )
      (foreach shape srcview->shapes
        (when (cadr shape->lpp)=="bus"
          (dbCopyFig shape dstview)
        )
      )
      (dbSave dstview)
)

(defun CopyGuideShapes (srcview dstview)
      (foreach shape srcview->shapes
        (when (cadr shape->lpp)=="bus"
          (dbCopyFig shape dstview)
        )
      )
      (dbSave dstview)
)


(defun RoundGuidesToGrid ( @key (paths nil) (grid TrackPitch/2) (CV (geGetEditCellView)))
 (let (haspaths newpoints)

  (if !paths then
    haspaths = nil
    (foreach shape CV->shapes
      (when shape->objType=="path" && (cadr shape->lpp)=="bus"
        haspaths = t
        paths = (cons shape paths)
      )
    )
  else
    haspaths = t
  )

  (if haspaths then
    (foreach path paths
      newpoints = (PathRoundPointsToGrid path->points ?grid grid)
      path->points = newpoints
    )
  else
    (hiDisplayAppDBox
        ?name 'RoundPathsDialogBox
        ?dboxText "No bus guidepaths found."
        ?dboxBanner "No Guides"
        ?dialogType     hicQuestionDialog
        ?dialogStyle    'modal
        ?buttonLayout   'Close
    )
  )

 )
)

(defun GetBusScriptVias (cv)
;  (setof inst cv->instances (and inst->libName==TechLibName (IsBusObject inst)))
   (setof inst cv->vias (IsBusObject inst))
  )

(defun AreViasDuplicate (via1 via2)
  (and via1->libName==via2->libName
       via1->cellName==via2->cellName
       via1->xy==via2->xy
       via1->orient==via2->orient)
  )

(defun DeleteDuplicateBusScriptVias (@key (CV (geGetEditCellView)))
 (let (busvias)
  busvias = (GetBusScriptVias CV)
  (foreach inst CV->instances
    (when inst->libName==TechLibName
      (foreach via busvias
        (when (AreViasDuplicate via inst) && !(IsInList inst busvias)
          (dbDeleteObject inst)
        )
      )
    )
  )
 )
)


(defun FindDuplicateBuswires (@key (check_points t) (CV (geGetEditCellView)))
 (let (wirescell wirescv paths duplicates already_done points_match)
  wirescell=(GetGuideInst nil busGuideInstName)
  (if wirescell then
    wirescv=(nrOpenCellViewWritable wirescell->libName wirescell->cellName wirescell->viewName ?mode "a")
  else
    wirescv=CV
  )

  (foreach shape wirescv->shapes
    (when shape->objType=="path" && (cadr shape->lpp)=="bus"
      paths=(cons shape paths)
    )
  )
  (foreach path1 paths
    already_done=(cons path1 already_done)
    (foreach path2 paths
      points_match=!check_points
      (when check_points && path1->points==path2->points
        points_match=t
      )

      (when !(IsInList path2 already_done) &&
            points_match &&
            path1->lpp==path2->lpp &&
            (GetProp path1 "BusPrefix" nil)==(GetProp path2 "BusPrefix" nil) &&
            (GetProp path1 "BusPattern" nil)==(GetProp path2 "BusPattern" nil) &&
            (GetProp path1 "BusPin" nil)==(GetProp path2 "BusPin" nil) 
        duplicates=(cons path2 duplicates)
      )
    )
  )
  duplicates
 )
)

; helper function to determine the length of all paths in a floorplan
(defun ReportPathLengths (@key (filename "alta_path_lengths.txt") 
                               (pathProp (list "Channel" "myProp"))
                               (CV (geGetEditCellView)))
  (let (shape fp channelName length points p0 p1 x0 y0 x1 y1 propName 
        shapeCount width refCoord refPoint oddList oddPoint)

    ; open the output file
    fp = (outfile filename)

    shapeCount = 0
    (if fp != nil
      then
        ; look for all paths in the current view
        (foreach shape CV->shapes
          ; process only shapes with type "path"
          shapeCount = shapeCount + 1
          (printf "%d: objType = %s - lpp = %s\n" shapeCount shape->objType (cadr shape->lpp))
          (when shape->objType=="path" 

             ; look for any of the valid properties
             channelName = nil
             properties = pathProp
             (while (and channelName == nil properties != nil) 
               propName   = (car properties)
               properties = (cdr properties)
               channelName = (GetProp shape propName nil)
               )   ; end (while ... )

             (if channelName != nil 
               then
                 ; now determine the length
                 length  = 0
                 aligned = t
                 oddList = nil
                 points  = shape->points
                 (for i 0 (length points)-2
                   p0 = (nth i   points)
                   p1 = (nth i+1 points)
                   x0 = (car  p0)
                   y0 = (cadr p0)
                   x1 = (car  p1)
                   y1 = (cadr p1)
                   length = length+(abs x1-x0)+(abs y1-y0)
                   width  = shape->width
                   ; determine the direction
                   (if x1 == x0
                      then
                        ; we're moving vertically
                        ; we'll determine the aligment
                        ; of the left edge of the path
                        refCoord = x0+width/2.0
                        (if y1 > y0 then dir=1 else dir=-1)
                        refPoint = (list refCoord y0+dir*width/2.0)
                      else
                        ; we're moving vertically
                        ; we'll determine the aligment
                        ; of the top edge of the path
                        refCoord = y0+width/2.0
                        (if x1 > x0 then dir=1 else dir=-1)
                        refPoint = (list x0+dir*width/2.0 refCoord)
                      )

                    ; is the reference coordinate aligned?
                    (when (IsAligned refCoord (2*TrackPitch)) != t
                      (if aligned == t
                        then
                          oddList = (list (list i refPoint))
                          aligned = nil
                        else
                          oddList = (append oddList (list (list i refPoint)))
                        )
                      ) 
                 )  ; end for
                 
                 ; output the result to file
                 (fprintf fp "Path=%-54s Length=%8.2f  Layer=\"%2s,%8s\" - " 
                   channelName
                   length
                   (car  shape->lpp)
                   (cadr shape->lpp)
                 )
                 (if aligned == t
                    then (fprintf fp "Path ALIGNED\n")
                    else 
                      (fprintf fp "Path NOT ALIGNED: ")
                      (foreach oddPoint oddList
                        (fprintf fp " (%4.2f:%4.2f)" 
                          (nth 0 (cadr oddPoint))
                          (nth 1 (cadr oddPoint))
                          ) ; end (fprintf ... )
                        ) ; end (foreach ... )
                      (fprintf fp "\n")
                    ) ; end (if aligned ... )
               else
                 (printf "Property not found!\n")
                 (printf "Layer         = %s, %s\n" (car shape->lpp) (cadr shape->lpp))
                 (geCreateMarker shape "error" "bus" "length" "")
             )  ; end (if channelName ... )

          ) ; end when

        ) ; end foreach

        ; close the output file
        (close fp)
      else
        (printf "Couldn't open file %s" filename)
    ) ; end if

  ) ; end let
)

; select bus paths with optional filters
(defun SelectBusPaths
  (@key (CV (geGetEditCellView))
        (prefix "")  ; require BusPrefix  to start with prefix
        (pattern "") ; require BusPattern to start with pattern
        (doPins t)   ; allow BusPin=t
        (doWires t)  ; allow BusPin=nil
        (rexPrefix nil)   ; optional regular expression for prefix
        (rexPattern nil)) ; optional regular expression for pattern
  (let (pre pat pin (n 0))
    (foreach path CV->shapes
             (when (and path->objType=="path" (cadr path->lpp)=="bus")
               pre = (GetProp path "BusPrefix" "")
               pat = (GetProp path "BusPattern" "")
               pin = (GetProp path "BusPin" nil)
               (when (and (if pin doPins doWires)
                          (strncmp prefix  pre (strlen prefix))==0
                          (strncmp pattern pat (strlen pattern))==0
                          (or !rexPrefix  (rexMatchp rexPrefix  pre))
                          (or !rexPattern (rexMatchp rexPattern pat)))
                 (geSelectObject path)
                 n = n+1
                 )
               )
             )
    n
    )
  )

; select inplace pins only
(defun SelectInplacePins
  (@key (CV (geGetEditCellView))
        (minLength 0) ; only select pins longer than this
        )
  (let (n)
    n = 0
    (foreach shape CV->shapes
             (when (and (GetProp shape "PinType" nil)=="InPlace"
                        (or (BBoxGetWidth  shape->bBox)>minLength
                            (BBoxGetHeight shape->bBox)>minLength))
               (geSelectObject shape)
               n = n+1
               )
             )
    n
    )
  )

; replace prefix of BusPrefix of bus paths
(defun ReplaceBusPrefix
  (from                           ; original   prefix
   to                             ; replacement prefix
   @key (CV (geGetEditCellView))) ; optional cell view
  (let (paths prefix (n 0))
    paths = (geGetSelSet)
    (unless paths paths=CV->shapes)
    paths = (setof shape paths shape->objType=="path")    (foreach path paths
             prefix = (GetProp path "BusPrefix" nil)
             (when (strncmp prefix from (strlen from))==0
                   prefix = (substring prefix (strlen from)+1)
                   prefix = (if prefix (strcat to prefix) to)
                   (dbReplaceProp path "BusPrefix" "string" prefix)
                   n=n+1
                   )
             )
    n
    )
  )

; Trim pins that overlap with bbox to stay inside bbox, converting the
; original shape into wires with connectivity.  Use to repair layout
; affected by BUG 18821.
(defun TrimPins (bbox @key (CV (geGetEditCellView)))
  (let (n shapes bbox_in rect)
    n=0
    shapes = (geGetSelSet)
    (unless shapes shapes=CV->shapes)
    (foreach shape shapes
             (when (and shape->pin shape->objType=="rect")
               bbox_in = (BBoxAnd bbox shape->bBox)
               (when bbox_in
                 rect = (dbCreateRect CV shape->lpp shape->bBox)
                 rect->net = shape->net
                 shape->bBox = bbox_in
                 (foreach label shape->children
                          (when label->objType=="label"
                                label->xy = (BBoxGetCenter bbox_in)
                                )
                          )
                 n=n+1
                 )
               )
             )
    n
    )
  )

; Offset a single point by specified amounts for each bBox it overlaps.
(defun StretchPointWithinBoxes
  (boxstretch xy @key (CV (geGetEditCellView)))
  (let (bbox dxy nxy)
    nxy = xy
    (foreach bs boxstretch
             bbox = (car bs)
             dxy = (cadr bs)
             (when (BBoxAnd (list xy xy) bbox)
               nxy = (car nxy)+(car dxy):(cadr nxy)+(cadr dxy)
               )
             )
    nxy
    )
  )

; Applies offset to all points within each bbox.  Bbox overlaps are
; tested before offsets are applied.  Takes a list of bbox, offset
; pairs (list (list bbox offet) ...).
(defun StretchShapesWithinBoxes
  (boxstretch @key (CV (geGetEditCellView)))
  (let (points)
    (foreach inst CV->instances
             inst->xy = (StretchPointWithinBoxes boxstretch inst->xy)
             )
    (foreach shape CV->shapes
      (cond (shape->objType=="rect"
             shape->bBox = (list (StretchPointWithinBoxes boxstretch (car  shape->bBox))
                                 (StretchPointWithinBoxes boxstretch (cadr shape->bBox)))
             )
            (t
             points = nil
             (foreach xy shape->points
               points = (append points
                                (list (StretchPointWithinBoxes boxstretch xy)))
               )
             shape->points = points
             )
            )
      )
    )
  t
  )
