; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; choose automatically which type of abstract to make
(defun MakeAbstract (@key (CV (geGetEditCellView))
                          (cutaroundpins t)
                          (topMetal nil))
  (if (IsLeafCell CV->cellName)
    (MakeLeafAbstract ?CV CV)
    (MakeSimpleAbstract ?CV CV ?cutaroundpins cutaroundpins ?topMetal topMetal)
    )
  )

; compatibility wrapper for UI menu
(defun MakeMidAbstract (@key (CV (geGetEditCellView)))
  (MakeSimpleAbstract ?CV CV)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MakeSimpleAbstract ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun MakeSimpleAbstract (@key (CV (geGetEditCellView))
                                (cutaroundpins t)
                                (topMetal nil)
                                )
 (let (abstractCV name net fig term pinlayer map inLayerMap outLayerMap inst)

  ; set topMetal
  (unless topMetal topMetal = (dbGetPropByName CV "topMetal")->value)
  (unless topMetal (LoadWires ?CV CV) topMetal = bundled_top_layer)

  ; create view
  abstractCV=(dbOpenCellViewByType CV->libName CV->cellName "abstract" "maskLayout" "w")
  (when !abstractCV
    (printf "Couldn't edit abstract view for %s\n" CV->cellName)
    (error "Cellview not writable.\n")
  )
  (printf "Generating abstract of %s at %s\n" CV->cellName (getCurrentTime))
  (foreach shape abstractCV->shapes
    (when shape (dbDeleteObject shape) )
    )

  ; create prBoundary
  prb = (dbCreatePRBoundary abstractCV (GetPrboundPoints CV))
  (dbCreatePolygon abstractCV BoundaryLPP (GetPrboundPoints CV))

  ; instantiate layout view so we can flatten its shapes
  inst = (dbCreateInst abstractCV CV "inst" 0:0 "R0")

  ; invert keepout (use blockage layer to avoid polygons with too many points)
  (for m 0 topMetal (AbstractInvertKeepout (list Metal[m] "blockage") CV abstractCV))
  
  ; flood via keepout (use blockage layer to avoid polygons with too many points)
  ; keep vias within a lego spacing of the prBoundary just to be safe!
  (for m -1 topMetal-2 (AbstractInvertKeepout (list Via[m] "blockage") CV abstractCV ?space 0:0))

  ; create keepout for all existing vias and metal
  (for m -1 TopMetal-1
       name = Via[m]
       map = (list name)
       (foreach purpose (list "drawing" "gnd" "vdd" "fill")
                map = (append map (list (list Via[m] purpose)))
                )
       inLayerMap = (cons map inLayerMap)
       outLayerMap = (cons (list name (list Via[m] KeepoutPurpose)) outLayerMap)
       map = (list (sprintf nil "flood_%s" Via[m]) (list Via[m] "blockage"))
       inLayerMap = (cons map inLayerMap)
       )
  (for m 0 TopMetal
       name = Metal[m]
       map = (list name)
       (foreach purpose (list "drawing" "gnd" "vdd" "fill")
                map = (append map (list (list Metal[m] purpose)))
                (when m==0 || m>=2 && m<=5 ; complement layers too
                      map = (append map (list (list (sprintf nil "mc%d" m) purpose)))
                      )
                )
       inLayerMap = (cons map inLayerMap)
       outLayerMap = (cons (list name (list Metal[m] KeepoutPurpose)) outLayerMap)
       map = (list (sprintf nil "flood_%s" Metal[m]) (list Metal[m] "blockage"))
       inLayerMap = (cons map inLayerMap)
       )
  (ICVLayerProcessorSimple abstractCV abstractCV "MakeAbstract" inLayerMap outLayerMap)

  ; convert blockages to keepout (avoid polygons with too many points)
  (foreach shape abstractCV->shapes
           (when shape->purpose=="blockage"
                 shape->lpp = (list (car shape->lpp) "keepout")
                 )
           )

  ; delete the inst
  (dbDeleteObject inst)

  ; copy pins to abstract
  pinlayer = (makeTable "pinlayer" nil)
  (foreach shape CV->shapes
    (when shape->pin
      name=shape->net->name
      net=(dbFindNetByName abstractCV name)
      (unless net net=(dbCreateNet abstractCV name))
      term=(dbFindTermByName abstractCV name)
      (unless term term=(dbCreateTerm net name shape->pin->term->direction))
      fig=(dbCopyFig shape abstractCV)
      fig->lpp = (list (car fig->lpp) "drawing")
      (dbCreatePin net fig)
      (for m 0 TopMetal
           (when shape->layerName==Metal[m] pinlayer[m]=t)
           )
      )
    )
  (DeleteLabels ?CV abstractCV)

  ; chop keepouts around pins
  (when cutaroundpins
    (for m 0 TopMetal
         (when pinlayer[m]
           (AbstractCutAroundPins abstractCV MetalDrawing[m] MetalKeepout[m])
           )
         )
    )

  ; save and exit
  (CopyKeepoutToBlockage abstractCV)
  (SetLefProperties abstractCV ?topMetal topMetal)
  (dbSave abstractCV)
  abstractCV
  )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MakeLeafAbstract ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun MakeLeafAbstract (@key (CV (geGetEditCellView))
                              (floodfillM2Keepout nil)
                              (floodfillM1Keepout nil)
                              (floodfillM0Keepout nil)
                              (extendM2Keepout nil)
                              (extendM1Keepout nil)
                              (extendM0Keepout nil)
                              )
  (let (abstractCV name net fig term pin prb dd_CV layer purpose map inLayerMap outLayerMap)

    ; sanity check
    (when CV->viewName=="abstract"
          (error "Can't call MakeAbstract on abstract view of %s\n" CV->cellName)
          )

    ; check for prBoundary first
    prb = (GetPrbound CV)
    (unless prb (error "No prBoundary object in %s\n" CV->cellName))

    ; create empty abstract view
    (printf "Generating abstract of %s at %s\n" CV->cellName (getCurrentTime))
    abstractCV=(dbOpenCellViewByType CV->libName CV->cellName "abstract" "maskLayout" "w")
    (when !abstractCV (error "Couldn't edit abstract view for %s\n" CV->cellName))

    ; flatten pcells and vias to abstract_temp view
    CV=(betterCopyCellView CV CV->libName CV->cellName "abstract_temp" nil nil t)
    (when !CV (error "Couldn't edit abstract_temp view for %s\n" CV->cellName))
    (foreach inst CV->instances (dbFlattenInst inst 32 t))
    (foreach inst CV->vias      (dbFlattenInst inst 32 t))
    (dbSave CV)

    ; create prBoundary
    (dbCreatePRBoundary abstractCV (GetPrboundPoints CV))
    (dbCreatePolygon abstractCV BoundaryLPP (GetPrboundPoints CV))

    ; flood fill 
    (when floodfillM0Keepout (AbstractInvertKeepout MetalKeepout[0] CV abstractCV) )
    (when floodfillM1Keepout (AbstractInvertKeepout MetalKeepout[1] CV abstractCV) )
    (when floodfillM2Keepout (AbstractInvertKeepout MetalKeepout[2] CV abstractCV) )

    ; extend keepout to left and right sides of cell
    (when !floodfillM2Keepout && extendM2Keepout (AbstractExtendHorizontal CV 2 prb->bBox))
    (when !floodfillM1Keepout && extendM1Keepout (AbstractExtendVertical   CV 1 prb->bBox))
    (when !floodfillM0Keepout && extendM0Keepout (AbstractExtendHorizontal CV 0 prb->bBox))

    ; copy pins on metal layers
    (foreach shape CV->shapes
             (when shape->pin && (isMetal (car shape->lpp))
               name=shape->net->name
               net=(dbFindNetByName abstractCV name)
               (unless net net=(dbCreateNet abstractCV name))
               term=(dbFindTermByName abstractCV name)
               (unless term term=(dbCreateTerm net name shape->pin->term->direction))
               pin=(car term->pins)
               (unless pin pin=(dbCreatePin net nil))
               fig=(dbCopyFig shape abstractCV)
               fig->lpp = (list (car fig->lpp) "drawing")
               (dbAddFigToPin pin fig) ; strongly connected figures
               )
             )
    (DeleteLabels ?CV abstractCV)

    ; chop flooded/extended keepout near pins
    (for m 0 2 (AbstractCutAroundPins abstractCV MetalDrawing[m] MetalKeepout[m]))

    ; create keepout for vias and metal
    map = (list "vcn")
    (foreach purpose (list "drawing" "fill")
             map = (append map (list (list "vcn" purpose)))
             )
    inLayerMap = (cons map inLayerMap)
    outLayerMap = (cons (list "vcn" (list "vcn" KeepoutPurpose)) outLayerMap)
    (for m 0 1
         name = Via[m]
         map = (list name)
         (foreach purpose (list "drawing" "gnd" "vdd" "fill")
                  map = (append map (list (list Via[m] purpose)))
                  )
         inLayerMap = (cons map inLayerMap)
         outLayerMap = (cons (list name (list Via[m] KeepoutPurpose)) outLayerMap)
         )
    (for m 0 2
         name = Metal[m]
         map = (list name)
         (foreach purpose (list "drawing" "gnd" "vdd" "fill")
                  map = (append map (list (list Metal[m] purpose)))
                  )
         inLayerMap = (cons map inLayerMap)
         outLayerMap = (cons (list name (list Metal[m] KeepoutPurpose)) outLayerMap)
         )
    (ICVLayerProcessorSimple CV abstractCV "mergeShape" inLayerMap outLayerMap)

    ; chop keepout overlapping pins
    (for m 0 2 (AbstractCutAroundPins abstractCV MetalDrawing[m] MetalKeepout[m] ?cutout 0:0))

    ; create final boundary shapes and blockage objects
    ; (leMergeShapes abstractCV->shapes)
    (CopyKeepoutToBlockage abstractCV)

    ; save and exit
    (SetLefProperties abstractCV ?leaf t)
    (dbSave abstractCV)
    dd_CV = (ddGetObj abstractCV->libName abstractCV->cellName "abstract_temp")
    (when dd_CV (ddDeleteObj dd_CV))
    abstractCV
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; copy MetalKeepout and ViaKeepout to blockage objects
(defun CopyKeepoutToBlockage (cv)
  (let (points valid)
    (foreach shape cv->shapes
      valid = (shape->lpp==(list "vcn" "keepout"))
      (for m 0 TopMetal   valid = valid || shape->lpp==MetalKeepout[m])
      (for m 0 TopMetal-1 valid = valid || shape->lpp==ViaKeepout[m])
      (when valid
        points = (cond (shape->objType=="path" (dbConvertPathToPolygon shape)->points)
                       (shape->objType=="rect" (RectGetPolygonPoints shape->bBox))
                       (t shape->points)
                       )
        (when points (dbCreateLayerBlockage cv shape->layerName "routing" points))
        )
      )
    )
  t
  )

; set properties for LEF export
(defun SetLefProperties (CV @key (powergrid nil) (leaf nil) (topMetal nil))
  (let (net)
    (cond (powergrid CV->cellType="cover")
          (leaf      CV->cellType="core")
          (topMetal && topMetal<=2 && CV->prBoundary &&
                    (topEdge CV->prBoundary->bBox)-(bottomEdge CV->prBoundary->bBox)<=2.01*GridPitch
                    CV->cellType="core"
                    ) ; treat m2 mid-level cells as core
          (t         CV->cellType="block")
          )
    (dbSetCellViewSymmetry CV "any") ; SYMMETRY X Y R90
    (foreach name PowerNets
             net = (dbFindNetByName CV name)
             (when net net->term->direction="inputOutput" net->sigType = "supply") ; USE POWER
             )
    (foreach name GroundNets
             net = (dbFindNetByName CV name)
             (when net net->term->direction="inputOutput" net->sigType = "ground") ; USE GROUND
             )
    )
  )

; Chop holes in flooded keepout around pins
(defun AbstractCutAroundPins (cv pinlpp keepoutlpp @key (cutout nil))
  (unless cutout cutout = MetalSpace[(car pinlpp)])
  (betterLayerSize cv pinlpp cutout Scratch1LPP)
  (leLayerAndNot cv keepoutlpp Scratch1LPP Scratch2LPP)
  (foreach shape cv->shapes
           (cond (shape->lpp==keepoutlpp  (dbDeleteObject shape))
                 (shape->lpp==Scratch1LPP (dbDeleteObject shape))
                 (shape->lpp==Scratch2LPP shape->lpp=keepoutlpp))
           )
  t
  )

; Floodfill keepout: KEEPOUT = (shrink PRB MetalBoundarySpace) & !(grow KEEPOUT MetalSpace)
(defun AbstractInvertKeepout (lpp cv1 cv2 @key (space nil) (shrink nil))
  (let ()
    (unless space  space= MetalSpace[(car lpp)])
    (unless shrink shrink=FloodfillBoundarySpace[(car lpp)])
    (unless shrink shrink=MetalBoundarySpace[(car lpp)])
    (foreach shape cv1->shapes
             (when shape->lpp==lpp (dbCopyFig shape cv2)->lpp=Scratch1LPP)
             )
    (betterLayerSize cv2 Scratch1LPP space Scratch1LPP)
    (betterLayerSize cv2 BoundaryLPP -(car shrink):-(cadr shrink) Scratch2LPP)
    (leLayerAndNot cv2 Scratch2LPP Scratch1LPP lpp)
    (foreach shape cv2->shapes
             (cond (shape->lpp==Scratch1LPP (dbDeleteObject shape))
                   (shape->lpp==Scratch2LPP (dbDeleteObject shape))
                   )
             )
    )
  t
  )

; Extend keepout to block entire horizontal track
(defun AbstractExtendHorizontal (CV m bbox)
  (let (xy0 xy1 x0 y0 x1 y1 prbSpacing)
    prbSpacing = (car MetalSpace[Metal[m]])/2
    (foreach shape CV->shapes
             (when shape->lpp==MetalDrawing[m] && !shape->pin && shape->objType!="label"
                   xy0=(car  shape->bBox)
                   xy1=(cadr shape->bBox)
                   y0=(cadr xy0)
                   y1=(cadr xy1)
                   x0=(car (car bbox))+prbSpacing
                   x1=(car (cadr bbox))-prbSpacing
                   (dbCreateRect abstractCV MetalKeepout[m] (list x0:y0 x1:y1))
                   )
             )
    )
  t
  )

; Extend keepout to block entire vertical track
(defun AbstractExtendVertical (CV m bbox)
  (let (xy0 xy1 x0 y0 x1 y1 prbSpacing)
    prbSpacing = (cadr MetalSpace[Metal[m]])/2
    (foreach shape CV->shapes
             (when shape->lpp==MetalDrawing[m] && !shape->pin && shape->objType!="label"
                   xy0=(car  shape->bBox)
                   xy1=(cadr shape->bBox)
                   y0=(cadr xy0)
                   y1=(cadr xy1)
                   x0=(car xy0)
                   x1=(car xy1)
                   y0=(cadr (car bbox))+prbSpacing
                   y1=(cadr (cadr bbox))-prbSpacing
                   (dbCreateRect abstractCV MetalKeepout[m] (list x0:y0 x1:y1))
                   )
             )
    )
  t
  )

;;;;;;;;;;;;;;;;;;;;;;; HACK: implement asymmetric leLayerSize ;;;;;;;;;;;;;;;;;;;;;;;

; like leLayerSize but with asymmetric x:y bloat
(defun betterLayerSize (CV lpp1 xy lpp2)
  (let (points)
    (foreach x (setof s CV->shapes s->lpp==lpp1 && s->objType!="label")
             points = (if x->objType=="polygon" x->points (RectGetPoints x->bBox))
             points = (betterGrowPoints points xy)
             (dbCreatePolygon CV lpp2 points)
             )
    t
    )
  )

; like pcGrowPoints but with asymmetric x:y bloat
; CLEVER: bloat symmetrically by x and y separately then mix coordinates back together
(defun betterGrowPoints (points xy)
  (let (xpoints ypoints newpoints)
    xpoints = (pcGrowPoints points (car  xy))
    ypoints = (pcGrowPoints points (cadr xy))
    (when (length xpoints)!=(length ypoints) (error "betterGrowPoints: (length xpoints)!=(length ypoints)"))
    (for i 0 (length xpoints)-1
         newpoints = (cons (car (nth i xpoints)):(cadr (nth i ypoints)) newpoints)
         )
    newpoints
    )
  )
