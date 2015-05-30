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
                                (topMetal TopMetal))
 (let (abstractCV name net fig term pinlayer)

  ; load wires.il and set topMetal
  (LoadWires ?CV CV)
  (unless topMetal topMetal = bundled_top_layer)

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

  ; invert keepout
  (for m 0 topMetal (AbstractInvertKeepout MetalKeepout[m] CV abstractCV))

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
      (dbCreatePin net fig)
      (for m 0 topMetal
           (when shape->lpp==MetalDrawing[m] || shape->lpp==MetalPin[m] pinlayer[m]=t)
           )
      )
    )
  (DeleteLabels ?CV abstractCV)

  ; chop keepouts around pins
  (when cutaroundpins
    (for m 0 topMetal
         (when pinlayer[m]
           (AbstractCutAroundPins abstractCV MetalDrawing[m] MetalKeepout[m] ?otherpinlpp MetalPin[m])
           )
         )
    )

  ; save and exit
  (CopyKeepoutToBlockage abstractCV )
  (SetLefProperties abstractCV)
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
  (let (abstractCV name net fig term pin prb dd_CV layer purpose)

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

    ; copy explicit metal shapes to KeepoutPurpose
    (foreach shape CV->shapes
             layer   = (car  shape->lpp)
             purpose = (cadr shape->lpp)
             (when !shape->pin && shape->objType!="label" && (isMetal layer) &&
                   (purpose=="drawing" || purpose=="pin" || purpose=="vdd" || purpose=="gnd")
                   fig = (dbCopyFig shape abstractCV)
                   fig->lpp = (list layer KeepoutPurpose)
                   )
             )

    ; chop keepout overlapping pins
    (for m 0 2 (AbstractCutAroundPins abstractCV MetalDrawing[m] MetalKeepout[m] ?cutout 0:0))

    ; create final boundary shapes and blockage objects
    (leMergeShapes abstractCV->shapes)
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

; turn blockage from layers to objects
(defun CopyKeepoutToBlockage (cv)
  (let (points layer)
    (foreach shape cv->shapes
      (for m 0 TopMetal
           layer = MetalKeepout[m]
           (when shape->lpp==layer
             points = (cond (shape->objType=="path" (dbConvertPathToPolygon shape)->points)
                            (shape->objType=="rect" (RectGetPolygonPoints shape->bBox))
                            (t shape->points)
                            )
             (when points (dbCreateLayerBlockage cv (car layer) "routing" points))
             )
           )
      )
    )
  t
  )

; set properties for LEF export
(defun SetLefProperties (CV @key (powergrid nil) (leaf nil))
  (let (Vdd GND)
    (cond (powergrid CV->cellType="cover")
          (leaf      CV->cellType="core")
          (t         CV->cellType="core") ; TODO: "block" for large mid-level cells?
          )
    (dbSetCellViewSymmetry CV "any") ; SYMMETRY X Y R90
    GND = (dbFindNetByName CV GNDNetName)
    Vdd = (dbFindNetByName CV VddNetName)
    (when GND GND->sigType = "ground") ; USE GROUND
    (when Vdd Vdd->sigType = "supply") ; USE POWER
    )
  )

; Chop holes in flooded keepout around pins
(defun AbstractCutAroundPins (cv pinlpp keepoutlpp
                                 @key (otherpinlpp nil) (cutout nil))
  (let (c)
    (unless cutout cutout = MetalSpace[(car pinlpp)])
    c = (min (car cutout) (cadr cutout)) ; BUG: can't size differently in X and Y dimensions?
    (leLayerSize cv pinlpp c Scratch1LPP)
    (when otherpinlpp (leLayerSize cv otherpinlpp c Scratch1LPP))
    (leLayerAndNot cv keepoutlpp Scratch1LPP Scratch2LPP)
    (foreach shape cv->shapes
      (cond (shape->lpp==keepoutlpp  (dbDeleteObject shape))
            (shape->lpp==Scratch1LPP (dbDeleteObject shape))
            (shape->lpp==Scratch2LPP shape->lpp=keepoutlpp))
      )
    )
  t
  )

; Floodfill keepout: KEEPOUT = (PRB & !KEEPOUT)-cutout/2
(defun AbstractInvertKeepout (lpp cv1 cv2
                                  @key (cutout nil))
  (let (c)
    (unless cutout cutout = MetalSpace[(car lpp)])
    c = (min (car cutout) (cadr cutout)) ; BUG: can't size differently in X and Y dimensions?
    (foreach shape cv1->shapes
             (when shape->lpp==lpp (dbCopyFig shape cv2)->lpp=Scratch1LPP)
             )
    (leLayerSize cv2 Scratch1LPP c Scratch1LPP)
    (leLayerSize cv2 BoundaryLPP -c/2 Scratch2LPP)
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
