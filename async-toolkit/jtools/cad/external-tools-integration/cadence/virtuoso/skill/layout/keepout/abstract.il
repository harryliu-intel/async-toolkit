; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MakeSimpleAbstract ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun MakeSimpleAbstract (@key (CV (geGetEditCellView))
                                (cutaroundpins nil)
                                (topMetal TopMetal)
                                (powerGrid nil))
 (let (abstractCV name net fig term pinlayer)

  ; load wires.il and set topMetal
  (LoadWires)
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

  ;copy pins to abstract
  pinlayer = (makeTable "pinlayer" nil)
  (foreach shape CV->shapes
    (when shape->pin
      name=shape->net->name
      net=(dbFindNetByName abstractCV name)
      (when !net net=(dbCreateNet abstractCV name))
      term=(dbFindTermByName abstractCV name)
      (when !term (dbCreateTerm net name "inputOutput"))
      fig=(dbCopyFig shape abstractCV)
      (dbCreatePin net fig)
      (for m 0 topMetal
           (when shape->lpp==MetalDrawing[m] || shape->lpp==MetalPin[m] pinlayer[m]=t)
           )
      )
    )
  (PinUtilOrientLabels abstractCV)

  ; chop keepouts around pins
  (when cutaroundpins
    (for m 0 topMetal
         (when pinlayer[m]
           (AbstractCutAroundPins abstractCV MetalDrawing[m] MetalKeepout[m] ?otherpinlpp MetalPin[m])
           )
         )
    )

  ; create power grid blockages (TODO: some other way; see BUG 24326) 
  (when powerGrid
    x2box = (rightEdge  prb->bBox)
    x1box = (leftEdge   prb->bBox)
    y2box = (topEdge    prb->bBox)
    y1box = (bottomEdge prb->bBox)
    numx2 = (round x2box/1.56)
    numx1 = (round x1box/1.56)
    numy2 = (round y2box/1.56)
    numy1 = (round y1box/1.56)
    (for i numx1 numx2
         (for j numy1 numy2
              (dbCreateRect abstractCV (list "M4" "boundary") (list i*1.56-0.105:j*1.56-0.105 i*1.56+0.105:j*1.56+0.105))
              (dbCreateRect abstractCV (list "M5" "boundary") (list i*1.56-0.105:j*1.56-0.105 i*1.56+0.105:j*1.56+0.105))
              (dbCreateRect abstractCV (list "M6" "boundary") (list i*1.56-0.105:j*1.56-0.105 i*1.56+0.105:j*1.56+0.105))
              (dbCreateRect abstractCV (list "M7" "boundary") (list i*1.56-0.105:j*1.56-0.105 i*1.56+0.105:j*1.56+0.105))
              (dbCreateRect abstractCV (list "M8" "boundary") (list i*1.56-0.25:j*1.56-0.31 i*1.56+0.25:j*1.56+0.31))
              )
         )
    (for k numy1 numy2
         (dbCreateRect abstractCV (list "M3" "boundary") (list numx1*1.56-0.105:k*1.56-0.1 numx2*1.56+0.105:k*1.56+0.1))
         (dbCreateRect abstractCV (list "M8" "boundary") (list numx1*1.56-0.31:k*1.56-0.4 numx2*1.56+0.31:k*1.56+0.4))
         )
    )

  (CopyKeepoutToBlockage abstractCV )
  (SetLefProperties abstractCV)
  (dbSave abstractCV)
  abstractCV
  )
 )

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

; KEEPOUT = (PRB & !KEEPOUT)-cutout/2
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MakeLeafAbstract ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun MakeLeafAbstract (@key (CV (geGetEditCellView))
                              (floodfillM3Keepout nil)
                              (floodfillM2Keepout t)
                              (floodfillM1Keepout t)
                              (extendM3Keepout t)
                              (extendM2Keepout t))
  (let (abstractCV name net fig term pin prb prbSpacing
        llPrB urPrB xy0 xy1 x0 y0 x1 y1
        trackshape Vdd GND dd_CV layer purpose)

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
    CV=(dbCopyCellView CV CV->libName CV->cellName "abstract_temp" nil nil t)
    (when !CV (error "Couldn't edit abstract_temp view for %s\n" CV->cellName))
    (foreach inst CV->instances (dbFlattenInst inst 32 t))
    (foreach inst CV->vias      (dbFlattenInst inst 32 t))
    (dbSave CV)

    ; create prBoundary
    (dbCreatePRBoundary abstractCV (GetPrboundPoints CV))
    (dbCreatePolygon abstractCV BoundaryLPP (GetPrboundPoints CV))
    llPrB=(car prb->bBox)
    urPrB=(cadr prb->bBox)

    ; flood fill keepout
    (when floodfillM1Keepout (AbstractInvertKeepout MetalKeepout[1] CV abstractCV) )
    (when floodfillM2Keepout (AbstractInvertKeepout MetalKeepout[2] CV abstractCV) )
    (when floodfillM3Keepout (AbstractInvertKeepout MetalKeepout[3] CV abstractCV) )

    ; extend M3 keepout to left and right sides of cell
    prbSpacing = KeepoutM2to7WireSpacing/2
    (when !floodfillM3Keepout && extendM3Keepout
          (foreach shape CV->shapes
                   (when shape->lpp==MetalDrawing[3] && !shape->pin && shape->objType!="label"
                         xy0=(car  shape->bBox)
                         xy1=(cadr shape->bBox)
                         y0=(cadr xy0)
                         y1=(cadr xy1)
                         x0=(car llPrB)+prbSpacing
                         x1=(car urPrB)-prbSpacing
                         trackshape=(list x0:y0 x1:y1)
                         (dbCreateRect abstractCV MetalKeepout[3] trackshape)
                         )
                   )
          )

    ; extend M2 keepout to top and bottom sides of cell
    (when !floodfillM2Keepout && extendM2Keepout
          (foreach shape CV->shapes
                   (when shape->lpp==MetalDrawing[2] && !shape->pin && shape->objType!="label"
                         xy0=(car  shape->bBox)
                         xy1=(cadr shape->bBox)
                         y0=(cadr xy0)
                         y1=(cadr xy1)
                         x0=(car llPrB)+prbSpacing
                         x1=(car urPrB)-prbSpacing
                         trackshape=(list x0:y0 x1:y1)
                         (dbCreateRect abstractCV MetalKeepout[2] trackshape)
                         )
                   )
          )

    ; convert Vdd/GND purposes to pins
    GND = (dbCreateNet abstractCV GNDNetName)
    Vdd = (dbCreateNet abstractCV VddNetName)
    (dbCreateTerm GND GND->name "inputOutput")
    (dbCreateTerm Vdd Vdd->name "inputOutput")
    (foreach shape CV->shapes
             (unless shape->pin
               (cond (shape->purpose=="gnd"
                      fig = (dbCopyFig shape abstractCV)
                      fig->lpp = (list (car fig->lpp) "drawing")
                      (dbCreatePin GND (dbCopyFig shape abstractCV))
                      )
                     (shape->purpose=="vdd"
                      fig = (dbCopyFig shape abstractCV)
                      fig->lpp = (list (car fig->lpp) "drawing")
                      (dbCreatePin Vdd (dbCopyFig shape abstractCV))
                      )
                     )
               )
             )

    ; copy pins on metal layers
    (foreach shape CV->shapes
             (when shape->pin && (isMetal (car shape->lpp))
               name=shape->net->name
               net=(dbFindNetByName abstractCV name)
               (unless net net=(dbCreateNet abstractCV name))
               term=(dbFindTermByName abstractCV name)
               (unless term term=(dbCreateTerm net name "inputOutput"))
               pin=(car term->pins)
               (unless pin pin=(dbCreatePin net nil))
               fig=(dbCopyFig shape abstractCV)
               fig->lpp = (list (car fig->lpp) "drawing")
               (dbAddFigToPin pin fig) ; strongly connected figures
               )
             )
    (DeleteLabels ?CV abstractCV)
    (LabelPins ?CV abstractCV)

    ; chop flooded/extended keepout near pins
    (AbstractCutAroundPins abstractCV MetalDrawing[2] MetalKeepout[2])
    (AbstractCutAroundPins abstractCV MetalDrawing[3] MetalKeepout[3])

    ; copy explicit metal shapes to boundary purpose
    (foreach shape CV->shapes
             layer   = (car  shape->lpp)
             purpose = (cadr shape->lpp)
             (when !shape->pin && shape->objType!="label" && (isMetal layer) &&
                   (purpose=="drawing" || purpose=="pin" || purpose=="vdd" || purpose=="gnd")
                   fig = (dbCopyFig shape abstractCV)
                   fig->lpp = (list layer "boundary")
                   )
             )

    ; chop keepout overlapping pins
    (AbstractCutAroundPins abstractCV MetalDrawing[2] MetalKeepout[2] ?cutout 0:0)
    (AbstractCutAroundPins abstractCV MetalDrawing[3] MetalKeepout[3] ?cutout 0:0)

    ; create final boundary shapes and blockage objects
    (leMergeShapes abstractCV->shapes)
    (CopyKeepoutToBlockage abstractCV)

    ; save and exit
    (SetLefProperties abstractCV)
    (dbSave abstractCV)
    dd_CV = (ddGetObj abstractCV->libName abstractCV->cellName "abstract_temp")
    (when dd_CV (ddDeleteObj dd_CV))
    abstractCV
    )
  )

; turn blockage from layers to objects
(defun CopyKeepoutToBlockage (cv)
  (let (points)
    (foreach shape cv->shapes
      (foreach layer (list MetalKeepout[1] MetalKeepout[2] MetalKeepout[3] MetalKeepout[4]
                           MetalKeepout[5] MetalKeepout[6] MetalKeepout[7] MetalKeepout[8])
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

(defun MakeMidAbstract ( @key (CV (geGetEditCellView))
                              (cutaroundpins t)
                              (topMetal nil))
  (MakeSimpleAbstract ?CV CV ?cutaroundpins cutaroundpins
                      ?topMetal topMetal
                      ?powerGrid t) ; TODO: BUG 24326
  )

; choose which type of abstract to make
(defun MakeAbstract ( @key (CV (geGetEditCellView))
                           (cutaroundpins t)
                           (topMetal nil))
  (if (IsLeafCell CV->cellName)
    (MakeLeafAbstract ?CV CV)
    (MakeMidAbstract ?CV CV ?cutaroundpins cutaroundpins ?topMetal topMetal)
    )
  )

; set properties for LEF export
(defun SetLefProperties (CV @key (powergrid nil))
  (let (Vdd GND)
    CV->cellType = (if powergrid "cover" "block") ; CLASS [COVER|BLOCK]
    (dbSetCellViewSymmetry CV "any") ; SYMMETRY X Y R90
    GND = (dbFindNetByName CV GNDNetName)
    Vdd = (dbFindNetByName CV VddNetName)
    (when GND GND->sigType = "ground") ; USE GROUND
    (when Vdd Vdd->sigType = "supply") ; USE POWER
    )
  )
