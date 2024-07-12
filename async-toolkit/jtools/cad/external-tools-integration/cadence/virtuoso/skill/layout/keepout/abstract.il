; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; choose automatically which type of abstract to make
(defun MakeAbstract (@key (CV (geGetEditCellView))
                          (topMetal nil))
  (when (IsLeafCell CV->cellName)
    (MakeSimpleAbstract ?CV CV ?topMetal -1 ?leaf t)
    (MakeSimpleAbstract ?CV CV ?cutaroundpins cutaroundpins ?topMetal topMetal)
    )
  )

; compatibility wrappers for UI menu
(defun MakeMidAbstract  (@key (CV (geGetEditCellView))) (MakeSimpleAbstract ?CV CV))
(defun MakeLeafAbstract (@key (CV (geGetEditCellView))) (MakeSimpleAbstract ?CV CV ?topMetal -1 ?leaf t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MakeSimpleAbstract ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun MakeSimpleAbstract (@key (CV (geGetEditCellView))
                                (topMetal nil)
                                (cutaroundpins t)
                                (convertpinvias t)
                                (leaf nil)
                                (flood nil) ; optional table of layerNames to flood-fill
                                )
  (let (abstractCV inst m name net term fig allpinfigs pinlayer map inLayerMap outLayerMap)

    ; create empty abstract view
    (printf "Generating abstract view of %s\n" CV->cellName)
    abstractCV=(dbOpenCellViewByType CV->libName CV->cellName "abstract" "maskLayout" "w")
    (unless abstractCV (error "Couldn't create abstract view of %s\n" CV->cellName))

    ; pick which layers to flood-fill with blockage
    (unless flood
      (unless topMetal topMetal = (dbGetPropByName CV "topMetal")->value)
      (unless topMetal (LoadWires ?CV CV) topMetal = bundled_top_layer)
      flood=(makeTable "flood" nil)
      (for m 0 topMetal   flood[Metal[m]]=t)
      (for m 0 topMetal-2 flood[Via[m]]=t) ; keep detailed vias on layer below topMetal
      )

    ; create prBoundary object and polygon for layer processing
    (dbCreatePRBoundary abstractCV (GetPrboundPoints CV))
    (dbCreatePolygon abstractCV BoundaryLPP (GetPrboundPoints CV))

    ; instantiate layout view so we can flatten its shapes
    inst = (dbCreateInst abstractCV CV "inst" 0:0 "R0")

    ; copy pins to abstract view and make them "pin" purpose (except keep "ipc" purpose)
    pinlayer = (makeTable "pinlayer" nil)
    (foreach shape CV->shapes
             m=MetalNum[shape->layerName]
             (when shape->pin && m
               name=shape->net->name
               net=(dbFindNetByName abstractCV name)
               (unless net net=(dbCreateNet abstractCV name))
               term=(dbFindTermByName abstractCV name)
               (unless term term=(dbCreateTerm net name shape->pin->term->direction))
               fig=(dbCopyFig shape abstractCV)
               (unless fig->purpose=="ipc" fig->lpp=(list (car fig->lpp) "pin"))
               (dbCreatePin net fig)
               allpinfigs = (cons fig allpinfigs)
               pinlayer[m]=t
               )
             )
    (DeleteLabels ?CV abstractCV)

    ; flood-fill blockage on selected layers
    (foreach e (tableToList flood)
             (AbstractInvertKeepout (list (car e) "blockage") CV abstractCV)
             )

    ; chop flood-filled blockage around pins with default spacing by layer
    (when cutaroundpins
      (for m 0 TopMetal
           (when pinlayer[m]
             (AbstractCutAroundPins abstractCV MetalPin[m] (list Metal[m] "blockage"))
             )
           )
      )

    ; create flattened blockage shapes for all vias and metal using ICV layer processor
    (for m 0 TopMetal-1
         name = Via[m]
         (when name
           map = (list name)
           (foreach purpose (list "drawing" "fill" "blockage")
                    map = (append map (list (list Via[m] purpose)))
                    )
           inLayerMap = (cons map inLayerMap)
           outLayerMap = (cons (list name (list Via[m] "blockage")) outLayerMap)
           map = (list (sprintf nil "flood_%s" Via[m]) (list Via[m] "blockage"))
           inLayerMap = (cons map inLayerMap)
           )
         )
    (for m 0 TopMetal
         name = Metal[m]
         (when name
           map = (list name)
           (foreach purpose (list "drawing" "fill" "blockage")
                    map = (append map (list (list Metal[m] purpose)))
                    (when TechLibName=="731" && (m==0 || m>=2 && m<=5) ; complement layers too
                          map = (append map (list (list (sprintf nil "mc%d" m) purpose)))
                          )
                    )
           inLayerMap = (cons map inLayerMap)
           outLayerMap = (cons (list name (list Metal[m] "blockage")) outLayerMap)
           map = (list (sprintf nil "flood_%s" Metal[m]) (list Metal[m] "blockage"))
           inLayerMap = (cons map inLayerMap)
           )
         )
    (ICVLayerProcessorSimple abstractCV abstractCV "MakeAbstract" inLayerMap outLayerMap)

    ; delete the layout inst
    (dbDeleteObject inst)

    ; chop blockage overlapping pins (without cutout spacing this time)
    (when cutaroundpins
      (for m 0 TopMetal
           (when pinlayer[m]
             (AbstractCutAroundPins abstractCV MetalPin[m] (list Metal[m] "blockage") ?cutout 0:0)
             )
           )
      )

    ; convert vias connected to pins into pins on the same net
    (when convertpinvias
      (foreach pin allpinfigs
               (AttachViasToPin abstractCV pin flood)
               )
      )

    ; block M1 on edges if M2 is flood-filled to avoid routing unreachable M1
    (when TechLibName=="731" && flood[Metal[2]] (BlockEdgeM1 ?CV abstractCV))

    ; convert layers and set LEF properties
    (ConvertKeepoutToBlockage abstractCV)
    (SetLefProperties abstractCV CV ?leaf leaf)

    ; save and return
    (dbSave abstractCV)
    abstractCV
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; convert MetalKeepout and ViaKeepout to blockage objects
(defun ConvertKeepoutToBlockage (CV)
  (let (shapes)
    (foreach shape CV->shapes
       (when shape->purpose==KeepoutPurpose || shape->purpose=="blockage"
             shapes = (dbLayerTile CV shape->lpp (list shape)) ; convert to rectangles
             (foreach shape shapes
                      (dbCreateLayerBlockage CV shape->layerName "routing"
                                             (RectGetPolygonPoints shape->bBox))
                      (dbCreateLayerBlockage CV shape->layerName "fill"
                                             (RectGetPolygonPoints shape->bBox))
                      )
             )
       )
    (foreach shape CV->shapes
             (when shape->purpose==KeepoutPurpose || shape->purpose=="blockage" (dbDeleteObject shape))
             )
    )
  t
  )

; set properties for LEF export
(defun SetLefProperties (CV srcCV @key (powergrid nil) (leaf nil))
  (let (net v)
    (cond (powergrid CV->cellType="cover")
          (leaf      CV->cellType="core")
          (t         CV->cellType="block")
          )
    (dbSetCellViewSymmetry CV "any") ; SYMMETRY X Y R90
    v = (caddr (GetLibHeightVariant ?CV srcCV))
    (cond (v=="1" || v=="2" || v=="3" CV->sitePattern = (list (list "core"   0:0 "R0")))
          (v=="0" || v=="4" || v=="5" CV->sitePattern = (list (list "corev2" 0:0 "R0")))
          (v=="h"                     CV->sitePattern = (list (list "halo"   0:0 "R0")))
          (t                          CV->sitePattern = nil)
          )
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
(defun AbstractCutAroundPins (CV pinlpp keepoutlpp @key (cutout nil))
  (unless cutout cutout = MetalSpace[(car pinlpp)])
  (unless cutout (error "No MetalSpace for %s\n" (car pinlpp)))
  (betterLayerSize CV pinlpp cutout Scratch1LPP)
  (leLayerAndNot CV keepoutlpp Scratch1LPP Scratch2LPP)
  (foreach shape CV->shapes
           (cond (shape->lpp==keepoutlpp  (dbDeleteObject shape))
                 (shape->lpp==Scratch1LPP (dbDeleteObject shape))
                 (shape->lpp==Scratch2LPP shape->lpp=keepoutlpp))
           )
  t
  )

; Floodfill keepout: KEEPOUT = (shrink PRB MetalBoundarySpace) & !(grow KEEPOUT MetalSpace)
(defun AbstractInvertKeepout (lpp CV1 CV2 @key (space nil) (shrink nil))
  (unless space  space= MetalSpace[(car lpp)])
  (unless shrink shrink=FloodfillBoundarySpace[(car lpp)])
  (unless shrink shrink=MetalBoundarySpace[(car lpp)])
  (unless shrink shrink=(list 0 0) (warn "No shrink value specified for AbstractInvertKeepout for %s\n" (car lpp)))
  (foreach shape CV1->shapes
           (when shape->lpp==lpp (dbCopyFig shape CV2)->lpp=Scratch1LPP)
           )
  (betterLayerSize CV2 Scratch1LPP space Scratch1LPP)
  (betterLayerSize CV2 BoundaryLPP -(car shrink):-(cadr shrink) Scratch2LPP)
  (leLayerAndNot CV2 Scratch2LPP Scratch1LPP lpp)
  (foreach shape CV2->shapes
           (cond (shape->lpp==Scratch1LPP (dbDeleteObject shape))
                 (shape->lpp==Scratch2LPP (dbDeleteObject shape))
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

; Find via keepout shapes that overlap a rectangular pin and attach them to the same net
(defun AttachViasToPin (CV pinfig flood)
  (let (m v)
    (when pinfig->objType=="rect"
          m=MetalNum[pinfig->layerName]
          (for v m-1 m
               (when Via[v] && !flood[Via[v]]
                     (foreach overlap (dbGetOverlaps CV pinfig->bBox (list Via[v] "blockage"))
                              overlap->lpp=(list (car overlap->lpp) "pin")
                              (dbCreatePin pinfig->net overlap)
                              )
                     )
               )
          )
    )
  t
  )

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

; Add additional M1 keepout on vertical edges of prBoundary
; to prevent ICC from causing many hard-to-fix routing errors
(defun BlockEdgeM1 (@key (CV (geGetEditCellView)))
  (let (points len w s p0 p1 x0 x1 y0 y1)
    w = (cadr (car node_m1[0]))
    s = (cadr MetalBoundarySpace[Metal[1]])
    points = CV->prBoundary->points
    len = (length points)
    (for i 0 len-1
         p0 = (nth i points)
         p1 = (nth (mod i+1 len) points)
         (when (car p0)==(car p1)
               x0 = (car p0) - w/2
               x1 = (car p0) + w/2
               y0 = (min (cadr p0) (cadr p1)) + s
               y1 = (max (cadr p0) (cadr p1)) - s
               (dbCreateRect CV MetalKeepout[1] (list x0:y0 x1:y1))
               )
         )
    )
  t
  )
