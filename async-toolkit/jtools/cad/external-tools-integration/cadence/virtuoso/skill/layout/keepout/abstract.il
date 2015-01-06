; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun AbstractCreateAbstractView ( CellView
                                    KeepoutRuleFile
                                    ViaLayerFormat
                                    MetalLayerFormat
                                    MetalLPPs
                                    KeepoutMetalLPPs
                                    ViaLPPs
                                    WorkingDir
                                    FoldableCellLibCellPairRegExs
                                    GNDNetName
                                    VddNetName
                                    ContactLibName
                                    ContactCellNameFormat
                                    ContactViewName
                                    BoundaryLPP
                                    @key
                                    ( Internal nil )
                                    ( ErasePowerGrid t )
                                    ( KeepoutPowerGrid t )
                                    ( DrawKeepout t )
                                    ( Simplify nil )
                                    ( DrawVias t )
                                    ( LimitPins nil )
                                    ( NoPCells nil )
                                    )
  (let (
        ( TempLibName
          ( LibCreateTempLibraryFromCellView CellView "ABSTRACT" WorkingDir ) )
        ( AssuraLayerMappings
          ( cons
            ( list "bound" BoundaryLPP )
            ( append
              ( ListNonDestructiveMapCan
                (lambda ( LPP )
                  (let (
                        ( Num 0 ) )
                    ( sscanf ( car LPP ) MetalLayerFormat Num )
                    ( list
                      ( list ( sprintf nil "m%dKeepout" Num )
                             ( list ( car LPP )
                                    "boundary" ) )
                      ( list ( sprintf nil "m%dNeedConnect" Num )
                             LPP ) ) ) )
                MetalLPPs )
              ( ListNonDestructiveMapCan
                (lambda ( LPP )
                  (let (
                        ( Num 1 )
                        )
                    ( sscanf ( car LPP ) ViaLayerFormat Num  )
                    ( list
                      ( list ( sprintf nil "via%dNeedConnect"
                                       Num
                                       )
                             LPP ) ) ) )
                ViaLPPs ) )
            ) ) )
    (let (
          ( ConductorCellView
            ( dbCopyCellView
              CellView
              TempLibName
              CellView->cellName
              "conductor"
              nil nil t ) ) )

        ( foreach
          Shape
          ( getq ConductorCellView shapes )
          (cond (
                 ( equal ( getq Shape objType ) "label" )
                 (unless ( and
                           KeepoutPowerGrid
                           ( or
                             ( equal Shape->theLabel GNDNetName )
                             ( equal Shape->theLabel VddNetName ) ) )
                   ( dbSetq Shape ( strcat "XXX" Shape->theLabel ) theLabel ) ) ) ) )

      ( dbSave ConductorCellView )

  (let (
        ( ErrorStr
          ( AssuraRunAssuraLayerProcessor
              ConductorCellView
              TempLibName
              CellView->cellName
              "keepout"
              KeepoutRuleFile
              WorkingDir
              AssuraLayerMappings
              nil
              ?NoPCells NoPCells
              ?AssuraSets
              ( append ( setof
                         Set
                         ( list (when ErasePowerGrid "ERASE_POWER_GRID" )
                                (when DrawKeepout "DRAW_KEEPOUT" )
                                (when Simplify "SIMPLIFY" )
                                (when DrawVias "DRAW_VIAS" )
                                (when LimitPins "LIMIT_PINS" )
                                )
                         ( stringp Set ) )
                       ( mapcar `car KeepoutMetalLPPs ) )
              ) ) )
    (if ErrorStr
          ErrorStr
        (let (
              ( Keepout
                ( dbOpenCellViewByType
                  TempLibName
                  CellView->cellName
                  "keepout"
                  nil
                  "r" ) )
              ( Abstract
                ( dbCopyCellView
                  CellView
                  CellView->libName
                  CellView->cellName
                  "abstract"
                  nil nil t) ) )


          ;replace shapes, but keep labels and pins
          ( foreach
            Fig
            ( getq Abstract shapes )
            (unless ( or
                      ( getq Fig pin )
                      ( equal ( getq Fig objType ) "label" ) )
              ( dbDeleteObject Fig ) )
            )

          ( foreach
            Shape
            ( getq Keepout shapes )
            ( dbCopyFig
              Shape
              Abstract
              ) )

          ( foreach
            Fig
            ( getq Abstract mosaics )
            ( dbDeleteObject Fig ) )

          ( foreach
            Fig
            ( getq Abstract instances )
            (unless Internal ( dbDeleteObject Fig )
            ) )

          ( foreach ly Abstract~>lpps
               if((ly~>layerName == "prBoundary" && ly~>purpose == "drawing") then
                    foreach( shape ly~>shapes shape~>purpose = "boundary" )
               )
          )

          sprintf( gec3Foreign "%s %f %f" Abstract~>cellName caar(Abstract~>bBox) cadar(Abstract~>bBox) )

          dbReplacePropList( Abstract list(
              list("prCellClass" "string" "block")
              list("prCellType" "string" "macro")
              list("maskLayoutSubType" "string" "abstract")
              list("symmetry" "string" "X Y")
;              list("placementClass" "string" "standard")
              list("gec3Foreign" "string" gec3Foreign)
              )
          )

          dbSave(Abstract)
          println(ViaLPPs)

          ( foreach
            LPP
            ViaLPPs
            (let (
                  ( BottomMetalLayer
                    ( car ( car ( PinUtilGetValidLayers LPP ) ) ) )
                  ( N 1 ) )
              ( sscanf BottomMetalLayer MetalLayerFormat Num )
              (let (
                    ( ContactCellName
                      ( sprintf
                        nil
                        ContactCellNameFormat
                        ( plus N 1 ) N ) ) )
                ( foreach
                  Shape
                  ( PinUtilGetAllShapesOnLPP
                    Abstract
                    LPP )

                  ( dbCreateInstByMasterName
                    Abstract
                    ContactLibName
                    ContactCellName
                    ContactViewName
                    nil
                    ( RectGetCenter
                      ( getq Shape bBox ) )
                    "R0"
                    )
                  ( dbDeleteObject Shape )
                  ) ) ) )

          ( dbPurge Keepout )
          ( dbPurge ConductorCellView )

          ( ddDeleteObj ( ddGetObj TempLibName ) )

          Abstract
          ) ) ) ) ) )

(defun AbstractCellUsingPDKInfo ( CellView
                                  WorkingDir
                                  @key
                                  ( Internal nil )
                                  ( ErasePowerGrid t )
                                  ( Simplify nil )
                                  ( DrawVias t )
                                  ( KeepoutPowerGrid t )
                                  ( LimitPins nil )
                                  ( NoPCells nil )
                                  ( KeepoutMetalLPPs
                                    ( list
                                      Metal3LPP
                                      Metal4LPP
                                      Metal5LPP
                                      Metal6LPP
                                      Metal7LPP
                                      Metal8LPP )
                                    ) )
  (let (
        ( Abstract
          ( AbstractCreateAbstractView
            CellView
            ( sprintf
              nil
              "%s/share/Fulcrum/cell_automation/abstract.rul"
              FulcrumPDKRoot )
            ViaLayerFormat
            MetalLayerFormat
            MetalLPPs
            KeepoutMetalLPPs
            ViaLPPs
            WorkingDir
            FoldableCellLibCellPairRegExs
            GNDNetName
            VddNetName
            ContactLibrary
            ContactMinCellNameFormat
            ContactViewName
            BoundaryLPP
            ?ErasePowerGrid ErasePowerGrid
            ?Internal Internal
            ?Simplify Simplify
            ?DrawVias DrawVias
            ?KeepoutPowerGrid KeepoutPowerGrid
            ?LimitPins LimitPins
            ?NoPCells NoPCells
            ) ) )
    Abstract
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MakeSimpleAbstract ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun MakeSimpleAbstract (@key (CV (geGetEditCellView))
                                (cutaroundpins nil)
                                (cutout KeepoutM2to7WireSpacing)
                                (topMetal nil)
                                (powerGrid nil))
 (let (abstractCV name net fig term
       havem2pins havem3pins havem4pins havem5pins havem6pins havem7pins)

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
  (AbstractInvertKeepout Metal1byLPP CV abstractCV ?cutout cutout)
  (when topMetal>1 (AbstractInvertKeepout Metal2byLPP CV abstractCV ?cutout cutout) )
  (when topMetal>2 (AbstractInvertKeepout Metal3byLPP CV abstractCV ?cutout cutout) )
  (when topMetal>3 (AbstractInvertKeepout Metal4byLPP CV abstractCV ?cutout cutout) )
  (when topMetal>4 (AbstractInvertKeepout Metal5byLPP CV abstractCV ?cutout cutout) )
  (when topMetal>5 (AbstractInvertKeepout Metal6byLPP CV abstractCV ?cutout cutout) )
  (when topMetal>6 (AbstractInvertKeepout Metal7byLPP CV abstractCV ?cutout cutout) )

  ;copy pins to abstract
  havem2pins=nil
  havem3pins=nil
  havem4pins=nil
  havem5pins=nil
  havem6pins=nil
  havem7pins=nil
  (foreach shape CV->shapes
    (when shape->pin
      name=shape->net->name
      net=(dbFindNetByName abstractCV name)
      (when !net net=(dbCreateNet abstractCV name))
      term=(dbFindTermByName abstractCV name)
      (when !term (dbCreateTerm net name "inputOutput"))
      fig=(dbCopyFig shape abstractCV)
      (dbCreatePin net fig)
      (when shape->lpp==Metal2LPP || shape->lpp==Metal2pinLPP
        havem2pins=t
      )
      (when shape->lpp==Metal3LPP || shape->lpp==Metal3pinLPP
        havem3pins=t
      )
      (when shape->lpp==Metal4LPP || shape->lpp==Metal4pinLPP
        havem4pins=t
      )
      (when shape->lpp==Metal5LPP || shape->lpp==Metal5pinLPP
        havem5pins=t
      )
      (when shape->lpp==Metal6LPP || shape->lpp==Metal6pinLPP
        havem6pins=t
      )
      (when shape->lpp==Metal7LPP || shape->lpp==Metal7pinLPP
        havem7pins=t
      )
    )
  )
  (PinUtilOrientLabels abstractCV)

  ;chop keepouts around pins
  (when cutaroundpins
    (when havem2pins (AbstractCutAroundPins abstractCV Metal2LPP Metal2byLPP
                                            ?otherpinlpp Metal2pinLPP ?cutout cutout))
    (when havem3pins (AbstractCutAroundPins abstractCV Metal3LPP Metal3byLPP
                                            ?otherpinlpp Metal3pinLPP ?cutout cutout))
    (when havem4pins (AbstractCutAroundPins abstractCV Metal4LPP Metal4byLPP
                                            ?otherpinlpp Metal4pinLPP ?cutout cutout))
    (when havem5pins (AbstractCutAroundPins abstractCV Metal5LPP Metal5byLPP
                                            ?otherpinlpp Metal5pinLPP ?cutout cutout))
    (when havem6pins (AbstractCutAroundPins abstractCV Metal6LPP Metal6byLPP
                                            ?otherpinlpp Metal6pinLPP ?cutout cutout))
    (when havem7pins (AbstractCutAroundPins abstractCV Metal7LPP Metal7byLPP
                                            ?otherpinlpp Metal7pinLPP ?cutout cutout))
  )

  ;create power grid blockages (TODO: some other way; see BUG 24326) 
  (when powerGrid
  x2box = (rightEdge   prb->bBox)
  x1box = (leftEdge   prb->bBox)
  y2box = (topEdge   prb->bBox)
  y1box = (bottomEdge   prb->bBox)
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
                                 @key (otherpinlpp nil) (cutout KeepoutM2to7WireSpacing))
  (let ()
    (leLayerSize cv pinlpp cutout Scratch1LPP)
    (when otherpinlpp (leLayerSize cv otherpinlpp cutout Scratch1LPP))
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
(defun AbstractInvertKeepout (lpp cv1 cv2 @key (cutout KeepoutM2to7WireSpacing))
  (foreach shape cv1->shapes
    (when shape->lpp==lpp (dbCopyFig shape cv2)->lpp=Scratch1LPP)
    )
  (leLayerSize cv2 Scratch1LPP cutout Scratch1LPP)
  (leLayerSize cv2 BoundaryLPP -cutout/2 Scratch2LPP)
  (leLayerAndNot cv2 Scratch2LPP Scratch1LPP lpp)
  (foreach shape cv2->shapes
    (cond (shape->lpp==Scratch1LPP (dbDeleteObject shape))
          (shape->lpp==Scratch2LPP (dbDeleteObject shape))
          )
    )
  t
  )

; KEEPOUT = (PRB)-cutout/2
(defun AbstractInvertCDCKeepout (lpp cv1 cv2 @key (cutout KeepoutM2to7WireSpacing))
;  (foreach shape cv1->shapes
;    (when shape->lpp==lpp (dbCopyFig shape cv2)->lpp=Scratch1LPP)
;    )
  (leLayerSize cv2 Scratch1LPP cutout Scratch1LPP)
  (leLayerSize cv2 BoundaryLPP -cutout/2 Scratch2LPP)
  (leLayerAndNot cv2 Scratch2LPP Scratch1LPP lpp)
  (foreach shape cv2->shapes
    (cond (shape->lpp==Scratch1LPP (dbDeleteObject shape))
          (shape->lpp==Scratch2LPP (dbDeleteObject shape))
          )
    )
  t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MakeLeafAbstract ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun MakeLeafAbstract (@key (CV (geGetEditCellView))
                              (cutout KeepoutM2to7WireSpacing)
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
    (when floodfillM1Keepout (AbstractInvertKeepout Metal1byLPP CV abstractCV ?cutout cutout) )
    (when floodfillM2Keepout (AbstractInvertKeepout Metal2byLPP CV abstractCV ?cutout cutout) )
    (when floodfillM3Keepout (AbstractInvertKeepout Metal3byLPP CV abstractCV ?cutout cutout) )

    ; extend M3 keepout to left and right sides of cell
    prbSpacing = KeepoutM2to7WireSpacing/2
    (when !floodfillM3Keepout && extendM3Keepout
          (foreach shape CV->shapes
                   (when shape->lpp==Metal3LPP && !shape->pin && shape->objType!="label"
                         xy0=(car  shape->bBox)
                         xy1=(cadr shape->bBox)
                         y0=(cadr xy0)
                         y1=(cadr xy1)
                         x0=(car llPrB)+prbSpacing
                         x1=(car urPrB)-prbSpacing
                         trackshape=(list x0:y0 x1:y1)
                         (dbCreateRect abstractCV Metal3byLPP trackshape)
                         )
                   )
          )

    ; extend M2 keepout to top and bottom sides of cell
    (when !floodfillM2Keepout && extendM2Keepout
          (foreach shape CV->shapes
                   (when shape->lpp==Metal2LPP && !shape->pin && shape->objType!="label"
                         xy0=(car  shape->bBox)
                         xy1=(cadr shape->bBox)
                         y0=(cadr xy0)
                         y1=(cadr xy1)
                         x0=(car llPrB)+prbSpacing
                         x1=(car urPrB)-prbSpacing
                         trackshape=(list x0:y0 x1:y1)
                         (dbCreateRect abstractCV Metal2byLPP trackshape)
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
    (AbstractCutAroundPins abstractCV Metal2LPP Metal2byLPP ?cutout cutout)
    (AbstractCutAroundPins abstractCV Metal3LPP Metal3byLPP ?cutout cutout)

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
    (AbstractCutAroundPins abstractCV Metal2LPP Metal2byLPP ?cutout 0)
    (AbstractCutAroundPins abstractCV Metal3LPP Metal3byLPP ?cutout 0)

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
      (foreach layer (list Metal1byLPP Metal2byLPP Metal3byLPP Metal4byLPP
                           Metal5byLPP Metal6byLPP Metal7byLPP Metal8byLPP)
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
                              (cutout KeepoutM2to7WireSpacing)
                              (topMetal nil))
  (MakeSimpleAbstract ?CV CV ?cutaroundpins cutaroundpins ?cutout cutout
                      ?topMetal topMetal
                      ?powerGrid t) ; TODO: BUG 24326
  )

(defun MakeAbstract ( @key (CV (geGetEditCellView))
                           (cutaroundpins t)
                           (cutout KeepoutM2to7WireSpacing)
                           (topMetal nil))
  (if (IsLeafCell CV->cellName)
    (MakeLeafAbstract ?CV CV ?cutout cutout)
    (MakeMidAbstract ?CV CV ?cutaroundpins cutaroundpins ?cutout cutout ?topMetal topMetal)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MakeCDCLeafAbstract ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun MakeCDCLeafAbstract (@key (cv (geGetEditCellView))
                              (cutaroundpins t)
                              (cutout KeepoutM2to7WireSpacing)
                              (topMetal 3))
  (let (abstractcv name net fig term prb
       havem2pins havem3pins llPrB urPrB llshapeM3 urshapeM3 llM3y urM3y
       lxPrB rxPrB trackshape)

  abstractcv=(dbOpenCellViewByType cv->libName cv->cellName "abstract" "maskLayout" "w")
  (when !abstractcv
    (printf "Couldn't edit abstract view for %s\n" cv->cellName)
    (error "Cellview not writable.\n")
  )
  (printf "Generating abstract of %s at %s\n" cv->cellName (getCurrentTime))
  (foreach shape abstractcv->shapes
    (when shape (dbDeleteObject shape) )
  )

    ; flatten pcells and vias to abstract_temp view
    abstract_temp=(dbCopyCellView cv cv->libName cv->cellName "abstract_temp" nil nil t)
    (when !abstract_temp (error "Couldn't edit abstract_temp view for %s\n" cv->cellName))
    cv=abstract_temp
    (foreach inst cv->instances (dbFlattenInst inst 32 t))
    (foreach inst cv->vias      (dbFlattenInst inst 32 t))
    (dbSave cv)

  ; create prBoundary
  prb = (GetPrbound cv)
  (dbCreatePRBoundary abstractcv (GetPrboundPoints cv))
  (dbCreatePolygon abstractcv BoundaryLPP (GetPrboundPoints cv))
  llPrB=(car prb->bBox)
  urPrB=(cadr prb->bBox)

  ; invert keepout
  (AbstractInvertCDCKeepout Metal1byLPP cv abstractcv ?cutout cutout)
  (when topMetal>1 (AbstractInvertCDCKeepout Metal2byLPP cv abstractcv ?cutout cutout) )
;  (when topMetal>2 (AbstractInvertCDCKeepout Metal3byLPP cv abstractcv ?cutout cutout) )

  ; extend M3 keepout to left and right sides of cell
  (foreach shape cv->shapes
    (when shape->lpp== Metal3LPP && !shape->pin && shape->objType!="label"
	llshapeM3=(car shape->bBox)
	urshapeM3=(cadr shape ->bBox)
	llM3y=(cadr llshapeM3)
        urM3y=(cadr urshapeM3)
        lxPrB=(car llPrB)
	rxPrB=(car urPrB)	
	trackshape=(list lxPrB+0.06 : llM3y rxPrB-0.06 : urM3y)
;	trackshape=shape->bBox
	(dbCreateRect abstractcv Metal3byLPP trackshape)
        )
    )

  ; explicit copy of m3 pwr stripes
  (foreach shape cv->shapes
    (when shape->lpp== Metal3LPP
      (when IsM3Power(shape)==GNDNetName || IsM3Power(shape)==VddNetName
        name=IsM3Power(shape)
        net=(dbFindNetByName abstractcv name)
        (when !net net=(dbCreateNet abstractcv name))
        term=(dbFindTermByName abstractcv name)
        (when !term (dbCreateTerm net name "input"))
        fig=(dbCopyFig shape abstractcv)
        (dbCreatePin net fig)
      )
    )
  )

  ; copy other pins to abstract
  havem2pins=nil
  havem3pins=t
   (foreach shape cv->shapes
    (when shape->pin || (shape->lpp==Metal3pinLPP && shape->objType != "label")
      (when shape->net->name!=GNDNetName && shape->net->name!=VddNetName
        name=shape->net->name
        net=(dbFindNetByName abstractcv name)
        (when !net net=(dbCreateNet abstractcv name))
        term=(dbFindTermByName abstractcv name)
        (when !term (dbCreateTerm net name "inputOutput"))
        fig=(dbCopyFig shape abstractcv)
        (dbCreatePin net fig)
      )
      (when shape->lpp==Metal2LPP || shape->lpp==Metal2pinLPP
        havem2pins=t
      )
      (when shape->lpp==Metal3LPP || shape->lpp==Metal3pinLPP
        havem3pins=t
      )
    )
  )
  (PinUtilOrientLabels abstractcv)

  ; chop keepouts around pins
  (when cutaroundpins
    (when havem2pins (AbstractCutAroundPins abstractcv Metal2LPP Metal2byLPP
                                            ?otherpinlpp Metal2pinLPP ?cutout cutout))
    (when havem3pins (AbstractCutAroundPins abstractcv Metal3LPP Metal3byLPP
                                            ?otherpinlpp Metal3pinLPP ?cutout cutout))
  )

; (CopyKeepoutToBlockage abstractcv)
 (SetLefProperties abstractcv)
 (dbSave abstractcv)
  dd_CV = (ddGetObj abstractcv->libName abstractcv->cellName "abstract_temp")
  (when dd_CV (ddDeleteObj dd_CV))
 abstractcv
 )
)

defun( IsM3Power ( shape )
  prog(( x1 x2 y1 y2 nthRail)
    y1=bottomEdge( shape->bBox )
    y2=topEdge( shape->bBox )
    x1=leftEdge( shape->bBox )
    x2=rightEdge( shape->bBox )
    if(x2-x1<0.25 then return(nil))
    nthRail=floor(y2/1.1)
    if(y1>nthRail*1.1+0.125 then return(nil) )
;    if(y2<nthRail*1.1+1.223 then return(nil) )
    if( nthRail==floor(nthRail/2.0)*2.0 return(GNDNetName) )
    return(VddNetName)
  )
)
