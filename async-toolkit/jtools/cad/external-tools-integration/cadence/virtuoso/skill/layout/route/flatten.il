; Flatten floorplan view to flatten view.  CAST cells without
; routed=true are inlined.  Also adds power and color grids, and runs
; UpdateFloorplan and RedrawAllPins and preps for DEF export.

useICV = t

; Top-level function to make a flatten view and switch to it
(defun MakeFlatten
  (@key (CV (geGetEditCellView))   ; specify CellView or nil to use edit view
        (topMetal 8)               ; top metal for power grid
        (RegenerateFromCast nil)   ; force CAST query
        (Verbose nil)              ; verbose
        (ConnectDanglingPins t)    ; paint metal on top of one-terminal subcell pins
        (ConnectAbuttingPins t)    ; paint metal between nearby pins on the same net
        (powerGrid t)              ; add a power grid
        (colorGrid t)              ; add a color grid
        (hookupD04 nil)            ; add power hookups around d04 cells
        (hookupLaygen nil)         ; add power hookups around Laygen cells
        (noPowerFill nil)          ; use POWER_GRID_NO_FILL instead of POWER_GRID
        (doPowerGates nil)         ; use POWER_GRID_GATED instead of POWER_GRID
        (softMacro nil)            ; when !softMacro, lock all instances
        )
  (let (MaxHeapSize TEMP PowerGridCellName ColorGridCellName xalign yalign win pgCV cgCV)

    ; check viewName
    (when CV->viewName!="floorplan" (error "call MakeFlatten on floorplan view\n"))

    ; options
    (LoadWires ?CV CV)
    MaxHeapSize = (nrGetMaxHeapSize)

    ; generated cell or file names
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    PowerGridCellName = (strcat CV->cellName "_pg")
    ColorGridCellName = (strcat CV->cellName "_color")

    ; get alignment properties
    xalign = (GetXAlignmentProp CV)
    yalign = (GetYAlignmentProp CV)

    ; Create flatten view from floorplan view
    CV = (DirectFlatten ?CV CV ?RegenerateFromCast RegenerateFromCast)

    ; lock all instances unless softMacro
    (unless softMacro (foreach x CV->instances x->status="locked"))

    ; draw pins
    (AddBusWires ?CV CV)
    (RedrawAllPins ?CV CV ?include_supply t ?supply_topMetal topMetal)
    (CanonicalizeNets ?CV CV)
    (DeleteRedundantPins ?CV CV ?warnOnly !delete_redundant_pins)

    ; update connectivity
    (UpdateFloorplan ?CV CV ?recurse nil)

    ; cache various information in CV properties
    (dbReplaceProp CV "topMetal" "integer" topMetal)
    (SetXAlignmentProp CV (max (if xalign xalign 0) GridPolyPitch))
    (SetYAlignmentProp CV (max (if yalign yalign 0) GridPitch))
    (SetLeafProp CV nil)
    (SetLefProperties CV)

    ; Create power grid and abstract
    (when powerGrid
      pgCV = (MakePowerGrid ?CV CV ?topMetal topMetal ?hookupLaygen hookupLaygen ?hookupD04 hookupD04
                            ?noPowerFill noPowerFill ?doPowerGates doPowerGates)
      (unless pgCV (error "Unable to create power grid\n"))
      pgCV = (MakePowerGridAbstract ?CV CV ?topMetal topMetal ?doPowerGates doPowerGates)
      (unless pgCV (error "Unable to create power grid abstract\n"))
      (CreatePowerPins ?topMetal topMetal ?CV CV)
      )

    ; Create color grid and abstract
    (when colorGrid
      cgCV = (MakeColorGrid ?CV CV)
      (unless cgCV (error "Unable to create color grid\n"))
      cgCV = (betterCopyCellView cgCV cgCV->libName cgCV->cellName "abstract" nil nil t)
      (unless cgCV (error "Unable to create color grid abstract\n"))
      (SetLefProperties cgCV ?powergrid t)
      (dbSave cgCV)
      )

    ; place metal on top of dangling subcell pins for StarRC extraction
    (when ConnectDanglingPins && !softMacro
      (nrCreateSingleNetConnector CV MetalLPPs ?MaxHeapSize MaxHeapSize)
      )

    ; route abutting pins
    (when ConnectAbuttingPins && !softMacro
      (RouteNearby ?CV CV ?topShapes t)
      )

    ; fix things for DEF export
    (foreach inst CV->instances
      (when inst->cellName==PowerGridCellName || inst->cellName==ColorGridCellName
            inst->source = "dist" ; SOURCE DIST
            )
      )
    (foreach term CV->terminals
      (foreach pin term->pins
        (foreach fig pin->figs
          (dbSetPinFigPlacementStatus fig "locked")
          )
        )
      )
    (CopyKeepoutToBlockage CV)

    ; switch current window to flatten view
    win = (hiGetCurrentWindow)
    (when win (geOpen ?window win
                      ?lib  CV->libName
                      ?cell CV->cellName
                      ?view CV->viewName
                      ?viewType "maskLayout"
                      ?mode "a")
          )

    ; return flatten view
    (dbSave CV)
    CV
    )
  )

; MakePowerGrid
(defun MakePowerGrid
  (@key (CV (geGetEditCellView)) ; flatten CV
        (topMetal 8)
        (hookupLaygen nil) ; HACK: add power hookups to all LayGen cells
        (hookupD04 nil)    ; HACK: add power hookups to all D04 cells
        (powerCellView "layout") ; used by MakeLayoutTagCell
        (noPowerFill nil)
        (doPowerGates nil)
        )
  (let (TEMP powerCellName align
        inst prb
        x0 y0 x1 y1
        inLayerMap outLayerMap shapes a b srcCV top
        powerCV pgMap
        count gridCV halfCV rows cols
        tiehilo GND Vdd instGND instVdd)

    ; generated cell or file names
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    powerCellName = (strcat CV->cellName "_pg")

    ; delete old power grid instance if it exists
    inst = (dbFindAnyInstByName CV "pg")
    (when inst (dbDeleteObject inst))

    ; clear power grid cell
    powerCV = (dbOpenCellViewByType CV->libName powerCellName powerCellView "maskLayout" "w")
    (unless powerCV (error "Can't write %s\n " powerCellName))

    ; get prBoundary boundary shapes
    (unless CV->prBoundary->points (error "Bad prBoundary"))
    prb = (dbCreatePolygon CV (list "prBoundary" "boundary") CV->prBoundary->points)
    (dbSave CV)
    align = gridAlignment[topMetal]
    (when topMetal==8 align=gridAlignment["lego"]) ; HACK: special case for M8
    (when (CheckPointsOffGrid prb->points align)
      (error "Not all prBoundary vertices are on grid")
      )

    ; ICV layers
    outLayerMap = (list
                   (list "power2" MetalBlock[2])
                   (list "power3" MetalBlock[3])
                   (list "power4" MetalBlock[4])
                   (list "power5" MetalBlock[5])
                   (list "power6" MetalBlock[6])
                   (list "power7" MetalBlock[7])
                   (list "power8" MetalBlock[8])
                   )
    inLayerMap = (cons (list "prb" (list "prBoundary" "boundary")) outLayerMap)

    ; loop until power grid is built up to topMetal
    srcCV = CV
    top = 0
    count = 0
    (while top<topMetal
      top=topMetal

      ; run ICV to find the highest "block" purpose regions
      (ICVLayerProcessorSimple srcCV powerCV "MakePowerGrid" inLayerMap outLayerMap)

      ; chop into tiles
      (for m 2 topMetal
           shapes = (setof s powerCV->shapes s->lpp==MetalBlock[m])
           (dbLayerTile powerCV MetalBlock[m] shapes)
           (foreach s shapes (dbDeleteObject s))
           )

      ; replace block shapes with power mosaics
      (foreach shape (setof s powerCV->shapes s->purpose=="block" && s->objType=="rect")
        (for n 2 TopMetal (when shape->lpp==MetalBlock[n] a=n))
        b = (MaxAlignedMetalLayer shape->bBox)
        (when b<a (error "%s:%s not grid aligned at %f:%f\n" shape->layerName shape->purpose
                         (caar shape->bBox) (cadar shape->bBox) ))
        b = (min b topMetal)
        top = (min b top)
        (when b>a || a==2 && b==2
          ; rectangle coordinates
          align = gridAlignment[b]
          xa = (car align)
          ya = (cadr align)
          gridCV = (ChoosePowerGridCell a b)
          x0 = (leftEdge   shape->bBox)
          y0 = (bottomEdge shape->bBox)
          x1 = (rightEdge  shape->bBox)
          y1 = (topEdge    shape->bBox)
          cols = (round (x1-x0)/(car  align))

          ; half-height cells on bottom edge
          (when (round y0/ya)*2 != (round y0/(ya/2))
                halfCV = (ChoosePowerGridCell a b ?half t)
                y0 = y0+ya/2
                (dbCreateSimpleMosaic powerCV halfCV
                                      (sprintf nil "pg%d" count++) x0:y0 "MX"
                                      1 cols (cadr align) (car align))
                )

          ; half-height cells on top edge
          (when (round y1/ya)*2 != (round y1/(ya/2))
                y1 = y1-ya/2
                halfCV = (ChoosePowerGridCell a b ?half t)
                (dbCreateSimpleMosaic powerCV halfCV
                                      (sprintf nil "pg%d" count++) x0:y1 "R0"
                                      1 cols (cadr align) (car align))
                )

          ; full-height cells
          rows = (round (y1-y0)/ya)
          (when rows>0
            (dbCreateSimpleMosaic powerCV gridCV
                                  (sprintf nil "pg%d" count++) x0:y0 "R0"
                                  rows cols (cadr align) (car align))
            )

          ; block and delete guide shape
          (dbCreateRect powerCV MetalBlock[b] shape->bBox) ; block up to Metal[b]
          (dbDeleteObject shape)
          )
        )
      (dbSave powerCV)
      (printf "MakePowerGrid top=%d count=%d\n" top count)
      (when doPowerGates (printf "Making power grid for gated Vdd\n"))
      srcCV = powerCV ; use powerCV as scratch to build more layers of power grid
      )

    ; hack: if noPowerFill do a bunch more crap
    (when noPowerFill (PatchPowerFill CV powerCV))
    (when doPowerGates (PatchPowerGates CV powerCV))

    ; delete temporary shapes, draw prBoundary and top block shape
    (dbDeleteObject prb)
    (foreach s powerCV->shapes (when s->purpose=="block" (dbDeleteObject s)))
    (dbCreatePRBoundary powerCV CV->prBoundary->points)
    (dbCreatePolygon powerCV (list Metal[topMetal] "block") CV->prBoundary->points)

    ; create power pins and instantiate power grid
    (dbCreateTerm (MakeNet powerCV GNDNetName) GNDNetName "input")
    (dbCreateTerm (MakeNet powerCV VddNetName) VddNetName "input")
    tiehilo = (dbCreateInst CV powerCV "pg" 0:0 "R0")
    (AnchorInstance tiehilo)
    GND = (dbFindNetByName CV GNDNetName)
    Vdd = (dbFindNetByName CV VddNetName)
    instGND = (dbFindTermByName powerCV GNDNetName)
    instVdd = (dbFindTermByName powerCV VddNetName)
    (dbCreateInstTerm GND tiehilo instGND)
    (dbCreateInstTerm Vdd tiehilo instVdd)

    ; HACK: add explicit power hookups before exporting to ICC
    (LaygenPowerHookup CV powerCV ?hookupLaygen hookupLaygen ?hookupD04 hookupD04)

    ; save and return
    (dbSave CV)
    (dbSave powerCV)
    (dbClose powerCV)
    powerCV
    )
  )

; Allow router to use the middle track between GND stripes on M2-M5.
; Swaps mosaics to POWER_GRID_NO_FILL versions, but add back some fill
; on the prBoundary and near hard macros.  Very nasty.  Expected to
; run from within MakePowerGrid after any old pg instance has been
; deleted.
(defun PatchPowerFill (CV pgCV)
  (let (tempCV inLayerMap outLayerMap len)
    tempCV = (dbOpenCellViewByType CV->libName (strcat CV->cellName "_fill_temp")
                                   "layout" "maskLayout" "w")

    ; flatten pg fill to tempCV
    (for m 2 5
         outLayerMap = (cons (list (sprintf nil "f%d" m) (list Metal[m] "fill")) outLayerMap)
         )
    (ICVLayerProcessorSimple pgCV tempCV "mergeShape" outLayerMap outLayerMap)

    ; flatten CV block purpose shapes to tempCV
    inLayerMap = (list (list "b3" (list Metal[3] "block"))
                       (list "b4" (list Metal[4] "block"))
                       (list "b5" (list Metal[5] "block"))
                       (list "b6" (list Metal[6] "block"))
                       (list "b7" (list Metal[7] "block"))
                       (list "b8" (list Metal[8] "block")))
    (ICVLayerProcessorSimple CV tempCV "mergeShape" inLayerMap inLayerMap)

    ; keep fill shapes near block shapes, save back to pgCV
    (dbCreatePolygon tempCV (list "prBoundary" "drawing") CV->prBoundary->points)
    inLayerMap = (append inLayerMap outLayerMap)
    inLayerMap = (cons (list "prb" (list "prBoundary" "drawing")) inLayerMap)
    (ICVLayerProcessorSimple tempCV pgCV "PatchPowerFill" inLayerMap outLayerMap)

    ; swap POWER_GRID cells to NO_FILL version
    len = (strlen "globals.POWER_GRID")
    (foreach inst pgCV->instances
             (when (strncmp inst->cellName "globals.POWER_GRID" len)==0
                   inst->master = (dbOpenCellViewByType inst->libName
                                                        (strcat "globals.POWER_GRID_NO_FILL"
                                                                (substring inst->cellName len+1))
                                                        "layout")
                   )
             )

    ; clean up
    (dbSave pgCV)
    pgCV
    )
 )

(defun PatchPowerGates (CV pgCV)
  (let (tempCV inLayerMap outLayerMap len)

    ; swap POWER_GRID cells to GATED version
    len = (strlen "globals.POWER_GRID")
    (foreach inst pgCV->instances
             (when (strncmp inst->cellName "globals.POWER_GRID" len)==0
                   inst->master = (dbOpenCellViewByType inst->libName
                                                        (strcat "globals.POWER_GRID_GATED"
                                                                (substring inst->cellName len+1))
                                                        "layout")
                   )
             )

    ; clean up
    (dbSave pgCV)
    pgCV
    )
 )


; Create power hookups for Laygen and/or D04 cells
; BUG: should be done by RDT flow but doesn't work yet
(defun LaygenPowerHookup (CV powerCV @key (hookupLaygen t) (hookupD04 t))
  (let (phCV bbox yp y0 y1 y odd orient (count 0))
    phCV = (dbOpenCellViewByType "globals" "globals.POWER_HOOKUP_HALF.0" "layout")
    (foreach inst (setof x CV->instances
                         (or (and hookupLaygen (dbGetPropByName x->master "LaygenVersion"))
                             (and hookupD04 (isStandardCell x->master))))
      bbox = (dbTransformBBox inst->master->prBoundary->bBox inst->transform)
      yp = (cadr gridAlignment[2])/2
      y0 = (round (cadr (car  bbox))/yp)
      y1 = (round (cadr (cadr bbox))/yp)
      (for row y0 y1-1
           odd = (mod row 2)!=0
           y = yp*(if odd row+1 row)
           orient = (if odd "MX" "R0")
           (foreach x (list (car (car bbox)) (car (cadr bbox)))
                    (dbCreateInst powerCV phCV (sprintf nil "ph%d" count++) x:y orient)
                    )
           )
      )
    )
  t
  )

; generate abstract of a power grid cell
; flattens and merges vdd and gnd purpose shapes
; converts those on specified layers to pins with strong connectivity
; others layers become boundary purpose
(defun MakePowerGridAbstract
  (@key (CV (geGetEditCellView)) ; flatten CellView
        (topMetal 8)
;       (powerLayers (list Metal[0] Metal[1] Metal[2] Metal[3] Metal[4] Metal[5]
;                          Metal[6] Metal[7] Metal[8])) ; which layers to keep as pins
        (powerLayers (list Metal[0] Metal[2] Metal[3])) ; HACK: minimize number of pin shapes
	(doPowerGates nil)
	)
  (let (inst pgCV pgaCV lpps figsVdd figsGND figsVddg)

    ; HACK: minimize number of pin shapes
    powerLayers = (cons Metal[topMetal] powerLayers)

    ; find powergrid CV
    inst = (dbFindAnyInstByName CV "pg")
    (unless inst (error "No pg instance"))
    pgCV = inst->master

    ; flatten and merge fill, gnd, vdd purpose shapes on many layers.
    lpps = (list (list "vcn" "drawing"))
    (foreach purpose (list "vdd" "gnd" "fill")
             (for m 0 TopMetal lpps = (cons (list Metal[m] purpose) lpps))
             (for m 0 TopMetal-1 lpps = (cons (list Via[m] purpose) lpps))
             )
    (when useICV
      pgaCV = (dbOpenCellViewByType pgCV->libName pgCV->cellName "abstract" "maskLayout" "w")
      pgaCV = (ICVMergeShapes pgCV pgaCV lpps)
      )
    (when !useICV ; BUG: work-around if ICV not available
      pgaCV=(betterCopyCellView pgCV pgCV->libName pgCV->cellName "abstract" nil nil t)
      (foreach inst pgaCV->instances (dbFlattenInst inst 32))
      (foreach label (setof x pgaCV->shapes x->objType=="label") (dbDeleteObject label))
      (foreach shape pgaCV->shapes (when shape->purpose=="block" (dbDeleteObject shape)))
      (dbSave pgaCV)
      )

    ; open power grid abstract
    pgaCV = (dbOpenCellViewByType pgCV->libName pgCV->cellName "abstract" "maskLayout" "a")
    (unless pgaCV (error "Unable to read %s abstract\n" pgCV->cellName))

    ; create prBoundary object
    (dbCreatePRBoundary pgaCV (GetPrboundPoints pgCV))
    (dbCreatePolygon pgaCV BoundaryLPP (GetPrboundPoints pgCV))

    ; create power pins on specified metal layers
    ; only create 1 pin each for Vdd and GND and add figures to it
    ; this represents strongly connected pins needed for correct LEF

    ; HACK: hastily added some speculative processing for power gating
    (when doPowerGates (printf "Making power grid abstract for gated Vdd\n"))
    (foreach shape (setof shape pgaCV->shapes (member shape->layerName powerLayers))
      (cond (shape->purpose=="gnd"
             shape->purpose="pin"
             figsGND = (cons shape figsGND)
             )
            )

      (when doPowerGates
        (when (MetalNumber ?lpp shape->lpp) < 2
              ; normal
             (cond (shape->purpose=="vdd"
                    shape->purpose="pin"
                    figsVdd = (cons shape figsVdd)
                    )
		   )
	  )
        (when (MetalNumber ?lpp shape->lpp) == 2
              ; m2 vdd becomes Vddg pins
             (cond (shape->purpose=="vdd"
                    shape->purpose="pin"
                    figsVddg = (cons shape figsVddg)
                    )
		   )
          )
        (when 3 <= (MetalNumber ?lpp shape->lpp) && (MetalNumber ?lpp shape->lpp) < 5
              ; mx vdd becomes Vddg pins
	      ; mx fill becomes Vdd pins
             (cond (shape->purpose=="vdd"
                    shape->purpose="pin"
                    figsVddg = (cons shape figsVddg)
                    )
		   )
             (cond (shape->purpose=="fill"
                    shape->purpose="pin"
                    figsVdd = (cons shape figsVdd)
                    )
		   )
	  )
        (when (MetalNumber ?lpp shape->lpp) > 5
              ; normal
             (cond (shape->purpose=="vdd"
                    shape->purpose="pin"
                    figsVdd = (cons shape figsVdd)
                    )
		   )
	  )
	)
      (when !doPowerGates
             (cond (shape->purpose=="vdd"
                    shape->purpose="pin"
                    figsVdd = (cons shape figsVdd)
                    )
		   )
        )
      )

    (dbAddFigsToPin (dbCreatePin (dbMakeNet pgaCV VddNetName) nil) figsVdd)
    (dbAddFigsToPin (dbCreatePin (dbMakeNet pgaCV GNDNetName) nil) figsGND)
    (dbAddFigsToPin (dbCreatePin (dbMakeNet pgaCV "Vddg") nil) figsVddg)

    ; convert remaining shapes to boundary
    (foreach shape (setof shape pgaCV->shapes shape->pin==nil)
             shape->purpose=KeepoutPurpose
             )
    (CopyKeepoutToBlockage pgaCV)

    ; set fields for LEF export
    (SetLefProperties pgaCV ?powergrid t)

    ; save and return
    (dbSave CV)
    (dbSave pgaCV)
    (dbClose pgaCV)
    pgaCV
    )
  )

; Mod operation converting float number to integer in terms of MfgGrid
(defun GetNonZeroMod (div dis)
  (mod (round div/MfgGrid) (round dis/MfgGrid)) != 0
  )

; Check vertices are on powergrid pitch
(defun CheckOffGrid (point align)
  (GetNonZeroMod (car point) (car align)) || (GetNonZeroMod (cadr point) (cadr align))
  )

; Check if a list of points is off grid
(defun CheckPointsOffGrid (points align)
  (CheckOffGrid (car points) align) || (cdr points) && (CheckPointsOffGrid (cdr points) align)
  )

; Figure out the highest metal layer which is grid aligned for a list of points
(defun MaxAlignedMetalLayer (points)
  (let (top align)
    top=0
    (for m 2 TopMetal
         align = gridAlignment[m]
         (when m==8 align=gridAlignment["lego"]) ; HACK: special case for M8
         (unless (CheckPointsOffGrid points align) top=m)
         )
    top
    )
  )

; sort a list of vertices by smallest Y then smallest X
(defun SortVertices (vertices)
  (sort (copy vertices)
        (lambda (a b)
          (cond ((cadr a)<(cadr b) t)
                ((cadr a)>(cadr b) nil)
                ((car  a)<(car  b) t)
                ((car  a)>(car  b) nil)
                (t                 nil)
                )
          )
        )
  )

; Check vertices are on lowest Y-coord
(defun CheckBotVert ( pt bottom )
  (let ((x (car pt))
        (y (cadr pt)))
  y == bottom
  )
)

; Paint an lpp over all subcell prBoundary
(defun PaintOverSubcellPrBoundary (lpp @key (CV (geGetEditCellView)))
  (let (transform points shapes)
    (foreach inst CV->instances
             transform = inst->transform
             points = (GetPrboundPoints inst->master)
             (when points
               points = (TransformPoints points transform)
               shape = (dbCreatePolygon CV lpp points)
               shapes = (cons shape shapes)
               )
             )
    shapes
    )
  )

; Draw top-level Vdd/GND pins referring to power grid abstract
(defun CreatePowerPins (@key (CV (geGetEditCellView)) (topMetal nil))
  (let (inst pgaCV lpp new_shape figsVdd figsGND)
    (unless topMetal topMetal = (dbFindProp CV "topMetal")->value)
    (unless topMetal topMetal = TopMetal)
    (DeletePinsByType "Power" ?CV CV)
    inst = (dbFindAnyInstByName CV "pg")
    (when inst pgaCV = (dbOpenCellViewByType inst->libName inst->cellName "abstract"))
    (unless pgaCV (printf "WARNING: no pg instance to make power pins from!"))
    (when pgaCV
      lpp = (list Metal[topMetal] "pin")
      (foreach shape (setof x pgaCV->shapes x->lpp==lpp)
               new_shape = (dbCopyShape shape CV)
               (dbReplaceProp new_shape "PinType" "string" "Power")
               (dbReplaceProp new_shape "BusScriptObject" "boolean" nil)
               (cond (shape->net->name==VddNetName figsVdd = (cons new_shape figsVdd))
                     (shape->net->name==GNDNetName figsGND = (cons new_shape figsGND))
                     )
               )
      (dbAddFigsToPin (dbCreatePin (dbMakeNet CV VddNetName) nil) figsVdd)
      (dbAddFigsToPin (dbCreatePin (dbMakeNet CV GNDNetName) nil) figsGND)
      (foreach fig figsVdd (LaygenLabelPin CV fig))
      (foreach fig figsGND (LaygenLabelPin CV fig))
      )
    )
  t
  )

; Choose power grid cell
(defun ChoosePowerGridCell (bot top @key (half nil) (noPowerFill nil))
  (let (CV gridName)
    gridName = (if noPowerFill "globals.POWER_GRID_NO_FILL.m" "globals.POWER_GRID.m")
    (for n bot top gridName = (strcat gridName (sprintf nil "%d" n)))
    gridName = (strcat gridName "_pg")
    (when half gridName = (strcat gridName "_half"))
    CV = (dbOpenCellViewByType "globals" gridName "layout")
    (unless CV (error "couldn't open %s\n" gridName))
    CV
    )
  )

; draw inplace pins for VDDM
(defun DrawPinsVDDM (@key (CV (geGetEditCellView)))
  (foreach inst CV->instances
           (MidPinsInheritPin CV inst "VDDM" "VDDM")
           )
  t
  )

; draw inplace pins for VDDIO
(defun DrawPinsVDDIO (@key (CV (geGetEditCellView)))
  (foreach inst CV->instances
           (MidPinsInheritPin CV inst "VDDIO" "VDDIO")
           )
  t
  )

; Draw inplace power-ground pins for upper level cells
(defun DrawPinsPowerGround (@key (CV (geGetEditCellView)))
  (foreach inst CV->instances
           (foreach name PowerGroundNets
                    (MidPinsInheritPin CV inst name name)
                    )
           )
  t
  )

; go straight from floorplan to flatten view
(defun DirectFlatten (@key (CV (geGetEditCellView)) (RegenerateFromCast nil) (flatViewName "flatten"))
  (let (not_routed progress)

    ; create flatten view
    CV = (betterCopyCellView CV CV->libName CV->cellName flatViewName nil nil t)

    ; query CAST routed directives
    not_routed = (filterCastQuery ?CV CV ?RegenerateFromCast RegenerateFromCast
                                  ?filter "!directive=routed:true")

    ; flatten everything that should be flattened, convert the rest to layout views
    progress=t
    (while progress
      progress=nil
      (foreach inst CV->instances
               (cond (inst->objType=="mosaic"
                      (dbFlattenInst inst 1)
                      progress=t
                      )
                     (not_routed[inst->cellName] && inst->viewName=="floorplan"
                      (TrivialInline inst ?CV CV)
                      progress=t
                      )
                     (inst->viewName!="layout"
                      lCV = (ReadCV inst->libName inst->cellName "layout")
                      (when lCV inst->master=lCV)
                      )
                     )
               )
      )
    )
  CV
  )

; inline one level with correct instance names but no extra work
(defun TrivialInline (inst @key (CV (geGetEditCellView)))
  (foreach sub1 inst->master->instances
           sub2 = (dbCopyFig sub1 CV inst->transform)
           sub2->name = (strcat inst->name "." sub1->name)
           (when inst->status=="locked" sub2->status="locked")
           )
  (dbDeleteObject inst)
  t
  )

; Run a cast query for subcells with a specified filter condition,
; returning a mapping table from cellName to t|nil.
(defun filterCastQuery (@key
                        (filter "")
                        (CV (geGetEditCellView))
                        (RegenerateFromCast t)
                        (MaxHeapSize (nrGetMaxHeapSize))
                        )
  (let (dir file Command cells p_in line cellName)

    ; choose directory
    dir = (strcat (ConfigFileGetValue TheCDSConfigTable "TEMP") "/ildirectives")
    (unless (isDir dir)
      (when (system (strcat "mkdir " dir))!=0 (error "failed to create dir %s.\n" dir))
      )

    ; call cast_query to get a file
    file = (strcat dir "/" CV->cellName ".query.txt")
    (when !(isFile file) || RegenerateFromCast
          Command = (sprintf nil
                             (strcat "%s/cast_query --cast-path=%s --cell=%s --task=subcells "
                                     "--filter=\"%s\" "
                                     "--translate=cadence --cadence-name --output=%s "
                                     "--max-heap-size=%dM --64")
                             (PackageGetBinRoot)
                             (ConfigFileGetValue TheCDSConfigTable "CAST_PATH")
                             CV->cellName filter file MaxHeapSize
                             )
          (shell Command)
          )

    ; read the file and return table of cellNames
    cells = (makeTable filter nil)
    p_in = (infile file)
    (unless p_in (error "filterCastQuery unable to read %s\n" file))
    (while (gets line p_in)
      (sscanf line "%s" cellName)
      cells[cellName]=t
      )
    (close p_in)
    cells
    )
  )
