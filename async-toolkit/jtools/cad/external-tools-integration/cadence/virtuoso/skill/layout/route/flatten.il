; Replacement for ui/Nano/Flatten/FlattenView.il

useICV = t

; Top-level function to make a flatten view and switch to it
(defun MakeFlatten
  (@key (CV (geGetEditCellView))   ; specify CellView or nil to use edit view
        (topMetal 8)               ; top metal for power grid
        (RegenerateFromCast nil)   ; force CAST query
        (Verbose nil)              ; verbose
        (ConnectDanglingPins t)    ; paint metal on top of one-terminal subcell pins
        (ConnectAbuttingPins nil)  ; paint metal between nearby pins on the same net
        (doMakePowerGrid t)        ; make power grid
        (doPowerGridAbstract t)    ; generate abstract view of power grid
        (hookupD04 nil)            ; add power hookups around d04 cells
        (hookupLaygen nil)         ; add power hookups around Laygen cells
        (noPowerFill nil)          ; use POWER_GRID_NO_FILL instead of POWER_GRID
        )
  (let (MaxHeapSize FlattenCellsList
        PowerGridCellName flattenCV xalign yalign
        pgCV win TEMP)

    ; check viewName
    (when (and CV->viewName!="floorplan" CV->viewName!="prelayout")
      (error "Wrong view: Call MakePrelayout or MakeFlatten on floorplan/prelayout view\n"))

    ; create prelayout view if necessray
    (when CV->viewName=="floorplan" CV = (MakePrelayout ?CV CV))

    ; save prelayout view
    (dbSave CV)

    ; options
    (LoadWires ?CV CV)
    MaxHeapSize = (nrGetMaxHeapSize)

    ; generated cell or file names
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    FlattenCellsList = (strcat TEMP "/" CV->cellName ".flatten_cells_list")
    PowerGridCellName = (strcat CV->cellName "_pg")

    ; Create flatten view
    flattenCV = (nrFlattenView CV "flatten" PowerGridCellName "layout"
                               FlattenCellsList ?Verbose Verbose
                               ?DeleteWires nil
                               ?RegenerateFromCast RegenerateFromCast
                               ?MaxHeapSize MaxHeapSize)
    (unless flattenCV (error "Unable to create flatten view\n"))

    ; set properties
    xalign = (GetXAlignmentProp CV)
    yalign = (GetYAlignmentProp CV)
    (SetXAlignmentProp flattenCV (max (if xalign xalign 0) GridPolyPitch))
    (SetYAlignmentProp flattenCV (max (if yalign yalign 0) GridPitch))
    (dbSave flattenCV)

    ; switch current window
    CV = flattenCV
    win = (hiGetCurrentWindow)
    (when win (geOpen ?window win
                      ?lib  CV->libName
                      ?cell CV->cellName
                      ?view CV->viewName
                      ?viewType "maskLayout"
                      ?mode "a")
          )

    ; create FlattenCellsList file
    celllist_filename = FlattenCellsList
    instances= (setof inst CV->instances inst->libName!=TechLibName)
    instances= (append (car (NameFilterInstances instances LibCellsToIgnore))
                       (cadr (NameFilterInstances instances PowerGridCellPairRegExs)))
    uniqueInstances=nil
    (foreach inst instances
      (when !(member inst->cellName uniqueInstances->cellName)
            uniqueInstances=(cons inst uniqueInstances )
            )
      )
    pout=(outfile celllist_filename)
    (when pout
      (foreach inst uniqueInstances
               (fprintf pout "%s %s %s\n" inst->libName inst->cellName "abstract")
               )
      (close pout)
      )

    ; draw pins
    (RedrawAllPins ?CV CV)
    
    ; canonicalize prerouted nets
    (CanonicalizeNets ?CV CV)

    ; warn or delete redundant non-canonical pins
    (DeleteRedundantPins ?CV CV ?warnOnly !delete_redundant_pins)

    ; cache topMetal
    (dbReplaceProp CV "topMetal" "integer" topMetal)

    ; Create power grid
    (when doMakePowerGrid
      pgCV = (MakePowerGrid ?CV CV ?topMetal topMetal ?hookupLaygen hookupLaygen ?hookupD04 hookupD04
                            ?noPowerFill noPowerFill)
      (unless pgCV (error "Unable to create power grid\n"))
      )

    ; Create power grid abstract
    (when doPowerGridAbstract
      pgCV = (MakePowerGridAbstract ?CV CV ?topMetal topMetal)
      (unless pgCV (error "Unable to create power grid abstract\n"))
      )

    ; create power pins
    (CreatePowerPins topMetal ?CV CV)

    ; place metal on top of dangling subcell pins for StarRC extraction
    (when ConnectDanglingPins
      (nrCreateSingleNetConnector CV MetalLPPs ?MaxHeapSize MaxHeapSize)
      )

    ; route abutting pins too
    (when ConnectAbuttingPins
      (nrCreateNeighborNetConnector CV DefaultWiringSpacing MetalLPPs ?maxFanout 2)
      )

    ; fill in fields for DEF export
    (foreach inst CV->instances
      (unless inst->typeName==TechLibName
        (when inst->cellName==PowerGridCellName inst->source = "dist") ; SOURCE DIST
        )
      )

    ; fix pin placement for DEF export
    (foreach term CV->terminals
      (foreach pin term->pins
        (foreach fig pin->figs
          (dbSetPinFigPlacementStatus fig "locked")
          )
        )
      )

    ; update pin directions
    (UpdateFloorplan ?CV CV ?recurse nil)

    ; set fields for LEF export
    (SetLefProperties CV)

    ; for DEF export
    (CopyKeepoutToBlockage CV)

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
        (when b<a (error "%s:%s not grid aligned\n" shape->layerName shape->purpose))
        b = (min b topMetal)
        top = (min b top)
        (when b>a || a==2 && b==2
          ; rectangle coordinates
          align = gridAlignment[b]
          xa = (car align)
          ya = (cadr align)
          gridCV = (ChoosePowerGridCell a b ?noPowerFill noPowerFill)
          x0 = (leftEdge   shape->bBox)
          y0 = (bottomEdge shape->bBox)
          x1 = (rightEdge  shape->bBox)
          y1 = (topEdge    shape->bBox)
          cols = (round (x1-x0)/(car  align))

          ; half-height cells on bottom edge
          (when (round y0/ya)*2 != (round y0/(ya/2))
                halfCV = (ChoosePowerGridCell a b ?half t ?noPowerFill noPowerFill)
                y0 = y0+ya/2
                (dbCreateSimpleMosaic powerCV halfCV
                                      (sprintf nil "pg%d" count++) x0:y0 "MX"
                                      1 cols (cadr align) (car align))
                )
          
          ; half-height cells on top edge
          (when (round y1/ya)*2 != (round y1/(ya/2))
                y1 = y1-ya/2
                halfCV = (ChoosePowerGridCell a b ?half t ?noPowerFill noPowerFill)
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
      srcCV = powerCV ; use powerCV as scratch to build more layers of power grid
      )

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
;                          Metal[6] Metal[7] Metal[8]))) ; which layers to keep as pins
        (powerLayers (list Metal[0] Metal[2]))) ; HACK: minimize number of pin shapes
  (let (inst pgCV pgaCV lpps figsVdd figsGND)
    
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
    (foreach shape (setof shape pgaCV->shapes (member shape->layerName powerLayers))
             (cond (shape->purpose=="vdd"
                    shape->purpose="drawing"
                    figsVdd = (cons shape figsVdd)
                    )
                   (shape->purpose=="gnd"
                    shape->purpose="drawing"
                    figsGND = (cons shape figsGND)
                    )
                   )
             )
    (dbAddFigsToPin (dbCreatePin (dbMakeNet pgaCV VddNetName) nil) figsVdd)
    (dbAddFigsToPin (dbCreatePin (dbMakeNet pgaCV GNDNetName) nil) figsGND)

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
(defun CreatePowerPins (topMetal @key (CV (geGetEditCellView)) (suffix "_pg"))
  (let (inst pgaCV lpp new_shape figsVdd figsGND)
    (DeletePinsByType "Power" ?CV CV)
    inst = (dbFindAnyInstByName CV "pg")
    (when inst pgaCV = (dbOpenCellViewByType inst->libName inst->cellName "abstract"))
    (unless pgaCV (printf "WARNING: no pg instance to make power pins from!"))
    (when pgaCV
      lpp = (list Metal[topMetal] "drawing")
      (foreach shape (setof x pgaCV->shapes x->lpp==lpp)
               new_shape = (dbCopyShape shape CV)
               new_shape->lpp = (list (car new_shape->lpp) "pin") ; hack
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
