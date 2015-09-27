; Replacement for ui/Nano/Flatten/FlattenView.il

useAssura = nil ; BUG: Assura isn't working!
useICV = nil

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
      pgCV = (MakePowerGrid ?CV CV ?topMetal topMetal ?hookupLaygen hookupLaygen ?hookupD04 hookupD04)
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
        inst->status = "locked" ; FIXED
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
        )
  (let (TEMP powerCellName align
        inst prb vertprb
        x1box x2box y1box y2box x1 y1 x2 x2
        AssuraLayerMappings
        powerCV pgMap blks
        count gridName gridCV rows cols
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
    align = gridAlignment[topMetal]
    vertprb = (setof v prb->points (CheckOffGrid v align))
    (when vertprb (error "Not all prBoundary vertices are on grid: %L" vertprb))

    ; get bounding box as an integer multiple of MfgGrid
    x1box = 1.0*(round (leftEdge   prb->bBox)/MfgGrid)
    x2box = 1.0*(round (rightEdge  prb->bBox)/MfgGrid)
    y1box = 1.0*(round (bottomEdge prb->bBox)/MfgGrid)
    y2box = 1.0*(round (topEdge    prb->bBox)/MfgGrid)

    ; minimum sized power grid
    x1 = (floor   x1box/(round (car  align)/MfgGrid))
    y1 = (floor   y1box/(round (cadr align)/MfgGrid))
    x2 = (ceiling x2box/(round (car  align)/MfgGrid))
    y2 = (ceiling y2box/(round (cadr align)/MfgGrid))

    ; avoid M2 overlapping prBoundary of subcells (looks only one level down)
    blks = (PaintOverSubcellPrBoundary (list Metal[2] "block") ?CV CV)

    ; run Assura rules
    (when useAssura
      ; BUG: Assura isn't working.  ICV may be used instead.
      AssuraLayerMappings = (list
                             (list "power2" (list Metal[2] "block"))
                             (list "power3" (list Metal[3] "block"))
                             (list "power4" (list Metal[4] "block"))
                             (list "power5" (list Metal[5] "block"))
                             (list "power6" (list Metal[6] "block"))
                             (list "power7" (list Metal[7] "block"))
                             (list "power8" (list Metal[8] "block"))
                             )
      powerCV = (AssuraLayerProcessor AssuraLayerMappings "makePowerGrid"
                                      CV->libName powerCellName powerCellView ?CV CV)
      )
    (when !useAssura
      (let (power2 power4 power6 power8 temp3 temp7)
        ; BUG: temporary hack to avoid Assura layer processing
        (FlattenBlockPurpose CV powerCV)

        ; power grid blockages (BUG: ignores power blockages on odd layers)
        (dbCreatePolygon powerCV MetalBlock[2] prb->points)
        (leLayerAndNot powerCV MetalBlock[6] MetalBlock[8] (list "y6" "drawing"))
        (leLayerOr     powerCV MetalBlock[6] MetalBlock[8] (list "y7" "drawing"))
        (leLayerAndNot powerCV MetalBlock[4] (list "y7" "drawing") (list "y4" "drawing"))
        (leLayerOr     powerCV MetalBlock[4] (list "y7" "drawing") (list "y3" "drawing"))
        (leLayerAndNot powerCV MetalBlock[2] (list "y3" "drawing") (list "y2" "drawing"))
        (foreach s powerCV->shapes (when s->purpose=="block" (dbDeleteObject s)))

        ; chop into tiles
        power8 = (setof s powerCV->shapes s->lpp==MetalBlock[8])
        power6 = (setof s powerCV->shapes s->lpp==(list "y6" "drawing"))
        power4 = (setof s powerCV->shapes s->lpp==(list "y4" "drawing"))
        power2 = (setof s powerCV->shapes s->lpp==(list "y2" "drawing"))
        temp3  = (setof s powerCV->shapes s->lpp==(list "y3" "drawing"))
        temp7  = (setof s powerCV->shapes s->lpp==(list "y7" "drawing"))
        (dbLayerTile powerCV MetalBlock[2] power2)
        (dbLayerTile powerCV MetalBlock[4] power4)
        (dbLayerTile powerCV MetalBlock[6] power6)
        (dbLayerTile powerCV MetalBlock[8] power8)

        ; delete temporary layers
        (foreach s power2 (dbDeleteObject s))
        (foreach s power4 (dbDeleteObject s))
        (foreach s power6 (dbDeleteObject s))
        (foreach s power8 (dbDeleteObject s))
        (foreach s temp3  (dbDeleteObject s))
        (foreach s temp7  (dbDeleteObject s))
        (dbSave powerCV)
        )
      )

    ; map block purpose layers to power grid cell names
    pgMap=(makeTable "POWER_GRID" nil)
    (for m 2 (max 2 topMetal-1)
         gridName = "globals.POWER_GRID.m"
         (for n m topMetal
              gridName = (strcat gridName (sprintf nil "%d" n))
              )
         pgMap[Metal[m]] = (strcat gridName "_pg")
         )

    ; replace block shapes with power mosaics
    count=0
    (foreach lpp powerCV->lpps
      gridName = pgMap[lpp->layerName]
      (when gridName && lpp->purpose=="block"
        gridCV = (dbOpenCellViewByType "globals" gridName "layout")
        (foreach shape lpp->shapes
          x1 = (leftEdge   shape->bBox)
          y1 = (bottomEdge shape->bBox)
          x2 = (rightEdge  shape->bBox)
          y2 = (topEdge    shape->bBox)
          cols = (round (x2-x1)/(car align))
          rows = (round (y2-y1)/(cadr align))
          (dbCreateSimpleMosaic powerCV gridCV
                                (sprintf nil "pg%d" count) x1:y1 "R0"
                                rows cols (cadr align) (car align))
          count = count+1
          (dbDeleteObject shape)
          )
        )
      )

    ; create prBoundary, power pins, and instantiate power grid
    (dbCreatePRBoundary powerCV CV->prBoundary->points)
    (dbCreatePolygon powerCV (list Metal[topMetal] "block") CV->prBoundary->points)
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

    ; delete temporary shapes
    (dbDeleteObject prb)
    (foreach x blks (dbDeleteObject x))

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
  (let (TEMP inst pgCV pgaCV AssuraLayerMappings figsVdd figsGND)
    
    ; HACK: minimize number of pin shapes
    powerLayers = (cons Metal[topMetal] powerLayers)

    ; find powergrid CV
    inst = (dbFindAnyInstByName CV "pg")
    (unless inst (error "No pg instance"))
    pgCV = inst->master

    ; run Assura makePowerGridAbstract.rul
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    AssuraLayerMappings = (list
                           (list "m2v" (list Metal[2] "vdd"))
                           (list "m2g" (list Metal[2] "gnd"))
                           (list "m2f" (list Metal[2] "fill"))
                           (list "m3v" (list Metal[3] "vdd"))
                           (list "m3g" (list Metal[3] "gnd"))
                           (list "m3f" (list Metal[3] "fill"))
                           (list "m4v" (list Metal[4] "vdd"))
                           (list "m4g" (list Metal[4] "gnd"))
                           (list "m4f" (list Metal[4] "fill"))
                           (list "m5v" (list Metal[5] "vdd"))
                           (list "m5g" (list Metal[5] "gnd"))
                           (list "m5f" (list Metal[5] "fill"))
                           (list "m6v" (list Metal[6] "vdd"))
                           (list "m6g" (list Metal[6] "gnd"))
                           (list "m6f" (list Metal[6] "fill"))
                           (list "m7v" (list Metal[7] "vdd"))
                           (list "m7g" (list Metal[7] "gnd"))
                           (list "m7f" (list Metal[7] "fill"))
                           (list "m8v" (list Metal[8] "vdd"))
                           (list "m8g" (list Metal[8] "gnd"))
                           (list "m8f" (list Metal[8] "fill"))
                           )
    (when useAssura ; BUG: Assura isn't working
      pgaCV = (AssuraLayerProcessor AssuraLayerMappings "makePowerGridAbstract"
                                    pgCV->libName pgCV->cellName "abstract" ?CV CV)
      )
    (when useICV ; NOTE not well tested yet?
      pgaCV = (ICVLayerProcessorSimple AssuraLayerMappings "mergeShape"
                                       pgCV->libName pgCV->cellName "abstract"
                                       ?CV pgCV
                                       ?inputLayerMap AssuraLayerMappings)
      )
    (when !useAssura && !useICV
      ; BUG: work-around
      pgaCV=(betterCopyCellView pgCV pgCV->libName pgCV->cellName "abstract" nil nil t)
      (foreach inst pgaCV->instances (dbFlattenInst inst 32))
      (foreach label (setof x pgaCV->shapes x->objType=="label") (dbDeleteObject label))
      (foreach shape pgaCV->shapes (when shape->purpose=="block" (dbDeleteObject shape)))
      (dbSave pgaCV)
      )
    
    ; open power grid abstract
    pgaCV = (dbOpenCellViewByType pgCV->libName pgCV->cellName "abstract" "maskLayout" "a")
    (unless pgaCV (error "Unable to read %s abstract\n" pgCV->cellName))

    ; merge shapes (too damn slow)
    ; (leMergeShapes pgaCV->shapes)
    
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
(defun GetNonZeroMod ( div dis )
  (mod (round div/MfgGrid) (round dis/MfgGrid)) != 0
  )

; Check vertices are on powergrid pitch
(defun CheckOffGrid ( pt align )
  (GetNonZeroMod (car pt) (car align)) || (GetNonZeroMod (cadr pt) (cadr align))
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
  (let (pgaCV lpp new_shape figsVdd figsGND)
    (DeletePinsByType "Power" ?CV CV)
    pgaCV = (dbOpenCellViewByType CV->libName (strcat CV->cellName suffix) "abstract")
    (unless pgaCV (error "No pg abstract view"))
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
  t
  )


; copy block purpose shapes from subcells
; TODO: use ICV instead
(defun FlattenBlockPurpose (CV dstCV)
  (let (pg inst_trans pg_trans)
    (foreach inst CV->instances
             inst_trans = (dbGetInstTransform inst)
             (foreach shape (setof s inst->master->shapes s->purpose=="block")
                      (dbCopyShape shape dstCV inst_trans)
                      )
             pg = (dbFindAnyInstByName inst->master "pg")
             (when pg
               pg_trans = (dbGetInstTransform pg)
               (foreach shape (setof s pg->master->shapes s->purpose=="block")
                        (dbCopyShape shape dstCV (dbConcatTransform inst_trans pg_trans))
                        )
               )
             )
    )
  t
  )
