; Replacement for ui/Nano/Flatten/FlattenView.il

useAssura = nil ; BUG: Assura isn't working!

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
        )
  (let (MaxHeapSize FlattenCellsList Components RComponents CellBaseName
        type_sub PowerGridCellName OBSCellName flattenCV xalign yalign
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
    Components = (parseString CV->cellName ".")
    RComponents = (reverse Components)
    CellBaseName = (cadr RComponents)
    (when (length RComponents)<3 (error "Cell name violates CAST conventions\n"))
    type_sub = (sprintf nil "%s_%s" (cadr RComponents) (car RComponents))
    OBSCellName = (strcat CV->libName ".wires." type_sub "_OBS")
    PowerGridCellName = (strcat CV->libName ".wires." type_sub "_POWER_GRID_TIEOFF")

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

    ; Create power grid
    (when doMakePowerGrid
      pgCV = (MakePowerGrid ?CV flattenCV ?topMetal topMetal)
      (unless pgCV (error "Unable to create power grid\n"))
      )

    ; Create power grid abstract
    (when doPowerGridAbstract
      pgCV = (MakePowerGridAbstract ?CV flattenCV)
      (unless pgCV (error "Unable to create power grid abstract\n"))
      )

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
    foreach( inst instances
      if( !member( inst~>cellName uniqueInstances~>cellName ) then
        uniqueInstances=cons( inst uniqueInstances )
      )
    )
    pout=outfile(celllist_filename)
    if( pout then
      foreach( inst uniqueInstances
        fprintf( pout "%s %s %s\n" inst~>libName inst~>cellName "abstract")
      )
      close(pout)
    )

    ; draw pins
    (RedrawAllPins ?CV CV)
    
    ; canonicalize prerouted nets
    (CanonicalizeNets ?CV CV)

    ; warn or delete redundant non-canonical pins
    (DeleteRedundantPins ?CV CV ?warnOnly !delete_redundant_pins)

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

    ; fix pin directions TODO: use existing *.il file in temp
    dirmap = (defGenerateP2DTable CV->cellName nil)
    (when dirmap
      (foreach term CV->terminals
               ; lock pin location
               (foreach pin term->pins
                        (when pin->fig (dbSetPinFigPlacementStatus pin->fig "locked")))
               ; set term direction
               (cond (dirmap[term->name]=="INPUT"  term->direction = "input")
                     (dirmap[term->name]=="OUTPUT" term->direction = "output")
                     (dirmap[term->name]=="INOUT"  term->direction = "inputOutput"))
               )
      )

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
        )
  (let (TEMP powerCellName align
        inst prb vertprb
        x1box x2box y1box y2box x1 y1 x2 x2 xy
        AssuraLayerMappings
        powerCV pgMap blks power2
        count gridName gridCV rows cols
        tiehilo GND Vdd instGND instVdd)

    ; generated cell or file names
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    powerCellName = (strcat CV->cellName "_pg")

    ; delete old power grid instance if it exists
    inst = (dbFindAnyInstByName CV "pg")
    (when inst (dbDeleteObject inst))

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

    ; find bottom left vertices of prbound to place Vdd/GND pins
    (cond (prb->objType=="polygon" xy = (car (SortVertices prb->points)))
          (t xy = x1box:y1box)
          )

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
                                      CV->libName powerCellName "layout" ?CV CV)
      )
    (when !useAssura
      ; BUG: temporary hack to avoid Assura layer processing
      ; Uses prBoundary shape but ignores "block" purpose layers
      powerCV = (dbOpenCellViewByType CV->libName powerCellName "layout" "maskLayout" "w")
      power2 = (dbCreatePolygon powerCV (list Metal[2] "block") prb->points)
      (dbLayerTile powerCV (list Metal[2] "block") (list power2))
      (dbDeleteObject power2)
      (dbSave powerCV)
      )

    ; define power grid cell names
    pgMap=(makeTable "POWER_GRID" nil)
    (for m 2 topMetal
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
        gridCV = (nrOpenCellViewReadable "globals" gridName "layout")
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
    (dbCreateTerm (MakeNet powerCV GNDNetName) GNDNetName "inputOutput")
    (dbCreateTerm (MakeNet powerCV VddNetName) VddNetName "inputOutput")
    (CreatePowerPins CV xy topMetal)
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

    ; save and return
    (dbSave CV)
    (dbSave powerCV)
    (dbClose powerCV)
    powerCV
    )
  )

; generate abstract of a power grid cell
; flattens and merges vdd and gnd purpose shapes
; converts those on specified layers to pins with strong connectivity
; others layers become boundary purpose
(defun MakePowerGridAbstract
  (@key (CV (geGetEditCellView)) ; flatten CellView
        (powerLayers (list Metal[2] Metal[4] Metal[6] Metal[7] Metal[8]))) ; which layers to keep as pins
  (let (TEMP inst pgCV pgaCV AssuraLayerMappings figsVdd figsGND)

    ; fing powergrid CV
    inst = (dbFindAnyInstByName CV "pg")
    (unless inst (error "No pg instance"))
    pgCV = inst->master

    ; run Assura makePowerGridAbstract.rul
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    (when useAssura
      ; BUG: Assura isn't working
      AssuraLayerMappings = (list
                             (list "m2v" (list Metal[2] "vdd"))
                             (list "m2g" (list Metal[2] "gnd"))
                             (list "m3v" (list Metal[3] "vdd"))
                             (list "m3g" (list Metal[3] "gnd"))
                             (list "m4v" (list Metal[4] "vdd"))
                             (list "m4g" (list Metal[4] "gnd"))
                             (list "m5v" (list Metal[5] "vdd"))
                             (list "m5g" (list Metal[5] "gnd"))
                             (list "m6v" (list Metal[6] "vdd"))
                             (list "m6g" (list Metal[6] "gnd"))
                             (list "m7v" (list Metal[7] "vdd"))
                             (list "m7g" (list Metal[7] "gnd"))
                             (list "m8v" (list Metal[8] "vdd"))
                             (list "m8g" (list Metal[8] "gnd"))
                             )
      pgaCV = (AssuraLayerProcessor AssuraLayerMappings "makePowerGridAbstract"
                                    pgCV->libName pgCV->cellName "abstract" ?CV CV)
      )
    (when !useAssura
      ; BUG: work-around
      pgaCV=(dbCopyCellView pgCV pgCV->libName pgCV->cellName "abstract" nil nil t)
      (foreach inst pgaCV->instances (dbFlattenInst inst 32))
      (foreach label (setof x pgaCV->shapes x->objType=="label") (dbDeleteObject label))
      (leMergeShapes pgaCV->shapes)
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
                    shape->purpose="drawing" ; net?
                    figsVdd = (cons shape figsVdd)
                    )
                   (shape->purpose=="gnd"
                    shape->purpose="drawing" ; net?
                    figsGND = (cons shape figsGND)
                    )
                   )
             )
    (dbAddFigsToPin (dbCreatePin (dbMakeNet pgaCV VddNetName) nil) figsVdd)
    (dbAddFigsToPin (dbCreatePin (dbMakeNet pgaCV GNDNetName) nil) figsGND)

    ; convert remaining shapes to boundary
    (foreach shape (setof shape pgaCV->shapes !(member shape->layerName powerLayers))
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

; Draw Vdd/GND pins
(defun CreatePowerPins (CV xy topMetal)
  (let (pattern)
    (cond (topMetal==2 || topMetal==3 pattern=power_m2)
          (topMetal==4 || topMetal==5 pattern=power_m4)
          (topMetal==6 || topMetal==7 pattern=power_m6)
          (topMetal==8 pattern=power_m8)
          )
    (DrawChannel nil pattern "" (list xy (car xy)+(car gridAlignment[topMetal]):(cadr xy))
                 ?isPin t ?CV CV)
    (foreach shape CV->shapes
             (when shape->purpose=="gnd" || shape->purpose=="vdd"
                   (dbReplaceProp shape "PinType" "string" "Power")
                   (dbReplaceProp shape "BusScriptObject" "boolean" nil)
                   )
             )
    )
  t
  )
