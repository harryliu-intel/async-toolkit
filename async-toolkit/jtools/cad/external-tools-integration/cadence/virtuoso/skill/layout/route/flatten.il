; Replacement for ui/Nano/Flatten/FlattenView.il

; Top-level function to make a flatten view and switch to it
(defun MakeFlatten
  (@key (CV (geGetEditCellView))   ; specify CellView or nil to use edit view
        (RegenerateFromCast nil)   ; force CAST query
        (Verbose nil)              ; verbose
        (ConnectDanglingPins t)    ; paint metal on top of one-terminal subcell pins
        (ConnectAbuttingPins nil)  ; paint metal between nearby pins on the same net
        (doMakePowerGrid nil)      ; make power grid
        (doPowerGridAbstract nil)  ; generate abstract view of power grid
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
      pgCV = (MakePowerGrid ?CV flattenCV)
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
        inst prb PowerGridSize PGS prbObjPoints prb1 copyprb vertprb vertprbrec
        x1box x2box y1box y2box x1 y1 x2 x2 xy
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

    ; get prBoundary boundary shapes 
    (unless CV->prBoundary->points (error "Bad prBoundary"))
    prbObjPoints = CV->prBoundary->points
    prb1 = (dbCreatePolygon CV (list "prBoundary" "drawing") prbObjPoints)
    copyprb1 = (setof x CV->shapes x->lpp==(list "prBoundary" "drawing"))

    ; checks for prBoundary drawing, that only one exists, and that vertices are on grid
    align = gridAlignment[topMetal]
    (when (length copyprb1)!=1
          (error "ERROR: Check prBoundary layer. Either no prBoundary or more than one exists."))
    copyprb = (car copyprb1)
    vertprb = (setof vertprb copyprb->points (CheckOffGrid vertprb align))
    (when vertprb (error "Not all prBoundary vertices are on grid: %L" vertprb))
    prb = (dbCopyShape copyprb CV)
    prb->lpp = (list "prBoundary" "boundary")

    ; get bounding box as an integer multiple of MfgGrid
    x1box = 1.0*(round (leftEdge   prb->bBox)/MfgGrid)
    x2box = 1.0*(round (rightEdge  prb->bBox)/MfgGrid)
    y1box = 1.0*(round (bottomEdge prb->bBox)/MfgGrid)
    y2box = 1.0*(round (topEdge    prb->bBox)/MfgGrid)

    ; minimum sized power grid
    PowerGridSize = 2*PowerGridPitch
    PGS = (round PowerGridSize/MfgGrid)
    x1 = (floor   x1box/PGS)
    y1 = (floor   y1box/PGS)
    x2 = (ceiling x2box/PGS)
    y2 = (ceiling y2box/PGS)

    ; avoid M2 overlapping prBoundary of subcells (looks only one level down)
    blks = (PaintOverSubcellPrBoundary (list Metal[2] "block") ?CV CV)

    ; find bottom left vertices of prbound to place Vdd/GND pins
    (cond (prb->objType == "polygon" xy = (car (SortVertices prb->points)))
          (t xy = x1box:y1box)
          )

    ; run Assura rules
    AssuraLayerMappings = (list
                           (list "power2" (list Metal[2] "block"))
                           (list "power3" (list Metal[3] "block"))
                           (list "power4" (list Metal[4] "block"))
                           (list "power5" (list Metal[5] "block"))
                           (list "power6" (list Metal[6] "block"))
                           (list "power7" (list Metal[7] "block"))
                           (list "prblk"  (list "prBoundary" "block"))
                           )
    powerCV = (dbOpenCellViewByType CV->libName powerCellName "layout" "maskLayout" "w")
    (dbSave powerCV)
    (SimpleAssuraLayerProcessor AssuraLayerMappings "makePowerGrid" ?CV powerCV)

    ; define power grid cell names
    pgMap=(makeTable "POWER_GRID" nil)
    pgMap["m2"]="globals.POWER_GRID.m2345678_pg"
    pgMap["m3"]="globals.POWER_GRID.m345678_pg"
    pgMap["m4"]="globals.POWER_GRID.m45678_pg"
    pgMap["m5"]="globals.POWER_GRID.m5678_pg"
    pgMap["m6"]="globals.POWER_GRID.m678_pg"
    pgMap["m7"]="globals.POWER_GRID.m78_pg"
    pgMap["m8"]="globals.POWER_GRID.m8_pg"

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
          cols = (round (x2-x1)/PowerGridSize)
          rows = (round (y2-y1)/PowerGridSize)
          (dbCreateSimpleMosaic powerCV gridCV
                                (sprintf nil "pg%d" count) x1:y1 "R0"
                                rows cols PowerGridSize PowerGridSize)
          count = count+1
          (dbDeleteObject shape)
          )
        )
      )

    ; create prBoundary, power pins, and instantiate power grid
    (dbCreatePRBoundary powerCV CV->prBoundary->points)
    (CreatePowerPins powerCV "M8" xy t)
    (CreatePowerPins CV      "M8" xy t)
    tiehilo = (dbCreateInst CV powerCV "pg" 0:0 "R0")

    ; finish up power grid and pins
    (AnchorInstance tiehilo)
    GND = (dbFindNetByName CV GNDNetName)
    Vdd = (dbFindNetByName CV VddNetName)
    instGND = (dbFindTermByName powerCV GNDNetName)
    instVdd = (dbFindTermByName powerCV VddNetName)
    instGND = (dbFindTermByName powerCV "GND")
    instVdd = (dbFindTermByName powerCV "Vdd")
    (dbCreateInstTerm GND tiehilo instGND)
    (dbCreateInstTerm Vdd tiehilo instVdd)

    ; delete temporary shapes
    (dbDeleteObject prb)
    (dbDeleteObject prb1)
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
        (powerLayers (list "M2" "M3" "M8"))) ; which layers to keep as pins
  (let (inst pgCV dd pgaCV points
        AssuraLayerMappings AssuraRuleFile AssuraRunLog figsVdd figsGND)

    ; fing powergrid CV
    inst = (dbFindAnyInstByName CV "pg")
    (unless inst (error "No pg instance"))
    pgCV = inst->master

    ; run Assura makePowerGridAbstract.rul
    AssuraLayerMappings = (list
                           (list "prbleaf" (list "M1" "boundary")) ; poly fill
                           (list "m2v" (list "M2" "vdd"))
                           (list "m2g" (list "M2" "gnd"))
                           (list "m3v" (list "M3" "vdd"))
                           (list "m3g" (list "M3" "gnd"))
                           (list "m4v" (list "M4" "vdd"))
                           (list "m4g" (list "M4" "gnd"))
                           (list "m5v" (list "M5" "vdd"))
                           (list "m5g" (list "M5" "gnd"))
                           (list "m6v" (list "M6" "vdd"))
                           (list "m6g" (list "M6" "gnd"))
                           (list "m7v" (list "M7" "vdd"))
                           (list "m7g" (list "M7" "gnd"))
                           (list "m8v" (list "M8" "vdd"))
                           (list "m8g" (list "M8" "gnd"))
                           )
    AssuraRuleFile = (sprintf nil "%s/share/Fulcrum/cell_automation/%s"
                              (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
                              "makePowerGridAbstract.rul")
    AssuraRunLog = (sprintf nil "%s/%s.abstract_pg.log"
                            (ConfigFileGetValue TheCDSConfigTable "TEMP")
                            pgCV->cellName)
    (AssuraRunAssuraLayerProcessor
     pgCV
     pgCV->libName pgCV->cellName "abstract"
     AssuraRuleFile
     (ConfigFileGetValue TheCDSConfigTable "TEMP")
     AssuraLayerMappings
     nil
     ?AssuraRunLog AssuraRunLog
     ?LeaveMess nil
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

    ; create placement blockage object over prBoundary leaf of fill cells
    (foreach shape (setof s pgaCV->shapes s->lpp==(list "M1" "boundary"))
      points = (cond (shape->objType=="path" (dbConvertPathToPolygon shape)->points)
                     (shape->objType=="rect" (RectGetPolygonPoints shape->bBox))
                     (t shape->points)
                     )
      (dbCreateAreaBlockage CV points) ; in flatten view directly
      )

    ; convert remaining shapes to boundary
    (foreach shape (setof shape pgaCV->shapes !(member shape->layerName powerLayers))
             shape->purpose="boundary"
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
(defun CreatePowerPins (CV layer xy label)
  (let (x y fig net)
    x = (car xy)
    y = (cadr xy)

    ; GND pin
    net = (MakeNet CV GNDNetName)
    fig = (dbCreateRect CV (list layer "gnd")
                        (list x-0.09:y+3.12-0.09 x+0.09:y+3.12+0.09))
    (dbReplaceProp fig "PinType" "string" "Power")
    (dbCreatePin net fig)
    (when label (LabelPin CV fig))
    
    ; Vdd pin
    net = (MakeNet CV VddNetName)
    fig = (dbCreateRect CV (list layer "vdd")
                         (list x-0.09:y+1.56-0.09 x+0.09:y+1.56+0.09))
    (dbReplaceProp fig "PinType" "string" "Power")
    (dbCreatePin net fig)
    (when label (LabelPin CV fig))
    )
  t
  )
