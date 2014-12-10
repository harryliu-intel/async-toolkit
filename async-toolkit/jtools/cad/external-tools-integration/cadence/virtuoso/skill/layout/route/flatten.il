; Replacement for ui/Nano/Flatten/FlattenView.il

; Top-level function to make a flatten view and switch to it
(defun MakeFlatten
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        (RegenerateFromCast nil) ; force CAST query
        (Proteus nil)            ; Proteus compatible POWER_GRID_TIEOFF
        (Verbose nil)            ; verbose
        (ConnectDanglingPins t)  ; paint metal on top of one-terminal subcell pins
        (ConnectAbuttingPins t)  ; paint metal between nearby pins on the same net
        (doFillPoly t)           ; fill poly
        (doMakePowerGrid t)      ; make power grid
        (doPowerGridAbstract t)  ; generate abstract view of power grid
        )
  (let (MaxHeapSize FlattenCellsList Components RComponents CellBaseName
        type_sub PowerGridCellName OBSCellName flattenCV xalign yalign
        pgCV win temp)

    ; check viewName
    (when (and CV->viewName!="floorplan" CV->viewName!="prelayout")
      (error "Wrong view: Call MakePrelayout or MakeFlatten on floorplan/prelayout view\n"))

    ; create prelayout view if necessray
    (when CV->viewName=="floorplan" CV = (MakePrelayout ?CV CV))

    ; save prelayout view
    (dbSave CV)

    ; options
    (LoadWires)
    MaxHeapSize = (nrGetMaxHeapSize)

    ; generated cell or file names
    FlattenCellsList = (strcat CV->cellName ".flatten_cells_list")
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
    (SetXAlignmentProp flattenCV (max (if xalign xalign 0) TrackPitch))
    (SetYAlignmentProp flattenCV (max (if yalign yalign 0) GridPitch))
    (dbSave flattenCV)

    ; Create Poly Fill 
    (when doFillPoly && !Proteus
      polyfillCV = (FillPoly ?CV flattenCV)
      (unless polyfillCV (error "Unable to create poly fill\n"))
    )

    ; Create power grid
    (when doMakePowerGrid
      pgCV = (MakePowerGrid ?CV flattenCV ?Proteus Proteus)
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
    instances= setof( inst CV~>instances inst->libName!=TechLibName && inst->viewName!="polyfill")
    instances= append( car( NameFilterInstances( instances LibCellsToIgnore ))
                       cadr( NameFilterInstances( instances PowerGridCellPairRegExs )))
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
    (RedrawAllPins)

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
      (nrCreateNeighborNetConnector CV 2*KeepoutPrBoundSpacing MetalLPPs ?maxFanout 2)
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
        (Proteus nil) ; include M2 stripes
        )
  (let (Components RComponents type_sub powerCellName
        inst prb PowerGridSize PGS prbObjPoints prb1
        x1box x2box y1box y2box x1 y1 x2 x2 xy
        AssuraLayerMappings AssuraRuleFile AssuraRunLog ErrorStr
        powerCV ReplaceCellNameTable blks
        count gridName gridCV rows cols transform points
        tiehilo GND Vdd instGND instVdd)

    ; generated cell or file names
    Components = (parseString CV->cellName ".")
    RComponents = (reverse Components)
    (when (length RComponents)<3 (error "Cell name violates CAST conventions\n"))
    type_sub = (sprintf nil "%s_%s" (cadr RComponents) (car RComponents))
    powerCellName = (strcat CV->libName ".wires." type_sub "_POWER_GRID_TIEOFF")

    ; delete old power grid instance if it exists
    inst = (dbFindAnyInstByName CV "tiehilo")
    (when inst (dbDeleteObject inst))

    ; get prBoundary boundary shapes 
    prbObjPoints = CV->prBoundary->points
    prb1 = (dbCreatePolygon CV list("prBoundary" "drawing") prbObjPoints)
    copyprb1 = (setof x CV->shapes (car x->lpp) == "prBoundary" && (cadr x->lpp) == "drawing")

    ; checks for prBoundary drawing, if only one exists, if vertices are on 3.12u grid
    (cond ((length copyprb1)==1 (letseq
                ((copyprb (car copyprb1)))
                vertprb = (setof vertprb copyprb->points (CheckOffGrid vertprb))
                vertprbrec = (setof vertprbrec copyprb->bBox (CheckOffGrid vertprbrec))
                (when vertprb (error "Not All prBoundary Vertices Are On 3.12u Grid: %L" vertprb))
                (when vertprbrec (error "Not All prBoundary Vertices Are On 3.12u Grid: %L" vertprbrec))
                prb =  (dbCopyShape copyprb CV )
                prb->lpp=(list "prBoundary" "boundary")))
           (t (error "ERROR: Check prBoundary layer. Either no prBoundary or more than one exists."))) 
           
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
    (when Proteus blks = (PaintOverSubcellPrBoundary (list "M2" "block") ?CV CV))

    ; avoid M2 power grid everywhere
    (unless Proteus blks = (list (dbCreatePolygon CV (list "M2" "block") (GetPrboundPoints CV))))

    ; find bottom vertices of prbound to place Vdd/GND pins
    (if prb->objType == "polygon"
        (letseq ()
                botprbverts = (setof botprbverts prb->points
                                     (CheckBotVert botprbverts ((ceiling y1box/PGS)*PowerGridSize)))
        xy = (car botprbverts))
        xy = (list (ceiling x1box/PGS)*PowerGridSize
                   (ceiling y1box/PGS)*PowerGridSize)
        )

    ; run layer processing
    AssuraLayerMappings = (list
                           (list "power2" (list "M2" "block"))
                           (list "power3" (list "M3" "block"))
                           (list "power4" (list "M4" "block"))
                           (list "power5" (list "M5" "block"))
                           (list "power6" (list "M6" "block"))
                           (list "power7" (list "M7" "block"))
                           (list "prblk"  (list "prBoundary" "block"))
                           )
    AssuraRuleFile = (sprintf nil "%s/share/Fulcrum/cell_automation/%s"
                              (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
                              "makePowerGrid.rul")
    AssuraRunLog = (sprintf nil "%s/%s.power_grid.log"
                            (ConfigFileGetValue TheCDSConfigTable "TEMP")
                            CV->cellName)
    ErrorStr = (AssuraRunAssuraLayerProcessor
                CV CV->libName powerCellName "layout"
                AssuraRuleFile
                (ConfigFileGetValue TheCDSConfigTable "TEMP")
                AssuraLayerMappings
                nil
                ?AssuraRunLog AssuraRunLog
                ?LeaveMess nil 
                )
    powerCV = (nrOpenCellViewWritable CV->libName powerCellName "layout")

    ; define power grid cell names
    ReplaceCellNameTable=makeTable("a" "")
    ReplaceCellNameTable["M2"]="globals.wires.POWER_GRID_M2345678"
    ReplaceCellNameTable["M3"]="globals.wires.POWER_GRID_M345678"
    ReplaceCellNameTable["M4"]="globals.wires.POWER_GRID_M45678"
    ReplaceCellNameTable["M5"]="globals.wires.POWER_GRID_M5678"
    ReplaceCellNameTable["M6"]="globals.wires.POWER_GRID_M678"
    ReplaceCellNameTable["M7"]="globals.wires.POWER_GRID_M78"

    ; replace block shapes with power mosaics
    count=0
    (foreach lpp powerCV->lpps
      gridName = ReplaceCellNameTable[lpp->layerName]
      (when gridName!="" && lpp->purpose=="block"
        gridCV = (nrOpenCellViewReadable "globals" gridName "layout")
        (foreach shape lpp->shapes
          x1 = (leftEdge   shape->bBox)
          y1 = (bottomEdge shape->bBox)
          x2 = (rightEdge  shape->bBox)
          y2 = (topEdge    shape->bBox)
          cols = (round (x2-x1)/PowerGridSize)
          rows = (round (y2-y1)/PowerGridSize)
          (dbCreateSimpleMosaic powerCV gridCV
                                (sprintf nil "I%d" count) x1:y1 "R0"
                                rows cols PowerGridSize PowerGridSize)
          count = count+1
          (dbDeleteObject shape)
          )
        )
      )

    ; copy prBoundary object to powergrid view
    (dbCreatePRBoundary powerCV CV->prBoundary->points)

    ; delete temporary prBoundary shape
    (dbDeleteObject prb)
    (dbDeleteObject prb1)

    ; delete temporary leaf M2 block shapes from flatten CV
    (foreach x blks (dbDeleteObject x))

    ; create power pins
    (CreatePowerPins powerCV "M8" xy t)
    (CreatePowerPins CV      "M8" xy t)

    ; instantiate power grid and connect power pins
    tiehilo = (dbCreateInst CV powerCV "tiehilo" 0:0 "R0")
    (AnchorInstance tiehilo)
    GND = (dbFindNetByName CV GNDNetName)
    Vdd = (dbFindNetByName CV VddNetName)
    instGND = (dbFindTermByName powerCV GNDNetName)
    instVdd = (dbFindTermByName powerCV VddNetName)
    (dbCreateInstTerm GND tiehilo instGND)
    (dbCreateInstTerm Vdd tiehilo instVdd)

    ; save and return
    (dbSave CV)
    (dbSave powerCV)
    (dbClose powerCV)
    powerCV
    )
  )

; Fill grided poly between custom leaf cells
(defun FillPoly
  (@key (CV (geGetEditCellView)) ; select CV
        )
  (let (Components RComponents type_sub polyCellName
        inst prb PowerGridSize PGS prbObjPoints prb1
        x1box x2box y1box y2box x1 y1 x2 x2 xy
        AssuraLayerMappings AssuraRuleFile AssuraRunLog ErrorStr
        polyCV blks
        count gridName gridCV rows cols leafcell leafbbox 
        polyfill XdirGridSize PolyGridSize PolyColSize)

    ; generated cell or file names
    Components = (parseString CV->cellName ".")
    RComponents = (reverse Components)
    (when (length RComponents)<3 (error "Cell name violates CAST conventions\n"))
    type_sub = (sprintf nil "%s_%s" (cadr RComponents) (car RComponents))
    polyCellName = (CV->cellName)

    ; delete old poly grid instance if it exists
    inst = (dbFindAnyInstByName CV "polyfill")
    (when inst (dbDeleteObject inst))

    ; get prBoundary boundary shapes 
    prbObjPoints = CV->prBoundary->points
    prb1 = (dbCreatePolygon CV list("prBoundary" "drawing") prbObjPoints)
    copyprb1 = (setof x CV->shapes (car x->lpp)=="prBoundary"&&(cadr x->lpp)=="drawing")

    ; checks for prBoundary drawing, if only one exists, if vertices are on 3.12u grid
    (cond ((length copyprb1)==1 (letseq
                ((copyprb (car copyprb1)))
                vertprb = (setof vertprb copyprb->points (CheckOffGrid vertprb))
                vertprbrec = (setof vertprbrec copyprb->bBox (CheckOffGrid vertprbrec))
                (when vertprb (error "Not All prBoundary Vertices Are On 3.12u Grid: %L" vertprb))
                (when vertprbrec (error "Not All prBoundary Vertices Are On 3.12u Grid: %L" vertprbrec))
                prb =  (dbCopyShape copyprb CV )
                prb->lpp=(list "prBoundary" "boundary")))
           (t (error "ERROR: Check prBoundary layer. Either no prBoundary or more than one exists.")))

    ;create prBoundary leaf boxes for fill block
    blks = (PaintOverSubcellPrBoundary (list "prBoundary" "leaf") ?CV CV)
 
    ; minimum sized power grid y-axis
    PowerGridSize = 2*PowerGridPitch

    ; minimum sized poly x-axis grid
    PolyGridSize = 2*0.13

    ; number of polyfill columns per powergrid
    OneCol = 1
    TwoCol = 2
    ThreeCol = 3
    PolyColSize = PowerGridSize/PolyGridSize

    ; run layer processing
    AssuraLayerMappings = (list
                           (list "fillpoly" (list "M7" "block"))
                           )
    AssuraRuleFile = (sprintf nil "%s/share/Fulcrum/cell_automation/%s"
                              (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
                              "fillPoly.rul")
    AssuraRunLog = (sprintf nil "%s/%s.fillpoly.log"
                            (ConfigFileGetValue TheCDSConfigTable "TEMP")
                            CV->cellName)
    ErrorStr = (AssuraRunAssuraLayerProcessor
                CV CV->libName polyCellName "polyfill"
                AssuraRuleFile
                (ConfigFileGetValue TheCDSConfigTable "TEMP")
                AssuraLayerMappings
                nil
                ?AssuraRunLog AssuraRunLog
                ?LeaveMess nil 
                )
    polyCV = (nrOpenCellViewWritable CV->libName polyCellName "polyfill")

    ; replace block shapes with poly mosaics
    count=0
    (foreach lpp polyCV->lpps
      (when lpp->layerName=="M7" && lpp->purpose=="block"
        (foreach shape lpp->shapes
          x1 = (leftEdge   shape->bBox)
          y1 = (bottomEdge shape->bBox)
          x2 = (rightEdge  shape->bBox)
          y2 = (topEdge    shape->bBox)
          cols = (round (x2-x1)/PolyGridSize)
          rows = (round (y2-y1)/PowerGridSize)
          XdirGridSize = PolyGridSize
          (cond (cols >= PolyColSize (let ()
           	   gridCV = (nrOpenCellViewReadable
                             "globals" "globals.wires.POLY_FILL_DCAP" "layout")
                   cols = (round cols/PolyColSize)
                   XdirGridSize = PowerGridSize))

                (cols >= ThreeCol (let ()
                   gridCV = (nrOpenCellViewReadable
                             "globals" "globals.wires.POLY_FILL_S78" "layout")
                   cols = (round cols/ThreeCol)
                   XdirGridSize = 3*PolyGridSize))

                (cols >= TwoCol (let ()
                   gridCV = (nrOpenCellViewReadable
                             "globals" "globals.wires.POLY_FILL_S52" "layout")
                   cols = (round cols/TwoCol)
                   XdirGridSize = 2*PolyGridSize))

                (cols >= OneCol (let ()
                   gridCV = (nrOpenCellViewReadable
                             "globals" "globals.wires.POLY_FILL_S26" "layout")
                   cols = (round cols/OneCol)
                   XdirGridSize = PolyGridSize))
                (t (error))
                )
          (dbCreateSimpleMosaic polyCV gridCV
                                (sprintf nil "I%d" count) x1:y1 "R0"
                                rows cols PowerGridSize XdirGridSize)

          count = count+1
          (dbDeleteObject shape)
          )
        )
      )

    ; copy prBoundary object to poly fill view
    (dbCreatePRBoundary polyCV CV->prBoundary->points)

    ; delete temporary prBoundary shape
    (dbDeleteObject prb)
    (dbDeleteObject prb1)

    ; delete temporary leaf prBoundary shapes 
    (foreach x blks (dbDeleteObject x))

    ; instantiate poly grid 
    polyfill = (dbCreateInst CV polyCV "polyfill" 0:0 "R0")
    (AnchorInstance polyfill)

    ; save and return
    (dbSave CV)
    (dbSave polyCV)
    (dbClose polyCV)
    polyCV
    )
  )

(defun WriteNanoTcl 
  (@key (CV (geGetEditCellView)) ; select CV
        (topLayer 7)
        (bottomLayer 3)
        )
  (let (polyfillinst tclImportFile tclRunFile top bottom instances uniqueInstances pout pout2 Command
        )

    tclImportFile = (strcat CV->cellName ".import.tcl")
    tclRunFile = (strcat CV->cellName ".run.tcl")
    nondefaultDefFile = (strcat CV->cellName "_nondefault.def")
    nondefaultDefLogFile = (strcat CV->cellName "_nondefault.def.log")
    top=topLayer
    bottom=bottomLayer

    NonDefaultLefFile = (sprintf nil "%s/share/Fulcrum/cell_automation/router/%s"
                              (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
                              "nondefault.lef")

    ; delete poly grid instance if it exists
    polyfillinst = (dbFindAnyInstByName CV "polyfill")
    (when polyfillinst (dbDeleteObject polyfillinst))

    instances= setof( inst CV~>instances inst->libName!=TechLibName && inst->viewName!="polyfill")
    uniqueInstances=nil
    foreach( inst instances
      if( !member( inst~>libName uniqueInstances~>libName ) then
        uniqueInstances=cons( inst uniqueInstances )
      )
    )

      Command = sprintf( nil
                "%s/cast2def --cast-path=%s --cell=%s --outfile=%s --cadence-name --max-heap-size=4G &> %s"
                PackageGetBinRoot( )
                ConfigFileGetValue( TheCDSConfigTable "CAST_PATH" )
                CV->cellName 
                nondefaultDefFile 
                nondefaultDefLogFile 
                )
      printf( "%s\n" Command )
      shell( Command )
      printf( "Nondefault Rule File %s is created.\n" nondefaultDefFile)

    pout=outfile(tclImportFile)
    if( pout then
      fprintf(pout "%s%s%s\n" "set rda_Input(ui_oa_designLib) {" CV~>libName "}")
      fprintf(pout "%s%s%s\n" "set rda_Input(ui_oa_designCell) {" CV~>cellName "}")
      fprintf(pout "%s%s%s\n" "set rda_Input(ui_oa_designView) {" CV~>viewName "}")
      fprintf(pout "%s\n" "set rda_Input(ui_oa_abstractname) {abstract}")
      fprintf(pout "%s\n" "set rda_Input(ui_oa_layoutname) {layout}")
      fprintf(pout "%s\n" "set rda_Input(ui_pwrnet) {Vdd}")
      fprintf(pout "%s\n" "set rda_Input(ui_gndnet) {GND}")
      fprintf(pout "%s" "set rda_Input(ui_oa_reflib) {tsmc28")
      foreach( inst uniqueInstances
        fprintf( pout " %s" inst~>libName)
      )
      fprintf(pout "%s\n" "}")
      close(pout)
    )
    printf( "Encounter import file %s is created.\n" tclImportFile)

    pout2=outfile(tclRunFile)
    if( pout2 then
      fprintf(pout2 "%s\n" "setOaxMode -updateMode true")
      fprintf(pout2 "%s %s\n" "loadConfig" tclImportFile)
      fprintf(pout2 "%s %s\n" "loadLefFile" NonDefaultLefFile)
      fprintf(pout2 "%s %s\n" "defIn" nondefaultDefFile)
      fprintf(pout2 "%s%s%s\n" "set TOP \"" CV~>cellName "\"")
      fprintf(pout2 "%s%s%s\n" "set LIB \"" CV~>libName "\"")
      fprintf(pout2 "%s\n" "set VIEW \"nano\"")
      fprintf(pout2 "%s %d\n" "set TOP_LAYER" top)
      fprintf(pout2 "%s %d\n" "set BOTTOM_LAYER" bottom)
      fprintf(pout2 "%s\n" "setNanoRouteMode -quiet -routeAllowPowerGroundPin true")
      fprintf(pout2 "%s\n" "setNanoRouteMode -quiet -routeWithLithoDriven false")
      fprintf(pout2 "%s\n" "setNanoRouteMode -quiet -routeWithSIDriven false")
      fprintf(pout2 "%s\n" "setNanoRouteMode -quiet -routeWithTimingDriven false")
      fprintf(pout2 "%s\n" "setNanoRouteMode -quiet -routeStrictlyHonorNonDefaultRule true")
      fprintf(pout2 "%s\n" "setNanoRouteMode -quiet -routeAutoGgrid false")
      fprintf(pout2 "%s\n" "setNanoRouteMode -quiet -drouteFixAntenna false")
      fprintf(pout2 "%s\n" "setNanoRouteMode -quiet -drouteUseMultiCutViaEffort medium")
      fprintf(pout2 "%s\n" "setNanoRouteMode -quiet -drouteAutoStop true")
      fprintf(pout2 "%s\n" "setNanoRouteMode -quiet -routeTopRoutingLayer    $TOP_LAYER")
      fprintf(pout2 "%s\n" "setNanoRouteMode -quiet -routeBottomRoutingLayer $BOTTOM_LAYER")
      fprintf(pout2 "%s\n" "setAttribute -net GND -skip_routing true")
      fprintf(pout2 "%s\n" "setAttribute -net Vdd -skip_routing true")
      fprintf(pout2 "%s\n" "setNanoRouteMode -quiet -routeSelectedNetOnly false")
      fprintf(pout2 "%s\n" "generateTracks
-m1HOffset 0.065 -m1HPitch 0.13 -m1VOffset 0.065 -m1VPitch 0.13
-m2HOffset 0.065 -m2HPitch 0.13 -m2VOffset 0.065 -m2VPitch 0.13
-m3HOffset 0.065 -m3HPitch 0.13 -m3VOffset 0.065 -m3VPitch 0.13
-m4HOffset 0.065 -m4HPitch 0.13 -m4VOffset 0.065 -m4VPitch 0.13
-m5HOffset 0.065 -m5HPitch 0.13 -m5VOffset 0.065 -m5VPitch 0.13
-m6HOffset 0.065 -m6HPitch 0.13 -m6VOffset 0.065 -m6VPitch 0.13
-m7HOffset 0.065 -m7HPitch 0.13 -m7VOffset 0.065 -m7VPitch 0.13
-m8HOffset 0.065 -m8HPitch 0.13 -m8VOffset 0.065 -m8VPitch 0.13")
      fprintf(pout2 "%s\n" "routeDesign -globalDetail -noPlacementCheck")
      fprintf(pout2 "%s\n" "saveOaDesign $LIB $TOP $VIEW")
      fprintf(pout2 "%s\n" "exit")
      close(pout2)
    )
    printf( "Encounter run file %s is created.\n" tclRunFile)
  )
)

; generate abstract of a power grid cell
; flattens and merges vdd and gnd purpose shapes
; converts those on specified layers to pins with strong connectivity
; others layers become boundary purpose
(defun MakePowerGridAbstract
  (@key (CV (geGetEditCellView)) ; flatten CellView
        (powerLayers (list "M2" "M3" "M8"))) ; which layers to keep as pins
  (let (inst pgCV dd pgaCV AssuraLayerMappings AssuraRuleFile AssuraRunLog figsVdd figsGND)

    ; fing powergrid CV
    inst = (dbFindAnyInstByName CV "tiehilo")
    (unless inst (error "No tiehilo instance"))
    pgCV = inst->master

    ; run Assura makePowerGridAbstract.rul
    AssuraLayerMappings = (list
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

    ; convert remaining shapes to boundary
    (foreach shape (setof shape pgaCV->shapes !(member shape->layerName powerLayers))
             shape->purpose="boundary"
             )
    (CopyKeepoutToBlockage pgaCV)

    ; set fields for LEF export
    (SetLefProperties pgaCV ?powergrid t)

    ; save and return
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
(defun CheckOffGrid ( pt @key (XGrid 2*PowerGridPitch) (YGrid 2*PowerGridPitch) )
  (let ((x (car pt))
        (y (cadr pt)))
   (GetNonZeroMod x XGrid) || (GetNonZeroMod y YGrid)
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


; apply a transform to a list of points
(defun TransformPoints (points transform)
  (when points
    (cons (geTransformUserPoint (car points) transform)
          (TransformPoints (cdr points) transform))
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




defun( CreateMissingAbstractList ( CellView CellsListFileName )
  let((inst cellList fout)
    cellList=nil
    fout=outfile(CellsListFileName)
    if( fout then 
      foreach(inst CellView~>instances
        if(!nrOpenCellViewReadable(inst->libName inst->cellName "abstract") &&
           !member(inst->cellName cellList) then
          cellList=cons(inst~>cellName cellList)
            fprintf(fout "%s\n" inst~>cellName)
        )
      )
      close(fout)
    else printf("Error: file %s not writable\n" CellsListFileName)
    )
  )
)

