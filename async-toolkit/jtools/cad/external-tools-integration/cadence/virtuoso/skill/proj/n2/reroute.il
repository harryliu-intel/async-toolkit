; scripts to reroute everything in N2 (above fragment level)

(defun RouteCell (cellName topMetal @key (redoPG nil) (colorGrid t) (importOnly nil) (timingDriven t))
  (let (lCV pgCV aCV fCV cgCV libName fillCV bbox h w)
    libName = (GetCastLibName cellName)
    fCV  = (dbOpenCellViewByType libName cellName "floorplan")
    lCV  = (dbOpenCellViewByType libName cellName "layout")
    cgCV = (dbOpenCellViewByType libName (strcat cellName "_color") "layout")
    pgCV = (dbOpenCellViewByType libName (strcat cellName "_pg") "layout")
    aCV  = (dbOpenCellViewByType libName cellName "abstract")
    (when lCV  (cdsp4edit ?CV lCV))
    (when pgCV (cdsp4edit ?CV pgCV))
    (when cgCV (cdsp4edit ?CV cgCV))
    (when aCV  (cdsp4edit ?CV aCV))
    lCV = (if importOnly
            (RouteImport ?CV fCV)
            (Route ?CV fCV ?topMetal topMetal ?timingDriven timingDriven ?colorGrid colorGrid ?Mem 16384)
            )
    (when redoPG
      (MakePowerGrid ?CV lCV ?topMetal 8)
      (MakePowerGridAbstract ?CV lCV ?topMetal 8)
      (CreatePowerPins ?topMetal 8 ?CV lCV)
      (when topMetal==6
        fillCV = (dbOpenCellViewByType "globals" "globals.FILL_GRID.m78_fill" "layout")
        bbox = lCV->prBoundary->bBox
        h = (cadr (cadr bbox)) - (cadr (car bbox))
        w = (car (cadr bbox)) - (car (car bbox))
        h = (round h/(cadr gridAlignment[8]))
        w = (round w/(car  gridAlignment[8]))
        (dbCreateSimpleMosaic lCV fillCV "fill" (car bbox) "R0"
                              h w (cadr gridAlignment[8]) (car gridAlignment[8]))
        (dbSave lCV)
        )
      )
    aCV = (MakeSimpleAbstract ?CV lCV ?topMetal topMetal)
    pgCV = (dbOpenCellViewByType libName (strcat cellName "_pg") "layout")
    cgCV = (dbOpenCellViewByType libName (strcat cellName "_color") "layout")
    (when lCV (cdsp4add ?CV lCV))
    (when pgCV (cdsp4add ?CV pgCV))
    (when aCV  (cdsp4add ?CV aCV))
    (when cgCV (cdsp4add ?CV cgCV))
    lCV
    )
  )

(defun RouteLeaf (cellName topMetal)
  (RouteCell cellname topMetal ?timingDriven nil ?colorGrid nil)
  )

(defun ExportProteusCell (baseName)
  (let (libName fpCV pgCV cgCV)
    libName = (GetCastLibName baseName)
    fpCV  = (dbOpenCellViewByType libName baseName "floorplan")
    pgCV = (dbOpenCellViewByType libName (strcat baseName "_pg") "layout")
    cgCV = (dbOpenCellViewByType libName (strcat baseName "_color") "layout")
    (when pgCV (cdsp4edit ?CV pgCV))
    (when cgCV (cdsp4edit ?CV cgCV))
    (RouteExport ?CV fpCV ?softMacro t)
    pgCV = (dbOpenCellViewByType libName (strcat baseName "_pg") "layout")
    cgCV = (dbOpenCellViewByType libName (strcat baseName "_color") "layout")
    (when pgCV (cdsp4add ?CV pgCV))
    (when cgCV (cdsp4add ?CV cgCV))
    t
    )
  )

(defun ImportCell (cellName)
  (let (libName lCV)
    libName = (GetCastLibName cellName)
    lCV = (dbOpenCellViewByType libName cellName "layout")
    (when lCV (cdsp4edit ?CV lCV))
    lCV = (RouteImport ?libName libName ?cellName cellName)
    (cdsp4add ?CV lCV)
    t
    )
  )

; replace bdc01's with our custom versions
(defun FixBDC (@key (CV (geGetEditCellView)))
  (let (old4 old8 new4 new8 n mapInstance)
    n=0
    old4 = (dbOpenCellViewByType "vendor.intel.d04" "vendor.intel.d04.bdc01ld.z04" "layout")
    old8 = (dbOpenCellViewByType "vendor.intel.d04" "vendor.intel.d04.bdc01ld.z08" "layout")
    new4 = (dbOpenCellViewByType "globals" "globals.bdc01ld.z04" "layout")
    new8 = (dbOpenCellViewByType "globals" "globals.bdc01ld.z08" "layout")
    mapInstance = (makeTable "mapInstance" nil)
    mapInstance[old4]=new4
    mapInstance[old8]=new8
    (foreach inst CV->instances
             (when mapInstance[inst->master] inst->master=mapInstance[inst->master] n++)
             )
    n
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Route Cells ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun RouteLeaves ()
  ; new BD controllers
  (RouteLeaf "lib.bd.source.bitgen.BDC_BG.0" 2)
  (RouteLeaf "lib.bd.standard.pipeline.CTRL.0" 2)
  (RouteLeaf "lib.bd.standard.pipeline.TOK_CTRL.0" 2)
  (RouteLeaf "lib.bd.standard.pipeline.ISOLATE_CTRL.0" 2)
  (RouteLeaf "lib.bd.standard.pipeline.ISOLATE_TOK_CTRL.0" 2)
  (RouteLeaf "lib.bd.standard.send.SEND_CTRL.0" 2)
  (RouteLeaf "lib.bd.standard.receive.RECV_CTRL.0" 2)
  (RouteLeaf "lib.bd.standard.split.SPLIT_CTRL-L2-R.0" 2)
  (RouteLeaf "lib.bd.standard.split.ALT_SPLIT_CTRL-L2-R.0" 2)
  (RouteLeaf "lib.bd.standard.merge.MERGE_CTRL-L2-R.0" 2)
  (RouteLeaf "lib.bd.standard.merge.ALT_MERGE_CTRL-L2-R.0" 2)
  (RouteLeaf "lib.bd.metastable.arbiter.ARBITER_CTRL.0" 2)

  ; small leaf cells
  (RouteLeaf "lib.bd.6T.bank.BD_SRAM_CTRL.0" 2)
  (RouteLeaf "lib.bd.6T.bank.SRAM_BANK_CTRL.0" 3)
  (RouteLeaf "core.noc.crossbar.datapath.TAIL_CONTROL.1000" 2)
  (RouteLeaf "lib.qdi.metastable.slackless_arbiter.SLACKLESS_ARBITER2.20000" 2)
  (RouteLeaf "core.io.fpio.FPIO.20000" 4)

  ; a bunch of SHIFT_FIFOs
  (for w 8 16
       (RouteLeaf (sprintf nil "lib.qdi.serial.funny_register.SHIFT_FIFO-L%d_0-R.0"  w) 3)
       (RouteLeaf (sprintf nil "lib.qdi.serial.funny_register.SHIFT_FIFO-L%d_0-R.0h" w) 3)
       )
  t
  )

(defun RouteTest ()
  (RouteLeaf "lib.bd.6T.bank.ROUTED_SRAM_BANK-L512_24-R.1000" 6)
  )

(defun RouteMid ()
  ; cables
  (RouteCell "core.neuro.tile.E_CABLE.20000" 6 ?redoPG t)
  (RouteCell "core.neuro.tile.N_CABLE.20000" 6 ?redoPG t)
  (RouteCell "core.neuro.tile.N_CABLE_TALL.20000" 6 ?redoPG t)
  (RouteCell "core.neuro.tile.N_CABLE_1ROW.20000" 6 ?redoPG t)
  (RouteCell "core.neuro.tile.N_CABLE_MC.20000" 6 ?redoPG t)
  (RouteCell "core.neuro.tile.N_CABLE_MC_TALL.20000" 6 ?redoPG t)
  (RouteCell "chip.n2.south.GO_WEST.20000" 6 ?redoPG t ?timingDriven nil) ; BUG: timingDriven
  (RouteCell "chip.n2.south.BYPASS.20000" 6 ?redoPG t)
  (RouteCell "chip.n2.pio.PIO_CABLE.20000" 6 ?redoPG t)

  ; MESH_ROUTERs and DOUBLE_ROUTER
  (RouteCell "core.noc.router.forward.MESH_FORWARD-L8_16_5_5_2_1_true_true_true_true-R.20000" 4 ?colorGrid nil)
  (RouteCell "core.noc.router.forward.MESH_FORWARD-L5_16_7_7_0_1_true_true_true_true-R.20001" 4 ?colorGrid nil)
  (RouteCell "core.noc.router.MESH_ROUTER-L8_8_16_5_5_2_true_true_true_true-R.20000" 6 ?colorGrid nil)
  (RouteCell "core.noc.router.MESH_ROUTER-L5_5_16_7_7_0_true_true_true_true-R.20001" 6 ?colorGrid nil)
  (RouteCell "core.neuro.tile.DOUBLE_ROUTER.20000" 8)

  ; SOUTH/NORTH
  (RouteCell "chip.n2.pads.PADS.20000" 8 ?timingDriven nil) ; BUG: timingDriven
  (RouteCell "chip.n2.pads.PADS_IN_BD-L9-R.20000" 8)
  (RouteCell "chip.n2.pads.PADS_OUT_BD-L9-R.20000" 8)
  t
  )

; list of APR blocks excluding NEURON
apr_cells = (list
             (list "core.neuro.bridge.BASE_BRIDGE.20000"  "core.neuro.bridge.BRIDGE.20000")
             (list "chip.n2.io.BASE_IO.20000"             "chip.n2.io.IO.20000")
             (list "chip.n2.test0.BASE_TEST0.20000"       "chip.n2.test0.TEST0.20000")
             (list "chip.n2.test1.BASE_TEST1.20000"       "chip.n2.test1.TEST1.20000")
             (list "chip.n2.lakemont.BASE_CPU_SHIM.20000" "chip.n2.lakemont.CPU_SHIM.20000")
             (list "chip.n2.lakemont.LAKEMONT.20000"      "chip.n2.lakemont.LAKEMONT.20000")
             )

; Export all APR blocks except for those in NEURON
(defun ExportProteus ()
  (foreach x apr_cells (ExportProteusCell (car x)))
  t
  )

; Import all APR blocks except for those in NEURON
(defun ImportProteus ()
  (foreach x apr_cells (ImportCell (cadr x)))
  t
  )

; Do chip-assembly steps on a top level layout cell
(defun UpdateTopCell (@key (CV (geGetEditCellView)))
  (let (TopMetal)
    TopMetal = 10
    (UpdateFloorplan ?CV CV ?recurse nil)
    (DeletePins ?CV CV)
    (DeleteAutoGen ?CV CV)
    (LabelBumps ?CV CV)
    (RefreshAllBuswires ?CV CV)
    (DrawMidLevelPins ?CV CV ?include_supply t ?supply_topMetal 8)
    (CanonicalizePins ?CV CV)
    )
  (RouteNearby ?CV CV)
  (dbSave CV)
  CV
  )
