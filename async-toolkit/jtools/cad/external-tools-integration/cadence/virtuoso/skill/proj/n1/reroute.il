; scripts to reroute everything in N1 (above fragment level)

(defun RouteCell (cellName topMetal @key (redoPG nil) (colorGrid nil))
  (let (lCV pgCV aCV fCV cgCV libName)
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
    lCV = (Route ?CV fCV ?topMetal topMetal 
                 ?hookupLaygen nil ?Mem 16384 ?metalFill t)
    (FixBDC ?CV lCV)
    (when redoPG
      (MakePowerGrid ?CV lCV ?topMetal 8)
      (MakePowerGridAbstract ?CV lCV ?topMetal 8)
      (CreatePowerPins ?topMetal 8 ?CV lCV)
      )
    (when colorGrid (MakeColorGrid ?CV lCV))
    (MakeSimpleAbstract ?CV lCV ?topMetal topMetal)
    (cdsp4add ?CV lCV)
    pgCV = (dbOpenCellViewByType libName (strcat cellName "_pg") "layout")
    aCV  = (dbOpenCellViewByType libName cellName "abstract")
    (when pgCV (cdsp4add ?CV pgCV))
    (when aCV  (cdsp4add ?CV aCV))
    lCV
    )
  )

(defun RouteLeaves ()
  ; BD controllers
  (RouteCell "lib.bd.standard.pipeline.BD_CTRL.0" 2)
  (RouteCell "lib.bd.standard.pipeline.BD_TOK_CTRL.0" 2)
  (RouteCell "lib.bd.standard.pipeline.BD_ISOLATE_CTRL.0" 2)
  (RouteCell "lib.bd.standard.pipeline.BD_ISOLATE_TOK_CTRL.0" 2)
  (RouteCell "lib.bd.standard.send.BD_SEND_CTRL.0" 2)
  (RouteCell "lib.bd.standard.receive.BD_RECV_CTRL.0" 2)
  (RouteCell "lib.bd.standard.split.BD_SPLIT_CTRL-L2-R.0" 2)
  (RouteCell "lib.bd.standard.split.BD_ALT_SPLIT_CTRL-L2-R.0" 2)
  (RouteCell "lib.bd.standard.merge.BD_MERGE_CTRL-L2-R.0" 2)
  (RouteCell "lib.bd.standard.merge.BD_ALT_MERGE_CTRL-L2-R.0" 2)
  (RouteCell "lib.bd.metastable.arbiter.BD_ARBITER_CTRL.0" 2)

  ; new BD controllers
  (RouteCell "lib.bd.standard.pipeline.CTRL.0" 2)
  (RouteCell "lib.bd.standard.pipeline.TOK_CTRL.0" 2)
  (RouteCell "lib.bd.standard.pipeline.ISOLATE_CTRL.0" 2)
  (RouteCell "lib.bd.standard.pipeline.ISOLATE_TOK_CTRL.0" 2)
  (RouteCell "lib.bd.standard.send.SEND_CTRL.0" 2)
  (RouteCell "lib.bd.standard.receive.RECV_CTRL.0" 2)
  (RouteCell "lib.bd.standard.split.SPLIT_CTRL-L2-R.0" 2)
  (RouteCell "lib.bd.standard.split.ALT_SPLIT_CTRL-L2-R.0" 2)
  (RouteCell "lib.bd.standard.merge.MERGE_CTRL-L2-R.0" 2) 
  (RouteCell "lib.bd.standard.merge.ALT_MERGE_CTRL-L2-R.0" 2)
  (RouteCell "lib.bd.metastable.arbiter.ARBITER_CTRL.0" 2)

  ; small leaf cells
  (RouteCell "lib.bd.6T.bank.BD_SRAM_CTRL.0" 2)
  (RouteCell "lib.bd.6T.bank.SRAM_BANK_CTRL.0" 3)
  (RouteCell "core.noc.router.forward.FILTER_B2A_POSEDGE_1of_CTRL.1000" 2)
  (RouteCell "core.noc.crossbar.datapath.TAIL_CONTROL.1000" 2)
  (RouteCell "core.noc.crossbar.arbiter.SLACKLESS_ARBITER2.1000" 3)
  (RouteCell "core.io.fpio.FPIO.1000" 4)
  (RouteCell "lib.qdi.serial.funny_register.CONFIG_SHIFT_FIFO-L16_20730-R.0" 3)
  (RouteCell "lib.qdi.serial.funny_register.CONFIG_SHIFT_FIFO-L16_20598-R.0" 3)
  (RouteCell "lib.qdi.serial.funny_register.CONFIG_SHIFT_FIFO-L16_65535-R.0" 3)
  t
  )

(defun RouteTest ()
  (RouteCell "lib.bd.6T.bank.ROUTED_SRAM_BANK-L512_24-R.1000" 6)
  )

(defun RouteNoc ()
  (RouteCell "core.neuro.tile.E_CABLE.1000" 6 ?redoPG ?colorGrid t) 
  (RouteCell "core.neuro.tile.N_CABLE.1000" 6 ?redoPG ?colorGrid t)
  (RouteCell "core.neuro.tile.HALF_N_CABLE.1000" 6 ?redoPG ?colorGrid t)
  (RouteCell "core.noc.router.forward.MESH_FORWARD-L8_16_6_6_2_1_true_true_true_true-R.1000" 4)
  (RouteCell "core.noc.router.MESH_ROUTER-L8_8_16_6_6_2_true_true_true_true-R.1000" 6)
  (RouteCell "core.neuro.tile.DOUBLE_ROUTER.1000" 8 ?colorGrid t)
  t
  )

(defun RouteTopCells ()
  (RouteCell "chip.n1.pads.PADS.10000" 8 ?colorGrid t)
  (RouteCell "core.neuro.neuron.EMPTY_NEURON.10000" 8 ?colorGrid t)
  t
  )

(defun ExportProteusCell (baseName cellName @key (noPowerFill nil))
  libName = (GetCastLibName cellName)
  (cdsp4edit ?CV (dbOpenCellViewByType libName (strcat baseName "_pg") "layout"))
  (RouteExport ?CV (dbOpenCellViewByType libName baseName "floorplan") ?softMacro t
               ?hookupLaygen nil ?noPowerFill noPowerFill)
  t
  )

(defun ExportProteus ()
  (ExportProteusCell "chip.n1.io.BASE_IO.10000"            "chip.n1.io.IO.10000")
  (ExportProteusCell "core.neuro.neuron.BASE_NEURON.10000" "core.neuro.neuron.NEURON.10000" ?noPowerFill nil)
  t
  )

; Conform check and color overlays were instantiated
(defun VerifyOverlays ()
  (let (err CV cellName libName inst)
    err = ""
    (foreach cellName (list "core.neuro.tile.E_CABLE.1000"
                            "core.neuro.tile.N_CABLE.1000"
                            "core.neuro.tile.DOUBLE_ROUTER.1000"
                            "chip.n1.pads.PADS.10000"
                            "chip.n1.test0.TEST0.10000"
                            "chip.n1.test1.TEST1.10000"
                            "chip.n1.testm.TESTM.10000"
                            "chip.n1.io.IO.10000"
                            "core.neuro.neuron.NEURON.10000")
             libName = (GetCastLibName cellName)
             CV = (dbOpenCellViewByType libName cellName "layout")
             inst = (dbFindAnyInstByName CV "color")
             (unless inst err=(strcat err (sprintf  nil "ERROR: %s missing color overlay\n" cellName)))
             )
    cellName = "chip.n1.N1.10000"
    libName = (GetCastLibName cellName)
    CV = (dbOpenCellViewByType libName cellName "layout")
    inst = (dbFindAnyInstByName CV "check")
    (unless inst err=(strcat err (sprintf nil "ERROR: %s missing check overlay\n" cellName)))
    (unless err=="" (printf err))
    err==""
    )
  )

; make nominal threshold new BD controllers from the UV1 threshold ones
(defun MakeNominalControllers ()
  (foreach cellName (list "lib.bd.standard.pipeline.CTRL.0"
                          "lib.bd.standard.pipeline.TOK_CTRL.0"
                          "lib.bd.standard.pipeline.ISOLATE_CTRL.0"
                          "lib.bd.standard.pipeline.ISOLATE_TOK_CTRL.0"
                          "lib.bd.standard.send.SEND_CTRL.0"
                          "lib.bd.standard.receive.RECV_CTRL.0"
                          "lib.bd.standard.split.SPLIT_CTRL-L2-R.0"
                          "lib.bd.standard.split.ALT_SPLIT_CTRL-L2-R.0"
                          "lib.bd.standard.merge.MERGE_CTRL-L2-R.0" 
                          "lib.bd.standard.merge.ALT_MERGE_CTRL-L2-R.0"
                          "lib.bd.metastable.arbiter.ARBITER_CTRL.0")
           (MakeNominalController cellName)
           )
  t
  )

; make nominal threshold new BD controller from the UV1 threshold one
(defun MakeNominalController (cellName)
  (let (libName CV newName map)
    libName = (GetCastLibName cellName)
    newName = (strcat cellName "n")

    ; edit views
    CV = (dbOpenCellViewByType libName newName "floorplan")
    (when CV (cdsp4edit ?CV CV))
    CV = (dbOpenCellViewByType libName newName "layout")
    (when CV (cdsp4edit ?CV CV))
    CV = (dbOpenCellViewByType libName newName "abstract")
    (when CV (cdsp4edit ?CV CV))
    
    ; copy floorplan view
    CV = (dbOpenCellViewByType libName cellName "floorplan")
    CV = (betterCopyCellView CV libName newName "floorplan" nil nil t)
    (UpdateFloorplan ?CV CV)
    (dbSave CV)
    (cdsp4add ?CV CV)

    ; copy abstract view
    CV = (dbOpenCellViewByType libName cellName "abstract")
    CV = (betterCopyCellView CV libName newName "abstract" nil nil t)
    (dbSave CV)
    (cdsp4add ?CV CV)

    ; replace physical cells with nominal threshold versions
    map = (makeTable "map" nil)
    map["vendor.intel.d04.spc00ln.z01"]="vendor.intel.d04.spc00nn.z01";
    map["vendor.intel.d04.spc00ln.z02"]="vendor.intel.d04.spc00nn.z02";
    map["vendor.intel.d04.spc00ln.z03"]="vendor.intel.d04.spc00nn.z03";
    map["vendor.intel.d04.spc00ln.z04"]="vendor.intel.d04.spc00nn.z04";
    map["vendor.intel.d04.tap02ld.z05"]="vendor.intel.d04.tap02nd.z05";

    ; copy layout view
    CV = (dbOpenCellViewByType libName cellName "layout")
    CV = (betterCopyCellView CV libName newName "layout" nil nil t)
    (UpdateFloorplan ?CV CV)
    (foreach sub CV->instances
             (when map[sub->cellName]
               sub->master = (dbOpenCellViewByType sub->libName map[sub->cellName] sub->viewName)
               )
             )
    (dbSave CV)
    (cdsp4add ?CV CV)
    )
  t
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
