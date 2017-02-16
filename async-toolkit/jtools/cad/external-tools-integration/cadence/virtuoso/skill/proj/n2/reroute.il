; scripts to reroute everything in N2 (above fragment level)

(defun RouteCell (cellName topMetal @key (redoPG nil) (colorGrid nil))
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
    lCV = (Route ?CV fCV ?topMetal topMetal 
                 ?hookupLaygen nil ?Mem 16384 ?metalFill t)
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
    (when colorGrid cgCV = (MakeColorGrid ?CV lCV))
    aCV = (MakeSimpleAbstract ?CV lCV ?topMetal topMetal)
    pgCV = (dbOpenCellViewByType libName (strcat cellName "_pg") "layout")
    (when lCV (cdsp4add ?CV lCV))
    (when pgCV (cdsp4add ?CV pgCV))
    (when aCV  (cdsp4add ?CV aCV))
    (when cgCV (cdsp4add ?CV cgCV))
    lCV
    )
  )

(defun RouteLeaves ()
  ; new BD controllers
  (RouteCell "lib.bd.source.bitgen.BDC_BG.0" 2)
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
  (RouteCell "core.noc.crossbar.datapath.TAIL_CONTROL.1000" 2)
  (RouteCell "lib.qdi.metastable.slackless_arbiter.SLACKLESS_ARBITER2.20000" 2)
  (RouteCell "core.io.fpio.FPIO.20000" 4)

  ; a bunch of SHIFT_FIFOs
  (for w 8 16
       (RouteCell (sprintf nil "lib.qdi.serial.funny_register.SHIFT_FIFO-L%d_0-R.0"  w) 3)
       (RouteCell (sprintf nil "lib.qdi.serial.funny_register.SHIFT_FIFO-L%d_0-R.0h" w) 3)
       )
  t
  )

(defun RouteTest ()
  (RouteCell "lib.bd.6T.bank.ROUTED_SRAM_BANK-L512_24-R.1000" 6)
  )

(defun RouteMid ()
  ; cables
  (RouteCell "core.neuro.tile.E_CABLE.20000" 6 ?redoPG t ?colorGrid t) 
  (RouteCell "core.neuro.tile.N_CABLE.20000" 6 ?redoPG t ?colorGrid t)
  (RouteCell "core.neuro.tile.N_CABLE_1ROW.20000" 6 ?redoPG t ?colorGrid t) 
  (RouteCell "core.neuro.tile.N_CABLE_MC.20000" 6 ?redoPG t ?colorGrid t) 
  (RouteCell "core.neuro.tile.N_CABLE_MC_TALL.20000" 6 ?redoPG t ?colorGrid t) 
  (RouteCell "core.neuro.tile.N_CABLE_TALL.20000" 6 ?redoPG t ?colorGrid t) 
  (RouteCell "chip.n2.south.BYPASS.20000" 6 ?redoPG t ?colorGrid t)
  (RouteCell "chip.n2.south.GO_WEST.20000" 6 ?redoPG t ?colorGrid t)

  ; DOUBLE_ROUTER
  (RouteCell "core.noc.router.forward.MESH_FORWARD-L8_16_5_5_2_1_true_true_true_true-R.20000" 4)
  (RouteCell "core.noc.router.MESH_ROUTER-L8_8_16_5_5_2_true_true_true_true-R.20000" 6)
  (RouteCell "core.neuro.tile.DOUBLE_ROUTER.20000" 8 ?colorGrid t)

  ; SOUTH/NORTH
  (RouteCell "chip.n2.pads.PADS.20000" 8 ?colorGrid t)
  (RouteCell "core.noc.router.MESH_ROUTER(5,5,16,7,7,0,true,true,true,true).20000" 4) 
  (RouteCell "core.noc.router.forward.MESH_FORWARD(5,16,7,7,0,1,true,true,true,true).20000" 6) 
  (RouteCell "chip.n2.pads.PADS_IN_BD-L9-R.20000" 8 ?colorGrid t)
  (RouteCell "chip.n2.pads.PADS_OUT_BD-L9-R.20000" 8 ?colorGrid t)
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
  (ExportProteusCell "chip.n2.io.BASE_IO.20000"             "chip.n2.io.IO.20000")
  (ExportProteusCell "chip.n2.lakemont.BASE_CPU_SHIM.20000" "chip.n2.lakemont.CPU_SHIM.20000")
  (ExportProteusCell "core.neuro.bridge.BASE_BRIDGE.20000"  "core.neuro.bridge.BRIDGE.20000")
  (ExportProteusCell "chip.n2.pio.BASE_PIO_CABLE.20000"     "chip.n2.pio.PIO_CABLE.20000")
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
  (foreach cellName (list "lib.bd.source.bitgen.BDC_BG.0"
                          "lib.bd.standard.pipeline.CTRL.0"
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
