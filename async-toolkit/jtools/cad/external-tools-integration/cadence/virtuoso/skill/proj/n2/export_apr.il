spar_cell_list =
    (list
     ; CTREE
     "lib.util.ctree.CTREE2.0"
     "lib.util.ctree.CTREE2.1"
     "lib.util.ctree.CTREE2.2"
     "lib.util.ctree.CTREE2.3"
     "lib.util.ctree.CTREE2.0n"
     "lib.util.ctree.CTREE2.1n"
     "lib.util.ctree.CTREE2.2n"
     "lib.util.ctree.CTREE2.3n"
     "lib.util.ctree.CTREE3.0"
     "lib.util.ctree.CTREE3.1"
     "lib.util.ctree.CTREE3.2"
     "lib.util.ctree.CTREE3.3"
     "lib.util.ctree.CTREE3.0n"
     "lib.util.ctree.CTREE3.1n"
     "lib.util.ctree.CTREE3.2n"
     "lib.util.ctree.CTREE3.3n"
     ; custom datapath cells
     "lib.bd.standard.merge.BD_MERGE_DATA-L2-R.0"
     ; SRAM controller
     "lib.bd.6T.bank.SRAM_BANK_CTRL.0"
     ; new BD_CONTROLLER (nominal thresold)
     "lib.bd.source.bitgen.BDC_BG.0n"
     "lib.bd.metastable.arbiter.ARBITER_CTRL.0n"
     "lib.bd.standard.merge.ALT_MERGE_CTRL-L2-R.0n"
     "lib.bd.standard.merge.MERGE_CTRL-L2-R.0n"
     "lib.bd.standard.pipeline.CTRL.0n"
     "lib.bd.standard.pipeline.ISOLATE_CTRL.0n"
     "lib.bd.standard.pipeline.ISOLATE_TOK_CTRL.0n"
     "lib.bd.standard.pipeline.FLOP_TOK_CTRL.0n"
     "lib.bd.standard.pipeline.TOK_CTRL.0n"
     "lib.bd.standard.receive.RECV_CTRL.0n"
     "lib.bd.standard.send.SEND_CTRL.0n"
     "lib.bd.standard.split.ALT_SPLIT_CTRL-L2-R.0n"
     "lib.bd.standard.split.SPLIT_CTRL-L2-R.0n"
     ; new BD_CONTROLLER (UV1 thresold)
     "lib.bd.source.bitgen.BDC_BG.0"
     "lib.bd.metastable.arbiter.ARBITER_CTRL.0"
     "lib.bd.standard.merge.ALT_MERGE_CTRL-L2-R.0"
     "lib.bd.standard.merge.MERGE_CTRL-L2-R.0"
     "lib.bd.standard.pipeline.CTRL.0"
     "lib.bd.standard.pipeline.ISOLATE_CTRL.0"
     "lib.bd.standard.pipeline.ISOLATE_TOK_CTRL.0"
     "lib.bd.standard.pipeline.FLOP_TOK_CTRL.0"
     "lib.bd.standard.pipeline.TOK_CTRL.0"
     "lib.bd.standard.receive.RECV_CTRL.0"
     "lib.bd.standard.send.SEND_CTRL.0"
     "lib.bd.standard.split.ALT_SPLIT_CTRL-L2-R.0"
     "lib.bd.standard.split.SPLIT_CTRL-L2-R.0"
     ; TOP_HALO (physical only but very common)
     "globals.TOP_HALO.n"
     "globals.TOP_HALO.s"
     "globals.TOP_HALO.nc"
     "globals.TOP_HALO.sc"
     "globals.TOP_HALO.ni"
     "globals.TOP_HALO.si"
     "globals.TOP_HALO.v"
     "globals.TOP_HALO.v_tap"
     "globals.TOP_HALO.ni_tap"
     "globals.TOP_HALO.si_tap"
     ; tunable delay lines
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L2-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L4-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L6-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L8-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L10-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L12-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L14-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L16-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L18-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L20-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L22-R.10000"
     "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L24-R.10000"
     )

; regenerate abstracts
(defun UpdateAbstractAPR ()
  (foreach cellName spar_cell_list
           CV = (dbOpenCellViewByType (GetCastLibName cellName) cellName "abstract")
           (cdsp4edit ?CV CV)
           CV = (dbOpenCellViewByType (GetCastLibName cellName) cellName "layout")
           (if cellName=="lib.bd.6T.bank.SRAM_BANK_CTRL.0"
             (MakeSimpleAbstract ?CV CV ?topMetal 3) ; CLASS BLOCK
             (MakeLeafAbstract ?CV CV)   ; CLASS CORE
             )
           )
  t
  )

; export GDS and LEF for all standard cells needed for APR
(defun ExportAPR ()
  (ExportLefGds "APR" spar_cell_list)
  t
  )

; export GDS and LEF for a list of cells
(defun ExportLefGds (basename cellNames)
  (let (cells TEMP WDIR)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR = (strcat TEMP "/export")
    (foreach cellName cellNames
             libName = (GetCastLibName cellName)
             cells = (cons (dbOpenCellViewByType libName cellName "layout") cells)
             )
    (ExportCellsGDS WDIR cells)
    (ExportCellsLEF (strcat WDIR "/" basename) cells)
    )
  t
  )
