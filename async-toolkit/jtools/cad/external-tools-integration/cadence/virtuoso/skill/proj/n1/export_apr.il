(defun ExportAPR ()
  (ExportLefGds "APR"
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
     ; BD_CONTROLLER
     "lib.bd.metastable.arbiter.BD_ARBITER_CTRL.0"
     "lib.bd.standard.merge.BD_MERGE_CTRL-L2-R.0"
     "lib.bd.standard.split.BD_ALT_SPLIT_CTRL-L2-R.0"
     "lib.bd.6T.bank.BD_SRAM_CTRL.0"
     "lib.bd.standard.split.BD_SPLIT_CTRL-L2-R.0"
     ;"lib.bd.standard.merge.BD_MERGE_DATA-L2-R.2"
     ;"lib.bd.standard.merge.BD_MERGE_DATA-L2-R.1"
     "lib.bd.standard.merge.BD_MERGE_DATA-L2-R.0"
     "lib.bd.standard.merge.BD_ALT_MERGE_CTRL-L2-R.0"
     "lib.bd.standard.receive.BD_RECV_CTRL.0"
     "lib.bd.standard.send.BD_SEND_CTRL.0"
     "lib.bd.standard.pipeline.BD_ISOLATE_TOK_CTRL.0"
     "lib.bd.standard.pipeline.BD_ISOLATE_CTRL.0"
     "lib.bd.standard.pipeline.BD_TOK_CTRL.0"
     "lib.bd.standard.pipeline.BD_CTRL.0"
     ; physical cells
     "globals.bdc01ld.z04"
     "globals.bdc01ld.z08"
     ; TOP_HALO
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
     )
    )
  t
  )

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
