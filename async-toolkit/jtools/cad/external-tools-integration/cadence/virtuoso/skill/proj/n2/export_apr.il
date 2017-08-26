spar_cell_list =
    (list
     ; gen 3 controllers
     "lib.bd.standard.nopipe.RECV_CTRL.0"
     "lib.bd.standard.nopipe.SEND_CTRL.0"
     "lib.bd.convert.qdi.BD_TO_QDI_CTRL.0"
     "lib.bd.convert.qdi.QDI_TO_BD_CTRL.0"
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
     )

; CTREE
(for i 0 3
  (foreach s (list "" "n")
    spar_cell_list = (cons (sprintf nil "lib.util.ctree.CTREE2.%d%s" i s) spar_cell_list)
    spar_cell_list = (cons (sprintf nil "lib.util.ctree.CTREE3.%d%s" i s) spar_cell_list)
    )
  )

; TDL
(for i 1 8
  spar_cell_list = (cons (sprintf nil "lib.util.delay.tunable.TDL-L%d-R.0" i) spar_cell_list)
  spar_cell_list = (cons (sprintf nil "lib.util.delay.tunable.TDL-L%d-R.1" i) spar_cell_list)
  )

; PULSE_CTRL/PULSE_SEND_CTRL
(for i 0 8
  (foreach s (list "false" "true")
    spar_cell_list = (cons (sprintf nil "lib.bd.standard.pipeline.PULSE_CTRL-L%d_%s-R.0" i s) spar_cell_list)
    spar_cell_list = (cons (sprintf nil "lib.bd.standard.send.PULSE_SEND_CTRL-L%d_%s-R.0" i s) spar_cell_list)
    )
  )

; regenerate abstracts
(defun UpdateAbstractAPR ()
  (foreach cellName spar_cell_list
           CV = (ReadCV (GetCastLibName cellName) cellName "abstract")
           (when CV (cdsp4edit ?CV CV))
           CV = (ReadCV (GetCastLibName cellName) cellName "layout")
           (if (strncmp cellName "lib.bd.6T.bank.SRAM_BANK_CTRL" (strlen "lib.bd.6T.bank.SRAM_BANK_CTRL"))==0
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
