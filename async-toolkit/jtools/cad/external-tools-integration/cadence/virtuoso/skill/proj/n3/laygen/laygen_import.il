; exeuctable for lgf2gds
lgf2gds = "~/sw/cad/external-tools-integration/cadence/virtuoso/skill/proj/n3/laygen/lgf2gds"

; list of our Laygen cells with both names
laygen_cells =
(list
 (list "lib.util.widget.DIVERTER.c0" "e08divertac1d06x5")
 (list "lib.util.widget.ALTVERTER.c0" "e08toggleac1d06x5")
 (list "core.noc.crossbar.datapath.TAIL_CONTROL.c0" "e08xbrctlac1d06x5")
 (list "lib.util.ctree.CTREE2.c0" "e08ctree2ac1n03x5")
 (list "lib.util.ctree.CTREE2.c1" "e08ctree2ac1n06x5")
 (list "lib.util.ctree.CTREE2.c2" "e08ctree2ac1n09x5")
 (list "lib.util.ctree.CTREE2.c3" "e08ctree2ac1n12x5")
 (list "lib.util.ctree.CTREE3.c0" "e08ctree3ac1n03x5")
 (list "lib.util.ctree.CTREE3.c1" "e08ctree3ac1n06x5")
 (list "lib.util.ctree.CTREE3.c2" "e08ctree3ac1n09x5")
 (list "lib.util.ctree.CTREE3.c3" "e08ctree3ac1n12x5")
 (list "lib.util.ctree.RHI_CTREE2.c1" "e08ctrhi2ac1n06x5")
 (list "lib.util.ctree.RLO_CTREE2.c1" "e08ctrlo2ac1n06x5")
 (list "lib.util.tdl.WEAKENER.c0" "e08weakenac1d03x5")
 (list "lib.util.tdl.WEAK_BUF.c0" "e08wekbufac1d02x5")
 (list "lib.util.tdl.WEAK_QUAD_INV.c0" "e08wkqinvac1d02x5")
 (list "lib.util.widget.XOR_LATCH.c0" "e08xorltcac1n06x5")
 (list "lib.util.widget.ALTVERTER_LATCH.c0" "e08atvltcac1n06x5")
 (list "lib.qdi.metastable.filter.HALF_FILTER.c0" "e08hffiltac1n03x5")
 (list "core.noc.crossbar.datapath.TAIL_CONTROL_La.c0" "e08xbrtclac1n03x5")
 (list "core.noc.crossbar.datapath.TAIL_CONTROL_y.c0" "e08xbrtcyac1n03x5")
 )

; Import one laygen cell
(defun ImportLaygenCell (fqcn cell @key
                              (dir "laygen")
                              (placements 10)
                              (action (lambda (CV) (cdsp4add ?CV CV))))
  (let (CV libName cellName done base log lgf gds cmd)
    libName = (GetCastLibName fqcn)
    (for n 0 placements-1
         log=(sprintf nil "%s/%s.run/out/%s_p%d/route.log" dir fqcn cell n)
         (when (shell (sprintf nil "grep \"routing successful\" %s" log))
           base=(sprintf nil "%s/%s.run/out/%s_p%d/out/%s_r0" dir fqcn cell n cell)
           lgf=(sprintf nil "%s.lgf" base)
           gds=(sprintf nil "%s.gds" base)
           (when (isFile lgf)
             (printf "%s %s %d\n" fqcn cell n)
             cmd=(sprintf nil "%s %s %s" lgf2gds lgf gds)
             (shell cmd)
             CV=(ImportGDS gds libName cell ?suffix (sprintf nill ".%d" n)
                           ?setCopyExact (lambda (CV) t))
             (unless done ; make variants from the first one that routes clean
               (MakeVariants libName fqcn ?CV CV ?action action)
               done=t
               )
             )
           )
         )
    )
  t
  )

; Import all laygen cells
(defun ImportLaygenCells (@key (dir "laygen") (placements 10) (action (lambda (CV) (cdsp4add ?CV CV))))
  (let (fqcn cell)
    (foreach fqcn_cell laygen_cells
             fqcn = (car fqcn_cell)
             cell = (cadr fqcn_cell)
             (ImportLaygenCell fqcn cell ?dir dir ?placements placements ?action action)
             )
    )
  t
  )
