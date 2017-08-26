(load "~/cds_wd/N2/n2/reroute.il")
(RouteLeaf "lib.bd.standard.pipeline.PULSE_CTRL_CORE-Lfalse-R.0" 2)
(RouteLeaf "lib.bd.standard.pipeline.PULSE_CTRL_CORE-Ltrue-R.0" 2)
(for i 0 8
     (RouteLeaf (sprintf nil "lib.bd.standard.pipeline.PULSE_CTRL-L%d_false-R.0" i) 2 ?nearby t)
     (RouteLeaf (sprintf nil "lib.bd.standard.pipeline.PULSE_CTRL-L%d_true-R.0"  i) 2 ?nearby t)
     )
(RouteCell "lib.bd.standard.pipeline.SIZE_PULSE_CTRL.0" 4 ?colorGrid nil ?timingDriven nil)
(exit)
