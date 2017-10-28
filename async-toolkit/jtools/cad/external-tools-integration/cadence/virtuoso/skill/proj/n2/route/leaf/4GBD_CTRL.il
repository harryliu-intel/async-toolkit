(load "~/cds_wd/N2/n2/reroute.il")
(RouteLeaf "lib.bd.standard.pipeline.4GBD_CTRL_CORE.0" 2)
(for i 1 8
     (RouteLeaf (sprintf nil "lib.bd.standard.pipeline.4GBD_CTRL-L%d_false_false-R.0" i) 2 ?nearby t)
     (RouteLeaf (sprintf nil "lib.bd.standard.pipeline.4GBD_CTRL-L%d_true_false-R.0"  i) 2 ?nearby t)
     (RouteLeaf (sprintf nil "lib.bd.standard.pipeline.4GBD_CTRL-L%d_false_true-R.0"  i) 2 ?nearby t)
     )
(RouteCell "lib.bd.standard.pipeline.SIZE_4GBD_CTRL.0" 4 ?colorGrid nil ?timingDriven nil)
(exit)
