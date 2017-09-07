(load "~/cds_wd/N2/n2/reroute.il")
(RouteLeaf "lib.bd.standard.pipeline.MOUSETRAP_CTRL_CORE.0" 2)
(for i 0 8
     (RouteLeaf (sprintf nil "lib.bd.standard.pipeline.MOUSETRAP_CTRL-L%d-R.0"  i) 2 ?nearby t)
     )
(RouteCell "lib.bd.standard.pipeline.SIZE_MOUSETRAP_CTRL.0" 4 ?colorGrid nil ?timingDriven nil)
(exit)
