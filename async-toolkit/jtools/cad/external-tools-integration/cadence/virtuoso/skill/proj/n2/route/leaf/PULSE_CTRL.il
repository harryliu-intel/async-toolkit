(load "~/cds_wd/N2/n2/reroute.il")
(RouteLeaf "lib.bd.standard.pipeline.PULSE_CTRL_CORE.0" 2)
(for i 0 15
     (RouteLeaf (sprintf nil "lib.bd.standard.pipeline.PULSE_CTRL-L%d-R.0" i) 2)
     )
(exit)
