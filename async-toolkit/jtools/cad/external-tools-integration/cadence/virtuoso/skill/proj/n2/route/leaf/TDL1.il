(load "~/cds_wd/N2/n2/reroute.il")
(for i 1 6
     CV = (RouteLeaf (sprintf nil "lib.util.delay.tunable.TDL-L%d-R.0" i) 2)
     (PatchTDL ?CV CV)
     )
(exit)
