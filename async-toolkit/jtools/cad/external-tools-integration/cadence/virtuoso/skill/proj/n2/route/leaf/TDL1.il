(load "~/cds_wd/N2/n2/reroute.il")
(for i 1 4
     CV = (RouteLeaf (sprintf nil "lib.util.delay.tunable.BINARY_CS_DELAY_STAGE-L%d-R.10000" 2*i) 2)
     (PatchTDL ?CV CV)
     )
(exit)
