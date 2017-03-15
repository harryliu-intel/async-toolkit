(load "~/cds_wd/N2/n2/reroute.il")
(for w 8 16
     (RouteLeaf (sprintf nil "lib.qdi.serial.funny_register.SHIFT_FIFO-L%d_0-R.0h" w) 3)
     )
(exit)
