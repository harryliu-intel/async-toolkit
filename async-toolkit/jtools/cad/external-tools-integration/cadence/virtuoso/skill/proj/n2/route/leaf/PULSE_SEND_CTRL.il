(load "~/cds_wd/N2/n2/reroute.il")
(RouteLeaf "lib.bd.standard.send.PULSE_SEND_CTRL_CORE.0" 2)
(for i 1 8 (RouteLeaf (sprintf nil "lib.bd.standard.send.PULSE_SEND_CTRL-L%d-R.0" i) 2))
(RouteCell "lib.bd.standard.send.SIZE_PULSE_SEND.0" 4 ?timingDriven nil)
