(load "~/cds_wd/N2/n2/reroute.il")
(RouteLeaf "lib.bd.standard.pipeline.PULSE_CTRL_CORE-Lfalse-R.0" 2)
(RouteLeaf "lib.bd.standard.pipeline.PULSE_CTRL_CORE-Ltrue-R.0" 2)
(for i 1 8
     CV = (RouteLeaf (sprintf nil "lib.bd.standard.pipeline.PULSE_CTRL-L%d_false-R.0" i) 2)
     CV = (dbCopyCellView
           CV CV->libName (sprintf nil "lib.bd.standard.pipeline.PULSE_CTRL-L%d_true-R.0" i) "layout"
           nil nil t)
     (UpdateFloorplan ?CV CV)
     (cdsp4add ?CV CV)
     CV = (MakeLeafAbstract ?CV CV)
     (cdsp4add ?CV CV)
     )
(RouteCell "lib.bd.standard.pipeline.SIZE_PULSE_CTRL.0" 4 ?colorGrid nil ?timingDriven nil)
