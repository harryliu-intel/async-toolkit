(LoadProjSW "n2/reroute.il")
(for i 1 8
     CV = (RouteLeaf (sprintf nil "lib.util.tdl.TDL-L%d-R.1" i) 2)
     ; (PatchTDL ?CV CV) ; keep quad height TDL filled to M2
     )
(RouteCell "lib.util.tdl.SIZE_TDL.1" 4 ?timingDriven nil)
(exit)
