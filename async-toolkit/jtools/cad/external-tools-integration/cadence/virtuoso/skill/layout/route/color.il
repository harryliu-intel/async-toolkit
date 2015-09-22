; create a FQCN_reverse_color_m0 overlay cell to reverse m0 coloring
(defun ReverseColorM0 (@key (CV (geGetEditCellView)))
  (let (inMapping outMapping cellName dstCV)
    cellName = (strcat CV->cellName "_reverseColorM0")
    inMapping = '(("m0draw"  ("m0"  "drawing"))
                  ("mc0comp" ("mc0" "complement"))
                  ("mc0draw" ("mc0" "drawing")))
    outMapping = '(("a" ("mc0" "complement"))
                   ("b" ("m0" "backBone")))
    (ICVLayerProcessorSimple outMapping "reverseColorM0" CV->libName cellName "layout"
                             ?CV CV ?inputLayerMap inMapping)
    (dbOpenCellViewByType CV->libName cellName "layout" "maskLayout" "r")
    )
  )
