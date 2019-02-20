cells=nil
(for n 2 3
     (for s 0 3
          cells = (cons (sprintf nil "lib.util.ctree.CTREE%d.%d" n s) cells)
          )
     )
cells = (cons "lib.util.ctree.RHI_CTREE2.2" cells)
(foreach cellName cells
         CV = (dbOpenCellViewByType (GetCastLibName cellName) cellName "floorplan")
         (LaygenPR ?CV CV ?p_max 2 ?num_place 20)
         )
(exit)
