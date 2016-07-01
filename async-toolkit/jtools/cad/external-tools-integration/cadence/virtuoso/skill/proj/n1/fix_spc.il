; add missing spacers
; steps across looking for "m2":"drawing" but no poly:drawing or poly:fill
(defun AddMissingSpacers (@key (CV (geGetEditCellView)))
  (let (spcCV x0 x1 y0 y1 n bbox orient inst)
    n=0
    spcCV = (dbOpenCellViewByType "vendor.intel.d04" "vendor.intel.d04.spc00ln.z01" "layout")
    bbox = CV->prBoundary->bBox
    x0 = (car  (car  bbox))
    y0 = (cadr (car  bbox))
    x1 = (car  (cadr bbox))
    y1 = (cadr (cadr bbox))
    xa = (car  gridAlignment[2])
    ya = (cadr gridAlignment[2])
    x0 = (round x0/xa)
    x1 = (round x1/xa)
    y0 = (round y0/ya)
    y1 = (round y1/ya)
    (for y y0 y1
      (for x x0 x1-1
        (foreach flipX (list -0.5 0.5)
           bbox = (list x*xa:y*ya (x+1)*xa:(y+flipX)*ya)
           (when (dbGetOverlaps CV bbox (list "m2" "drawing") 2)!=nil &&
                 (dbGetOverlaps CV bbox (list "poly" "drawing") 32)==nil &&
                 (dbGetOverlaps CV bbox (list "poly" "fill") 32)==nil
                 orient = (if flipX>=0 "R0" "MX")
                 (printf "%d %g %g %s\n" n x*xa y*ya orient)
                 inst = (dbCreateInst CV spcCV (sprintf nil "extra_spc%d" n++) x*xa:y*ya orient)
                 (dbReplaceProp inst "autogen" "boolean" t)
                 )
           )
        )
      )
    (FixFillerCells ?CV CV) ; choose proper implant
    n
    )
  )
