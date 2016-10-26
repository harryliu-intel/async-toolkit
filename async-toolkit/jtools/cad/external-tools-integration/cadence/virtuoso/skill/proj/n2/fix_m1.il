

(defun FixM1OverFillCells (@key (CV (geGetEditCellView)))
  (let (overlaps i)
     i=0
    (foreach shape (setof x CV->shapes (car x->lpp)=="m1" && (cadr x->lpp)=="fill")
        overlaps = (dbGetTrueOverlaps CV shape->bBox)
	(foreach item overlaps
	  (when item->objType=="inst"
	    (when item->cellName=="globals.FILL_GRID.m012345678_fill"
	      (dbDeleteObject shape)
	      i++
	      )
	    )
	  )
        )
      ;)
      (printf "Deleted %d m1 fill shapes over placed mosaics.\n" i)
      (dbSave CV)
    )
  )
