(defun LabelBumps (@key (CV (geGetEditCellView)))
  (let (inst transform x y rect pinfile finished)
    (DeleteAutoGen ?CV CV)
    inst = (dbFindAnyInstByName CV "bumps")
    (unless inst (error "Can't find bumps instance"))

    ; lift up labels from subcells
    transform = (dbGetInstTransform inst)
    (foreach fig (setof x inst->master->shapes x->objType=="label" && x->lpp==(list "tm1" "drawing"))
             fig = (dbCopyFig fig CV transform)
             (dbReplaceProp fig "autogen" "boolean" t)
             )

    ; rename and scale labels and add tm1 shape
    finished = (makeTable "finished" nil)
    pinfile = (outfile "n1_pins.tcl")
    (foreach fig (setof x CV->shapes x->objType=="label")
      (cond (finished[fig->xy] (dbDeleteObject fig))
            (t
             finished[fig->xy] = t
             fig->lpp = (list "tm1" "drawing")
             fig->height = 8
             x = (car fig->xy)
             y = (cadr fig->xy)
             rect = (dbCreateRect CV (list "tm1" "drawing") (list x-1.68:y-1.596 x+1.68:y+1.596))
             rect->net = (dbMakeNet CV fig->theLabel)
             (dbCreatePin rect->net rect)
             (dbReplaceProp rect "autogen" "boolean" t)
             (fprintf pinfile "createPin \"%s\" \"TM1\" %g %g %g %g\n"
                      fig->theLabel x-1.68 y-1.596 x+1.68 y+1.596)
             )
            )
      )
    (close pinfile)
    )
  t
  )

(defun AlignBumpLabels (@key (CV (geGetEditCellView)) (offsetX 0) (offsetY 0) (gridX 105) (gridY 79.8))
  (let (x y)
    (foreach label (setof x CV->shapes x->objType=="label" && x->lpp==(list "tm1" "drawing"))
             x = (car  label->xy)-offsetX
             y = (cadr label->xy)-offsetY
             x = gridX * (round x/gridX)+offsetX
             y = gridY * (round y/gridY)+offsetY
             label->xy = x:y
             )
    )
  t
  )

(defun CheckBumpCoords (@key (CV (geGetEditCellView)))
  (let (found_x found_y allx ally a x n)
    n=0
    found_xy = (makeTable "found_xy" nil)
    found_x = (makeTable "found_x" nil)
    found_y = (makeTable "found_y" nil)
    (foreach bump (setof x CV->instances x->cellName=="globals.BUMP.1")
             n++
             x = (round (car  bump->xy)/MfgGrid)
             y = (round (cadr bump->xy)/MfgGrid)
             (when found_xy[x:y] (geSelectObject bump) (printf "ERROR: duplicate at %d:%d\n" x y))
             found_xy[x:y]=t
             (printf "%d:%d\n" x y)
             (unless found_x[x] allx = (cons x allx))
             (unless found_y[y] ally = (cons y ally))
             found_x[x] = t
             found_y[y] = t
             )
    allx = (sort allx (lambda (a b) a<b))
    ally = (sort ally (lambda (a b) a<b))
    fout = (outfile "bump_coords.txt")
    (foreach x allx (fprintf fout "x=%d\n" x))
    (foreach y ally (fprintf fout "y=%d\n" y))
    (close fout)
    n
    )
  )

(defun CheckBumpLabels (@key (CV (geGetEditCellView)))
  (let (found_x found_y allx ally a x n)
    n=0
    found_xy = (makeTable "found_xy" nil)
    (foreach label (setof x CV->shapes x->objType=="label" && x->lpp==(list "tm1" "drawing"))
             n++
             x = (car  label->xy)
             y = (cadr label->xy)
             (when found_xy[x:y] (geSelectObject label) (printf "ERROR: duplicate at %g:%g\n" x y))
             found_xy[x:y]=t
             )
    n
    )
  )

; use with parse_die_file < 8SGL73I_A0_C4.die > place_bumps.il
; load "place_bumps.il"
(defun PlaceBump (num name x y @key (CV (geGetEditCellView)) (dx 1441.02) (dy -1604.736) (labels_only nil))
  (let (rect bumpCV inst prb n)
    n=0
    prb = CV->prBoundary->bBox
    x = x+dx
    y = y+dy
    (when x>=(leftEdge prb) && x<=(rightEdge prb) && y>=(bottomEdge prb) && y<=(topEdge prb)
          n++
          (unless labels_only
            bumpCV = (dbOpenCellViewByType "globals" "globals.BUMP.1" "layout")
            inst = (dbCreateInst CV bumpCV (sprintf nil "bump_%d" num) x:y "R0")
            (dbReplaceProp inst "autogen" "boolean" t)
            )
          rect = (dbCreateRect CV (list "tm1" "drawing") (list x-1.68:y-1.596 x+1.68:y+1.596))
          (dbReplaceProp rect "autogen" "boolean" t)
          rect->net = (dbMakeNet CV name)
          (dbCreatePin rect->net rect)
          (LabelPin CV rect)
          )
    n
    )
  )

(defun PlaceBumps (@key (CV (geGetEditCellView)) (labels_only nil))
  (let ( )
    (DeleteAutoGen ?CV CV)
    (load "place_bumps.il")
    (foreach bump bumps
             (PlaceBump (car bump) (cadr bump) (caddr bump) (cadddr bump) ?CV CV ?labels_only labels_only)
             )
    (foreach label CV->shapes
             (when label->objType=="label" label->height=15)
             )
    )
  t
  )
