; Copyright 2010 Fulcrum Microsystems.  All rights reserved.
; $Author: lines$
;
; User interface skill commands for opening cells and changing views.

; decode libName from a CAST FQCN (all but the last 2 dot-seperated fields)
(defun GetCastLibName (cellName)
  (let (libName)
    libName = (parseString cellName ".")
    libName = (reverse (cddr (reverse libName)))
    (when libName libName = (buildString libName "."))
    )
  )

; open a new window from a CAST name and view
(defun Open
  (cellName        ; CAST FQCN
   viewName        ; view name
   @key (mode "a") ; mode: r, w, a
   )
  (let (libName CV)
    libName = (GetCastLibName cellName)
    (when libName
      (when (ddGetObj libName cellName viewName)
        (geOpen ?lib libName ?cell cellName ?view viewName ?mode mode)
        )
      )
    )
  )

; open a new window from a CAST name and view in read mode
(defun Read 
  (cellName ; CAST FQCN
   viewName ; view name
   )
  (Open cellName viewName ?mode "r")
  )

; change views of the current cell
(defun View
  (viewName ; view name
   )
  (let (CV WIN newCV)
    CV = (geGetEditCellView)
    (when CV
      WIN = (geGetCellViewWindow CV)
      (when (and WIN (ddGetObj CV->libName CV->cellName viewName))
        (geChangeCellView WIN CV->libName CV->cellName viewName CV->mode)
        )
      )
    )
  )

; cycle between available view in the given window
(defun CycleViews ()
  (when (mod CycleView 6)==0
    (unless (View "floorplan") CycleView=CycleView+1 )
  )
  (when (mod CycleView 6)==1
    (unless (View "prelayout") CycleView=CycleView+1 )
  )
  (when (mod CycleView 6)==2
    (unless (View "flatten") CycleView=CycleView+1 )
  )
  (when (mod CycleView 6)==3
    (unless (View "layout_pg") CycleView=CycleView+1 )
  )
  (when (mod CycleView 6)==4
    (unless (View "layout") CycleView=CycleView+1 )
  )
  (when (mod CycleView 6)==5
    (unless (View "abstract") CycleView=0 (View "floorplan") )
  )
  CycleView=CycleView+1
)

(defun CycleViewsBack ()
  (when (mod CycleView 6)==0
    (unless (View "floorplan") CycleView=5 (View "abstract") )
  )
  (when (mod CycleView 6)==1
    (unless (View "prelayout") CycleView=CycleView-1 )
  )
  (when (mod CycleView 6)==2
    (unless (View "flatten") CycleView=CycleView-1 )
  )
  (when (mod CycleView 6)==3
    (unless (View "layout_pg") CycleView=CycleView-1 )
  )
  (when (mod CycleView 6)==4
    (unless (View "layout") CycleView=CycleView-1 )
  )
  (when (mod CycleView 6)==5
    (unless (View "abstract") CycleView=-1 )
  )
  CycleView=CycleView-1
)

(defun InitCycleViews ()
  CycleView = 0
)
InitCycleViews()



(defun StepViewBack ()
 (let (viewname)
  viewname=(geGetEditCellView)->viewName
  (when viewname=="prelayout"
    (View "floorplan")
  )
  (when viewname=="flatten"
    (View "prelayout")
  )
  (when viewname=="layout_pg"
    (View "flatten")
  )
  (when viewname=="layout"
    (View "layout_pg")
  )
  CycleView=CycleView-1
 )
)
