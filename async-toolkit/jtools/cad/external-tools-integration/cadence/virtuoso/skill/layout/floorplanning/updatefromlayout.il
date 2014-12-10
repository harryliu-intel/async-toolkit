(defun GetPrbound (cv)
 (let (prb prb2)
  (foreach shape cv->shapes
    (when (car shape->lpp)=="prBoundary" && (cadr shape->lpp)=="drawing"
      prb=shape
    )
  )
  (when cv->prBoundary
    prb=cv->prBoundary
  )
  prb
 )
)

; return points of prBoundary as if it were a polygon
(defun GetPrboundPoints (cv)
  (let (prb x0 y0 x1 y1)
    prb = (car (setof s cv->shapes (car s->lpp)=="prBoundary" && (cadr s->lpp)=="drawing"))
    (unless prb prb=cv->prBoundary)
    (cond (prb->objType=="rect"
           x0 = (car  (car  prb->bBox))
           y0 = (cadr (car  prb->bBox))
           x1 = (car  (cadr prb->bBox))
           y1 = (cadr (cadr prb->bBox))
           (list x0:y0 x1:y0 x1:y1 x0:y1)
           )
          (t
           prb->points
           )
          )
    )
  )

(defun ConvertPrboundToObject
  (@key (CV (geGetEditCellView))) ; specify CellView or nil to use edit view
  (let (prb)
    prb=(GetPrbound CV)
    (if prb->objType=="PRBoundary" then
      (printf "already a boundary object.\n")
    else
      (when prb->objType=="rect"
        (dbCreatePRBoundary CV (RectGetPolygonPoints prb->bBox))
        (dbDeleteObject prb)
      )
      (when prb->objType=="polygon" 
        (dbCreatePRBoundary CV prb->points)
        (dbDeleteObject prb)
      )
    )
  )
)


(defun UpdateLeafFloorplanList (leaflist)
 (let (cells floorplan layout prbf prbl count)

  count=0
  cells=(OpenCellViewsFromFile leaflist "floorplan" "r")
  (foreach cell cells
     floorplan=nil layout=nil prbf=nil prbl=nil
     layout=(nrOpenCellViewReadable cell->libName cell->cellName
               "layout")
     (when layout
       prbl=(GetPrbound layout)
     )
     floorplan=(nrOpenCellViewReadable cell->libName cell->cellName
               "floorplan")
     (when floorplan
       prbf=(GetPrbound cell)
       (when prbl && prbf
        (if prbf->bBox!=prbl->bBox then
          (CDSP4Edit
           ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
           ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
           ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
           ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
           cell 
           ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
          ) 
         (printf "Updating floorplan of %s.\n" cell->cellName)
          prbf->bBox=prbl->bBox
          count=count+1
          (dbSave floorplan)
        else
         (printf "Prbounds match for %s.\n" cell->cellName)
        )
       )
     )
  )
  (printf "Updated %d floorplan views.\n" count)
 )
)

