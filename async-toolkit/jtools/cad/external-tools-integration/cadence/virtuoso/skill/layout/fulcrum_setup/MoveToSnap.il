; This Fuction snaps the selected objext to the closest neighbor instance only 


procedure( MoveToSnap( direction )
  let( (SelectedCellList x_low_org y_low_org x_high_org y_high_org
         x_low y_low x_high y_high d_closestNeighbor)

  ; sort selected cells so that they move in order of closest first
  SelectedCellList = sort( geGetSelSet( )
    lambda((cell1 cell2) 
      case( direction
        ("L" leftEdge(cell1~>bBox)<leftEdge(cell2~>bBox))
        ("R" rightEdge(cell1~>bBox)>rightEdge(cell2~>bBox))
        ("T" topEdge(cell1~>bBox)>topEdge(cell2~>bBox))
        ("B" bottomEdge(cell1~>bBox)<bottomEdge(cell2~>bBox))
      )
    )
  )

  foreach( SelectedCell SelectedCellList 
    (if (IsInstanceAnchored SelectedCell) then
      (printf "Not moving anchored instance %s.\n" SelectedCell->name)
    else ;do the rest

    x_low_org  = car( car( SelectedCell->bBox ) )
    y_low_org  = car( cdr( car( SelectedCell->bBox ) ) )
    x_high_org = car( car( cdr( SelectedCell->bBox ) ) )
    y_high_org = car( cdr( car( cdr( SelectedCell->bBox ) ) ) )
    d_closestNeighbor=nil

    if( SelectedCell->objType == "inst" then 
      ;d_closestNeighbor = dbGetNeighborInstanceNoAbutted( SelectedCell direction )
      ;d_closestNeighbor= car(cadr( dbGetNeighbor( wcv() 
      ;                     list(x_low_org+0.005:y_low_org+0.005 
      ;                          x_high_org-0.005:y_high_org-0.005) 
      ;                          direction nil 32 )))

      d_closestNeighbor = (FindClosestNeighborInstance (wcv) direction SelectedCell)
    ) ;end if

    if( d_closestNeighbor != nil then
      x_low  = car( car( d_closestNeighbor->bBox ) )
      y_low  = car( cdr( car( d_closestNeighbor->bBox ) ) )
      x_high = car( car( cdr( d_closestNeighbor->bBox ) ) )
      y_high = car( cdr( car( cdr( d_closestNeighbor->bBox ) ) ) )

      if( direction == "L" then
        xOffset = x_high - x_low_org
        yOffset = 0
      )
      if( direction == "R" then
        xOffset = x_low - x_high_org
        yOffset = 0
      )
      if( direction == "T" then
        xOffset = 0
        yOffset = y_low - y_high_org
      )
      if( direction == "B" then
        xOffset = 0
        yOffset = y_high - y_low_org
      )
      dbMoveFig( SelectedCell wcv() list(xOffset:yOffset "R0"))
    ) ;end if
   )
  ) ;foreach
  d_closestNeighbor
 )
) ;procedure


; Re-implement dbGetNeighbor that works for instances.  Silly Cadence!
(defun FindClosestNeighborInstance (CV direction inst)
  (let (bbox d_closestNeighbor x x0 y0 x1 y1 search_bbox overlaps distance
        distance_L distance_R distance_T distance_B best_inst best_distance)
    bbox = inst->bBox
    x0 = (car  (car  bbox))
    y0 = (cadr (car  bbox))
    x1 = (car  (cadr bbox))
    y1 = (cadr (cadr bbox))
    (cond (direction=="L" x1=x0 x0=(car  (car  CV->bBox)))
          (direction=="R" x0=x1 x1=(car  (cadr CV->bBox)))                     
          (direction=="B" y1=y0 y0=(cadr (car  CV->bBox)))
          (direction=="T" y0=y1 y1=(cadr (cadr CV->bBox)))
          )
    search_bbox = (list x0+MfgGrid:y0+MfgGrid x1-MfgGrid:y1-MfgGrid)
    overlaps = (dbGetOverlaps CV search_bbox)
    best_distance = nil
    best_inst = nil
    (foreach b (setof a overlaps a->objType=="inst" && a!=inst)
             distance_L = x1 - (car (cadr b->bBox))
             distance_R = (car (car b->bBox)) - x0
             distance_B = y1 - (cadr (cadr b->bBox))
             distance_T = (cadr (car b->bBox)) - y0
             (cond (direction=="L" distance=distance_L)
                   (direction=="R" distance=distance_R)
                   (direction=="B" distance=distance_B)
                   (direction=="T" distance=distance_T)
                   )
             (when distance>=0 && (!best_distance || distance<best_distance)
                   best_distance = distance
                   best_inst = b
                   )
             )
    best_inst
    )
  )

; This Fuction snaps the selected objext to the closest neighbor instance and
; tries to align the selected cell vertically with the closest neighbor to form a data path
; This Skill Code is intended to run when you have ONLY ONE slected cell 
procedure( MoveToSnapAlignV( direction )

	; Make sure there is ONLY ONE cell selected
	
	if( length( geGetSelectedSet() ) == 1 
	
		then	
			d_SelectedCell = car( geGetSelectedSet( ) )
			d_closestNeighbor = MoveToSnap( direction )

			x_low_org  = car( car( d_SelectedCell->bBox ) )
        		y_low_org  = car( cdr( car( d_SelectedCell->bBox ) ) )
        		x_high_org = car( car( cdr( d_SelectedCell->bBox ) ) )
        		y_high_org = car( cdr( car( cdr( d_SelectedCell->bBox ) ) ) )

			x_low      = car( car( d_closestNeighbor->bBox ) )
        		y_low      = car( cdr( car( d_closestNeighbor->bBox ) ) )
        		x_high     = car( car( cdr( d_closestNeighbor->bBox ) ) )
        		y_high     = car( cdr( car( cdr( d_closestNeighbor->bBox ) ) ) )

			if( d_closestNeighbor != nil &&
			    d_closestNeighbor->master == d_SelectedCell->master

				then
					if( direction == "L" || direction == "R"

						then
							xOffset = x_low - x_low_org
							yOffset = y_low - y_high_org

					) ; end if

					if( direction == "T" || direction == "B" 

						then
							xOffset = x_low - x_low_org
							yOffset = y_low - y_low_org
							dbMoveFig( d_closestNeighbor, nil, list(0:-yOffset "R0"))

					) ; end if

					dbMoveFig( d_SelectedCell, nil, list(xOffset:yOffset "R0"))

			) ;end if

		else
			println( "Error! You have selected more than ONE cell")	

	) ;end if

	; Return Value
	d_closestNeighbor

) ;procedure




