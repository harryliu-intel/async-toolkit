; This Fuction is used to align all the selected cells on to the power grids
procedure( AlignCells( xGrid yGrid xOffset yOffset )

	SelectedCellList = geGetSelSet( )

	foreach( SelectedCell SelectedCellList 

               if( SelectedCell->objType == "inst"
                        
                        then

				xCord = car( SelectedCell -> xy )
				yCord = car( cdr( SelectedCell -> xy ))

				; Move to negative x direction
				xDiff1 = (( xCord / xGrid ) - floor( xCord / xGrid )) * xGrid

				; Move to negative y direction
				yDiff1 = (( yCord / yGrid ) - floor( yCord / yGrid )) * yGrid 

				; Move to positive x direction
				xDiff2 = (( xCord / xGrid ) - floor( ( xCord / xGrid ) + 1 )) * xGrid

				; Move to positive y direction
				yDiff2 = (( yCord / yGrid ) - floor( ( yCord / yGrid ) + 1 )) * yGrid 

				; Find the closest
				if( abs( xDiff1 ) > abs( xDiff2 ) 
					then
						xDiff = xDiff2
					else
						xDiff = xDiff1
				)
				if( yDiff1 > abs( yDiff2 ) 
					then
						yDiff = yDiff2
					else
						yDiff = yDiff1
				)
	
				dbMoveFig( SelectedCell, nil, list(-xDiff:-yDiff "R0"))
				dbMoveFig( SelectedCell, nil, list(xOffset:yOffset "R0"))
	         
			else

				geDeselectObject( SelectedCell )
		) ;if

	) ;foreach


) ;procedure



