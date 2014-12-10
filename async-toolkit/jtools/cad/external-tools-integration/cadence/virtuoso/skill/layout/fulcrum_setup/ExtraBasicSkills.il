; This Fuction is resize the windowsimilar to "dbGetOverlaps" 
; except it will filter out completely abutted (aligned) cells
procedure( dbGetOverlapsNoAbutted( d_cellView )

	OverlappedCellList = dbGetOverlaps( d_cellView->cellView d_cellView->bBox nil)
	OverlappedCellListNoAbutted = '()

	x_low_org  = car( car( d_cellView->bBox ) )
	y_low_org  = car( cdr( car( d_cellView->bBox ) ) )
	x_high_org = car( car( cdr( d_cellView->bBox ) ) )
	y_high_org = car( cdr( car( cdr( d_cellView->bBox ) ) ) )

	foreach( OverlappedCell OverlappedCellList 

		; Get Coordinations
		x_low  = car( car( OverlappedCell->bBox ) )
		y_low  = car( cdr( car( OverlappedCell->bBox ) ) )
		x_high = car( car( cdr( OverlappedCell->bBox ) ) )
		y_high = car( cdr( car( cdr( OverlappedCell->bBox ) ) ) )

		; Set Filter to True by default
		FilterResult = "Filter"


		;Check for itself
		if( OverlappedCell == d_cellView

			then
				FilterResult = "NoFilter"
		) ;end if


		;Check for Abutment
		if(   (	x_low  == x_high_org ) ||
		      (	x_high == x_low_org  ) ||
		      (	y_low  == y_high_org ) ||
		      (	y_high == y_low_org  )

			then
				FilterResult = "NoFilter"
		) ;end if


		; Appending to the new List
		if( FilterResult == "Filter"

			then

				OverlappedCellListNoAbutted = cons( OverlappedCell OverlappedCellListNoAbutted ) 
		)


	) ;foreach

	; Return Value
	OverlappedCellListNoAbutted
	
) ;procedure




; This Fuction is an enhanced version of the dbGetNeighbor in the sense that
; it will ignore any abutted or non-instance type neighbors 
procedure( dbGetNeighborInstanceNoAbutted( d_cellView direction )

	cellNeighborList = dbGetNeighborList( d_cellView->cellView d_cellView->bBox direction )

	; x and y coordinates for the selected object
        x_low_org  = car( car( d_cellView->bBox ) )
        y_low_org  = car( cdr( car( d_cellView->bBox ) ) )
        x_high_org = car( car( cdr( d_cellView->bBox ) ) )
        y_high_org = car( cdr( car( cdr( d_cellView->bBox ) ) ) )

	; Initialize Variables
	x_min = abs( car( car( geGetWindowCellView()->bBox)) - car( car( cdr( geGetWindowCellView()->bBox ) ) ) )
	y_min = abs( car( cdr( car( geGetWindowCellView()->bBox))) - car( cdr( car( cdr( geGetWindowCellView()->bBox ) ) ) ) )
	d_closestNeighbor = nil

	foreach( cellNeighbor cellNeighborList

		d_cellNeighbor = car( cdr( cellNeighbor))

		if( !listp( d_cellNeighbor ) && ( d_cellNeighbor->objType == "inst")

			then
			        ; Get Coordinations
                		x_low  = car( car( d_cellNeighbor->bBox ) )
                		y_low  = car( cdr( car( d_cellNeighbor->bBox ) ) )
                		x_high = car( car( cdr( d_cellNeighbor->bBox ) ) )
                		y_high = car( cdr( car( cdr( d_cellNeighbor->bBox ) ) ) )

				if( ( direction == "L" &&  
				      y_low  != y_high_org &&
				      y_high != y_low_org )
					then
						x_diff = abs( x_low_org - x_high )

						if( x_diff < x_min
							then 
								d_closestNeighbor = d_cellNeighbor
								x_min = x_diff
						) ;end if

                           	)

				if( ( direction == "R" &&  
				      y_low  != y_high_org &&
				      y_high != y_low_org )
					then
						x_diff = abs( x_high_org - x_low )

						if( x_diff < x_min
							then 
								d_closestNeighbor = d_cellNeighbor
								x_min = x_diff
						) ;end if

                           	)

				if( ( direction == "T" &&  
				      x_low  != x_high_org &&
				      x_high != x_low_org )
					then
						y_diff = abs( y_high_org - y_low )

						if( y_diff < y_min
							then 
								d_closestNeighbor = d_cellNeighbor
								y_min = y_diff
						) ;end if

                           	)

				if( ( direction == "B" &&  
				      x_low  != x_high_org &&
				      x_high != x_low_org )
					then
						y_diff = abs( y_low_org - y_high )

						if( y_diff < y_min
							then 
								d_closestNeighbor = d_cellNeighbor
								y_min = y_diff
						) ;end if

                           	)


		) ;end if		

	) ;end foreach

	; Return Value
	d_closestNeighbor

) ;procedure



; This Fuction is used to sort a list of d_objects w.r.t. their instance names
; It does NOT do an error check on the input list (i.e. it always assume that the input is a list of dbObjects
procedure( dbSortDBList( l_d_cellView )

	let( (	genericPattern
		l_d_cellViewSorted)

	l_d_cellViewSorted = nil

;	genericPattern = "\\[N\\]$"
        genericPattern = "\\[N\\][\\.]*[a-zA-Z0-9]*$"

	while( l_d_cellView != nil

 		for( i 0 1024
			iString = sprintf( nil "%d" i)

			rexCompile( "N" )
			actualPattern = rexReplace( genericPattern iString 0 )

			rexCompile( actualPattern )

			foreach( d_cellView l_d_cellView

				if( rexExecute( d_cellView->name )

					then

						printf( "dbSortDBList --> Instance %s matches pattern %s \n" d_cellView->name actualPattern )
						l_d_cellView = remove( d_cellView l_d_cellView )
						l_d_cellViewSorted = cons( d_cellView l_d_cellViewSorted )

				); end if

			); end foreach

		); end for

		; In case the instance names do not match any of the pre-defined hashed table
		; Just append the rest of the string to the front (= back after reverse)
		l_d_cellViewSorted = nconc( l_d_cellView l_d_cellViewSorted )
		l_d_cellView = nil

	); end while

	reverse( l_d_cellViewSorted )

	); end let

); end procedure


; =============================================================================================
; This Fuction is used to flip an instance upside down
; It works on multiple # of instances
; This procedure assumes that the selected instances do not have any rotation "R90" "R180" etc
; =============================================================================================
procedure( dbFlipInstanceUD( )
	let( ( 
		y_low
		y_high
		y_origin
		bBox 
		cellHeight
		yOffset
		returnValue)
	
	returnValue = t

	l_d_cellView = geGetSelectedSet( )

	foreach( d_cellView l_d_cellView
		
		bBox = d_cellView->bBox
		y_low  = car( cdr( car( bBox)))
		y_high = car( cdr( car( cdr( bBox)))) 
		y_origin = car( cdr( d_cellView->xy ) )
		cellHeight = y_high - y_low
		yOffset = cellHeight - 2 * (y_origin - y_low) 


		case( d_cellView->orient

			( "MX"
				d_cellView->orient = "R0"
				dbMoveFig( d_cellView, nil, list(0:yOffset "R0"))
			)

			( "R0"
				d_cellView->orient = "MX"
				dbMoveFig( d_cellView, nil, list(0:yOffset "R0"))
			)

			( "R90"
				d_cellView->orient = "MYR90"
				dbMoveFig( d_cellView, nil, list(0:yOffset "R0"))
			)

			( "MYR90"
				d_cellView->orient = "R90"
				dbMoveFig( d_cellView, nil, list(0:yOffset "R0"))
			)

			( "R180"
				d_cellView->orient = "MY"
				dbMoveFig( d_cellView, nil, list(0:yOffset "R0"))
			)

			( "MY"
				d_cellView->orient = "R180"
				dbMoveFig( d_cellView, nil, list(0:yOffset "R0"))
			)

			( "R270"
				d_cellView->orient = "MXR90"
				dbMoveFig( d_cellView, nil, list(0:yOffset "R0"))
			)

			( "MXR90"
				d_cellView->orient = "R270"
				dbMoveFig( d_cellView, nil, list(0:yOffset "R0"))
			)

			( t
				returnValue = nil
			)

		); end case

	); end foreach

	returnValue

	); end let

); end procedure

procedure( dbFlipInstanceLR( )
	let( ( 
		x_low
		x_high
		x_origin
		bBox 
		cellWidth
		xOffset
		returnValue)
	
	returnValue = t

	l_d_cellView = geGetSelectedSet( )

	foreach( d_cellView l_d_cellView
		
		bBox = d_cellView->bBox
		x_low  = car( car( bBox))
		x_high = car( car( cdr( bBox)))
		x_origin = car( d_cellView->xy ) 
		cellWidth = x_high - x_low
		xOffset = cellWidth - 2 * (x_origin - x_low) 

		case( d_cellView->orient

			( "MY"
				d_cellView->orient = "R0"
				dbMoveFig( d_cellView, nil, list(xOffset:0 "R0"))
			)

			( "R0"
				d_cellView->orient = "MY"
				dbMoveFig( d_cellView, nil, list(xOffset:0 "R0"))
			)

			( "MYR90"
				d_cellView->orient = "R270"
				dbMoveFig( d_cellView, nil, list(xOffset:0 "R0"))
			)

			( "R270"
				d_cellView->orient = "MYR90"
				dbMoveFig( d_cellView, nil, list(xOffset:0 "R0"))
			)

			( "MX"
				d_cellView->orient = "R180"
				dbMoveFig( d_cellView, nil, list(xOffset:0 "R0"))
			)

			( "R180"
				d_cellView->orient = "MX"
				dbMoveFig( d_cellView, nil, list(xOffset:0 "R0"))
			)

			( "MXR90"
				d_cellView->orient = "R90"
				dbMoveFig( d_cellView, nil, list(xOffset:0 "R0"))
			)

			( "R90"
				d_cellView->orient = "MXR90"
				dbMoveFig( d_cellView, nil, list(xOffset:0 "R0"))
			)

			( t
				returnValue = nil
			)

		); end case
	 
	); end foreach

	returnValue

	); end let

); end procedure


; This function is a wrap around of the dbFlipInstance
; it will flip it in unit of a group
procedure( DB_FlipGroup( direction )

	let( ( 	l_d_cellView 
		libName 
		cellName
		viewName
		d_tempCell
		returnValue
		)
	
	l_d_cellView = geGetSelectedSet( )
	libName  = geGetWindowCellView()->libName
	cellName = "tempCell"
	viewName = "layout"
	returnValue = t

		leMakeCell( l_d_cellView libName cellName viewName t )

		geSelectAllFig( geGetWindowCellView())
		l_d_cellView = geGetSelectedSet( )
		
		foreach( d_cellView l_d_cellView
			if( d_cellView->cellName != cellName
				then
					geDeselectFig( d_cellView )
				else
					d_tempCell = d_cellView
			);end if
		); end foreach

		if( direction == "UD"
			then
				dbFlipInstanceUD( )
			else
				dbFlipInstanceLR( )
		);end if

		dbFlattenInst( d_tempCell 1 )

		ddDeleteLocal( ddGetObj( libName cellName viewName )  )

	returnValue

	); end let

); end procedure


; =============================================================
; Simple Move Procedure Similar to MAGIC Move Command
; =============================================================
procedure( Move( direction value @key ( DebugOn nil ))

	let((	returnValue
		l_d_Selected
		)

	returnValue = t
	l_d_Selected = geGetSelectedSet( )

	foreach( d_Selected l_d_Selected
		case( direction
			( "l" 
				dbMoveFig( d_Selected nil list( -value:0 "0" ))
			)
			( "r" 
				dbMoveFig( d_Selected nil list( value:0 "0" ))
			)
			( "u" 
				dbMoveFig( d_Selected nil list( 0:value "0" ))
			)
			( "d" 
				dbMoveFig( d_Selected nil list( 0:-value "0" ))
			)
		); end case
	); end foreach


	); end let

); end procedure 

procedure( ML( value )

	Move( "l" value )

); end procedure

procedure( MR( value )

	Move( "r" value )

); end procedure

procedure( MU( value )

	Move( "u" value )

); end procedure

procedure( MD( value )

	Move( "d" value )

); end procedure


; =============================================================
; Simple Move Procedure Similar to MAGIC Copy Command
; =============================================================
procedure( Copy( direction value @key ( DebugOn nil ))

	let((	returnValue
		l_d_Selected
		d_Selected
		)

	returnValue = t
	l_d_Selected = geGetSelectedSet( )

	case( direction
		( "l" 
			geCopySelSet(geGetCellViewWindow(geGetWindowCellView()) list(0 0) list(-value 0))
		)
		( "r" 
			geCopySelSet(geGetCellViewWindow(geGetWindowCellView()) list(0 0) list(value 0))
		)
		( "u" 
			geCopySelSet(geGetCellViewWindow(geGetWindowCellView()) list(0 0) list(0 value))
		)
		( "d" 
			geCopySelSet(geGetCellViewWindow(geGetWindowCellView()) list(0 0) list(0 -value))
		)
	); end case

	returnValue

	); end let

); end procedure 

procedure( CL( value )

	Copy( "l" value )

)
procedure( CR( value )

	Copy( "r" value )

)
procedure( CU( value )

	Copy( "u" value )

)
procedure( CD( value )

	Copy( "d" value )

)
