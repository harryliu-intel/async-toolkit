; Work on Selected Cells Only
; Return Value = x:y relative to the input x:y

procedure( FloorplanCells( x y
				@key
                        		( Dimension 		0 )
					( Type			"Data" )
					( Orientation		"NA" )
					( HashList      	nil )
					( DebugOn   		nil )
                        		( BitPitch		0 )
					( DatapathHeight        0 )
					( NumBitsPerCell       	0 )
			)

	; local Varibales
	let( ( 	l_d_SelectedCell 
		xOffset x_Selected x_Multiplier x_Shifter xHashList 
		yOffset y_Selected y_Multiplier y_Shifter yHashlist yCellHeight yMaxBitNum
		width height
		num_column
		num_row
		max_Index
		max_Height
		returnValue)

		num_column = 1
		num_row    = 1
		l_d_SelectedCell = geGetSelectedSet( )

		x_Multiplier = 1
		x_Shifter = 0

		; Make sure that all indices increases in positive x and positive y direction
		; ===========================================================================
		xHashList = car( HashList )
		yHashList = cadr( HashList )
		if( Type == "Ctrl"
			then
				case( car( yHashList )
					( nil
						yHashList = list( -1 )
					)
					( -1
						yHashList = nil
					)
				); end case
		); end if




		x_Shifter2 = 0
		y_Shifter2 = 0

		DebugOn && printf( "FloorplanCells --> Strating Procedure xHashList=%L yHashList=%L \n" xHashList yHashList)


		; Deals with Horizontal Hashing 
		if( xHashList != nil
			then
				case( car( xHashList )
					( -1
						DebugOn && printf( "FloorplanCells --> Inverting x-arrays for %s\n" car( l_d_SelectedCell )->name )
						x_Multiplier = -1
						x_Shifter  = 1
						x_Shifter2 = 1
					)		
				); end case
		); end if

		; Deals with Vertical Hashing 
		if( yHashList != nil
			then
				case( car( yHashList )
					( -1
						DebugOn && printf( "FloorplanCells --> Inverting y-arrays for %s\n" car( l_d_SelectedCell )->name )
						case( Type 
							("Data"
								y_Multiplier = -1
								y_Shifter = 1
								y_Shifter2 = 1
							)
							("Ctrl"
								y_Multiplier = 1
								y_Shifter = 0
								y_Shifter2 = -1
							)
						)
					)		
				); end case
			else
				case( Type

					( "Data"
						y_Multiplier = 1
						y_Shifter = 0
					)
	
					( "Ctrl"
						y_Multiplier = -1
						y_Shifter = 1
					)

				)				
		)

		; =======================================================================================================================================
		; SMART Floorplanning
		; Setting the CellHeight
		; =======================================================================================================================================
		yMaxBitNum = cadr( GetMaxIndex( l_d_SelectedCell ?Orientation Orientation )) + 1

		if( BitPitch > 0 && Type == "Data" && yMaxBitNum > 1 && NumBitsPerCell == 0 
			then
				NumBitsPerCell = round( float( DatapathHeight ) / ( yMaxBitNum * BitPitch ) ) 
		)

		if( BitPitch > 0 && Type == "Data" 
			then
				yCellHeight = max( BitPitch BitPitch*NumBitsPerCell cadadr( car( l_d_SelectedCell )->bBox ) - cadar( car( l_d_SelectedCell )->bBox ))
			else
				yCellHeight = cadadr( car( l_d_SelectedCell )->bBox ) - cadar( car( l_d_SelectedCell )->bBox )
		)

		foreach( d_SelectedCell l_d_SelectedCell 

	 	        x_Selected  = caar( d_SelectedCell->bBox )
		        y_Selected  = cadar( d_SelectedCell->bBox )

			xOffset = x - x_Selected
		       	yOffset = y - y_Selected

			xOffset = xOffset + ( car( GetIndex( d_SelectedCell->name ?Orientation Orientation ) ) + x_Shifter ) * x_Multiplier *  
						( caadr( d_SelectedCell->bBox ) - caar( d_SelectedCell->bBox ))

			yOffset = yOffset + ( cadr( GetIndex( d_SelectedCell->name ?Orientation Orientation ) ) + y_Shifter ) * y_Multiplier * yCellHeight

			num_column = max( num_column car( GetIndex( d_SelectedCell->name ?Orientation Orientation ))+1 )
			num_row    = max( num_row    cadr( GetIndex( d_SelectedCell->name ?Orientation Orientation ))+1 )

       			dbMoveFig( d_SelectedCell, nil, list(xOffset:yOffset "R0"))

		); end foreach

		width = ( caadr( car( l_d_SelectedCell )->bBox ) - caar( car( l_d_SelectedCell )->bBox ) ) * num_column
		height = yCellHeight * num_row

					
		foreach( d_SelectedCell l_d_SelectedCell 
			dbMoveFig( d_SelectedCell, nil, list(width*x_Shifter2:height*y_Shifter2 "R0"))
		)

	; Return Value
	width

	); end let


); end procedure


;=========================================================================================================
; This procedure get the index list of a particular string in the form of list( x y z )
;     where x = horizontal index
;           y = vertical index
;           z = UNUSED 
;=========================================================================================================
procedure( GetIndex( String @key (Orientation "NA" ))

	let( (	l_Substring
		l_returnIndex )

	l_Substring   = parseString( String "[]" )
	l_returnIndex = nil


	foreach( Substring l_Substring

		rexCompile( "[0-9\\-]" )
		if( rexExecute( Substring )
			then

			rexCompile( "[a-zA-Z]" )
			if( !rexExecute( Substring )
				then
					l_returnIndex = reverse( cons( evalstring( Substring ) reverse( l_returnIndex )))
			); end if
	
		); end if

	); end foreach

	; Post-processing
	; Supports 3-D i.e list( a b c )
	case( length( l_returnIndex )

		(0
			l_returnIndex = list( 0 0 0 )
		)

		(1
			case( Orientation
				( "V"
					l_returnIndex = list( 0 car(l_returnIndex) 0 )
				)
				( "H"
					l_returnIndex = list( car(l_returnIndex) 0 0 )
				)
				( t
					l_returnIndex = list( 0 car(l_returnIndex) 0 )
				)
			)
		)

		(2
			case( Orientation
				( "HV"
					l_returnIndex = list( car(l_returnIndex) cadr(l_returnIndex) 0 )
				)
				( "VH"
					l_returnIndex = list( cadr(l_returnIndex) car(l_returnIndex) 0 )
				)
				( t
					l_returnIndex = list( car(l_returnIndex) cadr(l_returnIndex) 0 )
				)
			)
		)

	); end case

	l_returnIndex

	); end let

); end procedure


;=========================================================================================================
; This procedure get the total number of horizontal and vertical cells in the given cell list
; Return Value is in the form of list( x y z )
;     where x = number of horizontal cells 
;           y = number of vertical cells
;           z = UNUSED 
;=========================================================================================================
procedure( GetMaxIndex( l_d_Cell @key (Orientation "NA" ) ( DebugOn nil ))

	let((	l_returnValue
		l_tempValue
		)

	l_returnValue = list( 0 0 0 )

	DebugOn && printf( "=============================================================\n" )
	foreach( d_Cell l_d_Cell

		l_tempValue = GetIndex( d_Cell->name ?Orientation Orientation )
		DebugOn && printf( "GetMaxIndex --> Processing \"%s\" => list%L \n" d_Cell->name l_tempValue )
	
		l_returnValue = list( 	max( car( l_returnValue )  car( l_tempValue )) 
					max( cadr( l_returnValue ) cadr( l_tempValue ))
					0
					)		
	); end foreach

	DebugOn && printf( "GetMaxIndex --> l_returnValue = list%L \n" l_returnValue )
	DebugOn && printf( "=============================================================\n" )

	l_returnValue

	); end let

); end procedure
