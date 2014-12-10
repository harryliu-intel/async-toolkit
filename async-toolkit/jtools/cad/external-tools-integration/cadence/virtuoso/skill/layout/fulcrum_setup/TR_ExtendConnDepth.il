


procedure( TR_ExtendConnDepth_ProcMain( @key 
					( WorkOnAll	nil )
					( ConnDepth	2 )
					)

	let((	l_Cell	Cell
		)

	if( WorkOnAll
		then
			geSelectAllFig( )
	); end if

	l_Cell = setof( x geGetSelectedSet( ) x->net->name && !member( x->net->name list( "Vdd" "GND" )))

	geDeselectAllFig( )

	foreach( Cell l_Cell
;		println( Cell->net->name)
		geSelectFig( Cell )
		TR_ExtendConnDepth_ProcProcessOnePin( ?d_Pin Cell ?ConnDepth ConnDepth )
	); end foreach

	t

	); end let

); end procedure




procedure( TR_ExtendConnDepth_ProcProcessOnePin( @key
				( d_Pin		car(geGetSelectedSet()))
				( ConnDepth	2 )
				)

	let((	l_Overlapped Overlapped
		)

	l_Overlapped = dbGetTrueOverlaps( 	geGetWindowCellView( )
						d_Pin->bBox 
						car( d_Pin->lpp )
						ConnDepth
						)

	foreach( Overlapped l_Overlapped

		TR_ExtendConnDepth_ProcDrawShape( Overlapped car( d_Pin->lpp ) 0:0 1 1 d_Pin->net->name )

	); end foreach
	
;	println( l_Overlapped )

	t

	); end let

); end procedure

procedure( TR_ExtendConnDepth_ProcDrawShape( 	d_object 
						layer
						xyOffset 
						xMultiplier
						yMultiplier
						pinName
						@key	( DebugOn	nil )
						)

	let((	l_Overlapped Overlapped Parent
		XL XH YL YH
		currentbBox currentPath
		xOffset yOffset
		d_Pin d_Net
		l_point point
		PinLayer PinPurpose
		)

	xOffset = car( xyOffset )
	yOffset = cadr( xyOffset )

	; If the input object is a shape
	; ==============================
	if( car( d_object->lpp ) == layer && member( d_object->objType list( "path" "rect" ))
		then
			XL = xMultiplier*caar( d_object->bBox ) + xOffset
			XH = xMultiplier*caadr( d_object->bBox ) + xOffset
			YL = yMultiplier*cadar( d_object->bBox ) + yOffset
			YH = yMultiplier*cadadr( d_object->bBox ) + yOffset

			currentbBox = list( XL:YL XH:YH )

			DebugOn && printf( "TR_ExtendConnDepth_ProcDrawShape --> %L \n" currentbBox )


			PinLayer   = layer
			PinPurpose = "drawing" 

			; First, draw the pin
			case( d_object->objType 
				( "rect" 
					d_Pin = dbCreateRect( geGetWindowCellView( ) list( PinLayer PinPurpose ) currentbBox )
				)
				( "path"

					l_point = d_object->points
					currentPath = nil

					foreach( point l_point
						currentPath = cons( list( 	xMultiplier*car(point) + xOffset 
										yMultiplier*cadr(point) + yOffset )
									currentPath )
					); end foreach

					d_Pin = dbCreatePath( geGetWindowCellView( ) list( PinLayer PinPurpose ) currentPath d_object->width)
				)
			); end case

			; Second, Fill up Connectivity
			d_Net = dbMakeNet( geGetWindowCellView() pinName )
			dbCreatePin( d_Net d_Pin )

	); end if


	; If the input object is a list ==> need to further tranverse the tree
	; ====================================================================
	if( listp( d_object ) && !member( car( d_object )->objType list( "mosaic" "mosaicInst" ))
		then

			l_Overlapped = cdr( d_object )
			Parent = car( d_object )

			xOffset = xMultiplier*car( Parent->xy ) + xOffset
			yOffset = yMultiplier*cadr( Parent->xy ) + yOffset

			case( Parent->orient 

				( "MY" 
					xMultiplier = -xMultiplier
					yMultiplier = yMultiplier

				); end MY

				( "MX" 
					xMultiplier = xMultiplier
					yMultiplier = -yMultiplier

				); end MX

				( "R0"
					xMultiplier = xMultiplier
					yMultiplier = yMultiplier
				)					

				( t
					DebugOn && 
					printf( "TR_ExtendConnDepth_ProcDrawShape --> Orientation \"%s\" NOT Supported\n"
						Parent->orient
						)
				)

			); end case

			foreach( Overlapped l_Overlapped
				TR_ExtendConnDepth_ProcDrawShape( Overlapped layer xOffset:yOffset xMultiplier yMultiplier pinName )
			); end foreach


	); end if

	t

	); end let

); end procedure







