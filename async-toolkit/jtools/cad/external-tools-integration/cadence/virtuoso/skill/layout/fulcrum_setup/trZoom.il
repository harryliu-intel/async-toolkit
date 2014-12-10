trX = hiCreateStringField( 
	?name 'trX 
	?prompt "Enter x-value" 
	?defValue "23.4" 
	?editable t )

trY = hiCreateStringField( 
	?name 'trY 
	?prompt "Enter y-value" 
	?defValue "12" 
	?editable t )

trRadius = hiCreateStringField( 
	?name 'trRadius 
	?prompt "Enter Zoom Radius" 
	?defValue "14" 
	?editable t )

trLabel = hiCreateMLTextField( 
	?name `trLabel
	?prompt "Enter Label (Optional)" )

trColor = hiCreateRadioField( 
	?name 'trColor 
	?choices '( "White" "Magenta" "Orange" "Lime" "Pink" )
	?prompt "Enter desired Color to add"
)

trErase = hiCreateToggleField( 
	?name 'trErase 
	?choices list( 	list( `ER1 "White") 
		 	list( `ER2 "Magenta")
			list( `ER3 "Orange")
			list( `ER4 "Lime")
			list( `ER5 "Pink") 
			list( `ER6 "Erase All"))
	?prompt "Enter desired Color to erase"
)

procedure( trZoom( theForm )

	let( ( 	x x_low x_high
	 	y y_low y_high
		Radius
		ER
		HiliteSet)

		x 	= evalstring( theForm->trX->value )
		y 	= evalstring( theForm->trY->value )
		Radius 	= evalstring( theForm->trRadius->value )
		ER1	= theForm->trErase->ER1->value
		ER2	= theForm->trErase->ER2->value
		ER3	= theForm->trErase->ER3->value
		ER4	= theForm->trErase->ER4->value
		ER5	= theForm->trErase->ER5->value
		ER6	= theForm->trErase->ER6->value
		Label   = theForm->trLabel->value
		Color 	= theForm->trColor->value	

		; If User wants to erase all markers
		if( ER1 || ER2 || ER3 || ER4 || ER5 || ER6
			then
				if( ER1
					then
						boundp( `setWhite )   &&  geIsValidHilightSet( evalstring( "setWhite" )) && geDeleteHilightSet( evalstring( strcat( "set" "White" ) ) )
						printf( "trZoom --> White markers erased!!! \n")
				) ; end if

				if( ER2
					then
						boundp( `setMagenta ) &&  geIsValidHilightSet( evalstring( "setMagenta" )) &&  geDeleteHilightSet( evalstring( strcat( "set" "Magenta" ) ) )
						printf( "trZoom --> Magenta markers erased!!! \n")
				) ; end if

				if( ER3
					then
						boundp( `setOrange )  &&  geIsValidHilightSet( evalstring( "setOrange" )) &&  geDeleteHilightSet( evalstring( strcat( "set" "Orange" ) ) )
						printf( "trZoom --> Orange markers erased!!! \n")
				) ; end if

				if( ER4 
					then
						boundp( `setLime )    &&  geIsValidHilightSet( evalstring( "setLime" )) &&  geDeleteHilightSet( evalstring( strcat( "set" "Lime" ) ) )
						printf( "trZoom --> Lime markers erased!!! \n")
				) ; end if

				if( ER5
					then
						boundp( `setPink )    &&  geIsValidHilightSet( evalstring( "setPink" )) &&  geDeleteHilightSet( evalstring( strcat( "set" "Pink" ) ) )
						printf( "trZoom --> Pink  markers erased!!! \n")
				) ; end if

				if( ER6
					then
						boundp( `setWhite )   &&  geIsValidHilightSet( evalstring( "setWhite" )) && geDeleteHilightSet( evalstring( strcat( "set" "White" ) ) )
						boundp( `setMagenta ) &&  geIsValidHilightSet( evalstring( "setMagenta" )) &&  geDeleteHilightSet( evalstring( strcat( "set" "Magenta" ) ) )
						boundp( `setOrange )  &&  geIsValidHilightSet( evalstring( "setOrange" )) &&  geDeleteHilightSet( evalstring( strcat( "set" "Orange" ) ) )
						boundp( `setLime )    &&  geIsValidHilightSet( evalstring( "setLime" )) &&  geDeleteHilightSet( evalstring( strcat( "set" "Lime" ) ) )
						boundp( `setPink )    &&  geIsValidHilightSet( evalstring( "setPink" )) &&  geDeleteHilightSet( evalstring( strcat( "set" "Pink" ) ) )
						printf( "trZoom --> All markers erased!!! \n")
				) ; end if
			else

				printf( "trZoom --> Zoom to ( %n , %n ) with a radius of %n \n" x y Radius)

				x_low  = x - Radius
				y_low  = y - Radius
				x_high = x + Radius
				y_high = y + Radius
				boundBox = list( x_low:y_low  x_high:y_high )
				pathList = list( x:y 
						 x:(y + (y_high-y)/2) 
						 (x + (x_high-x)/2):y 
						 x:y 
						 x_high:y_high )

				; Setting the drawType

				case( 
					Color

					( "White"
						drawType = "drawing"
					)

					( "Magenta"
						drawType = "drawing1"
					)

					( "Orange"
						drawType = "drawing4"
					)

					( "Lime"
						drawType = "drawing5"
					)

					( "Pink"
						drawType = "drawing9"
					)
				); end case

				; Create a New Hilite Group
				if( boundp( concat( "set" Color )) == nil || geIsValidHilightSet( evalstring( strcat( "set" Color ))) == nil
				;	if( boundp( concat( "set" Color )) == nil
					then
						printf( "trZoom --> Hilite Set \"set%s\" created!!! \n" Color )
						evalstring( strcat( "set" Color "= geCreateHilightSet( geGetWindowCellView( ) list(\"hilite\" drawType) nil )" ))

				); end if

				; Set the current Hilite Group
				setCurrent = evalstring( strcat( "set" Color ) )
				setCurrent->enable = t

				geAddHilightDot( setCurrent x:y 0.48 0.48 )
				geAddHilightCircle( setCurrent x:y Radius )
				geAddHilightLabel( setCurrent list(x y+Radius) Label "upperCenter" "R0" "stick" max( Radius/4 2 ) t )


				hiZoomIn( geGetCellViewWindow( geGetWindowCellView( )) boundBox )
   				leZoomToPoint(hiGetCurrentWindow(), x:y)
	;			leCreatePath( geGetWindowCellView() list("hilite" drawType) pathList 0.01 )
		); end if

	) ; end let

) ; procedure

	;	hiDeleteForm( A )

A = hiCreateAppForm( 
	?name 'trZoomForm
	?fields  '( trX trY trRadius trLabel trColor trErase)
	?callback "trZoom( hiGetCurrentForm() )" 
)







