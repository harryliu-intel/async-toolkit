; Global Variables
; ================
PathFinder_GlobalRectList = nil



procedure( PathFinder( @key 
			( layer		"METAL2")
			( purpose	"drawing" )
			( direction	"vertical")
			( width		0.24 )
			( spacing	0.24 )
			( viaSpacing	0.215 ) 
			( searchDepth 	32 )
			( resolution	0 )
			( onPowerGrid	nil )
			( bBox		geGetWindowCellView( )->bBox )
			( numTracks 	0 )
			( inverse	nil )
			( exceptionList	nil )
			( DebugOn 	t )
			)

	let((	x_low x_high x_start x_end xDirectionFactor
		y_low y_high y_start y_end yDirectionFactor
		bBox_current
		isFinish
		AnyOverlaps AnyOverlapsTmp
		returnValue 
		layoutResolution
		DeltaError
		Counter
		Track_bBox
		l_ContactCells
		)

	; Local Variables
	; ===============
	DeltaError = 0.001
	isFinish = nil
	returnValue = nil
	layoutResolution = 0.01
	Counter = 0
	Track_bBox = nil
	l_ContactCells = list( 	"M8_M7" "M7_M6" "M6_M5" "M5_M4" "M4_M3" "M3_M2" "M2_M1"
				"M8_M7_min" "M7_M6_min" "M6_M5_min" "M5_M4_min" "M4_M3_min" "M3_M2_min" "M2_M1_min"
				)
	

	case( direction
		( "vertical"
			if( !inverse
				then
					xDirectionFactor = 1
					yDirectionFactor = 0
				else
					xDirectionFactor = -1
					yDirectionFactor = 0
			)
		)
		( "horizontal"
			if( !inverse
				then
					xDirectionFactor = 0
					yDirectionFactor = 1
				else
					xDirectionFactor = 0
					yDirectionFactor = -1
			)
	
		)
		( t
			xDirectionFactor = 0
			yDirectionFactor = 0	
			isFinish = t
			printf( "PathFinder --> FATAL ERROR!!! <?direction \"%s\"> is NOT an acceptable input\n" direction ) 
		)
	); end case
	
	;============================================================================================

	x_start = round( float( caar( bBox )) / layoutResolution ) * layoutResolution
	x_end   = round( float( caadr( bBox )) / layoutResolution ) * layoutResolution

	y_start = round( float( cadar( bBox )) / layoutResolution ) * layoutResolution
	y_end   = round( float( cadadr( bBox )) / layoutResolution ) * layoutResolution

	;============================================================================================

	if( !inverse
		then
			x_low  = x_start + DeltaError
			y_low  = y_start + DeltaError

			x_high = xDirectionFactor * ( x_start + width + spacing * 2 ) + yDirectionFactor * x_end - DeltaError
			y_high = yDirectionFactor * ( y_start + width + spacing * 2 ) + xDirectionFactor * y_end - DeltaError
		else
			x_low  = -xDirectionFactor * ( x_start + DeltaError ) - yDirectionFactor * ( x_end - width - spacing * 2 ) - DeltaError
			y_low  = -yDirectionFactor * ( y_start + DeltaError ) - xDirectionFactor * ( y_end - width - spacing * 2 ) - DeltaError

			x_low  = ( -yDirectionFactor * ( x_start + DeltaError ) ) - ( xDirectionFactor * ( x_end - width - spacing * 2 ) - DeltaError )
			y_low  = ( -xDirectionFactor * ( y_start + DeltaError ) ) - ( yDirectionFactor * ( y_end - width - spacing * 2 ) - DeltaError )

			x_high = x_end - DeltaError
			y_high = y_end - DeltaError
	)

	bBox_current = list( x_low:y_low x_high:y_high )



	; Check Extreme Cases
	if( !inverse && ( x_high > x_end || y_high > y_end )
		then
			isFinish = t
	)

	if( inverse && ( x_low < x_start || y_low < y_start )
		then
			isFinish = t
	)











	; Auto-resolution setup
	if( resolution == 0
		then
			resolution = ( abs( xDirectionFactor ) * abs( x_end - x_start ) + abs( yDirectionFactor ) * abs( y_end - y_start ) )/ 500

			resolution = round( float( resolution ) / layoutResolution ) * layoutResolution

			if( resolution == 0
				then
					resolution = 0.01
			)

			resolution = min( width + spacing * 2  resolution )

			DebugOn && printf( "PathFinder --> Resolution is automatically set to %num\n" resolution )
		else
			DebugOn && printf( "PathFinder --> Resolution is set to input value %num\n" resolution )
	)

	CreateRectangleHilite( layer purpose geGetWindowCellView( ) ) 

	while( !isFinish

		if( onPowerGrid
			then
				case( layer
					( "METAL3"
						if( direction == "horizontal" && ( floor( y_low /4.8 ) - floor( y_high / 4.8 )) !=0 
							then
								if( !inverse
									then
										y_low  = ( ceiling( y_low /4.8 ) * 4.8 ) + 0.36 + DeltaError
									else
										y_low  = ( ceiling( y_low /4.8 ) * 4.8 ) - 0.36 - width - 2 * spacing + DeltaError
								); end if

								y_high = y_low + width + 2 * spacing  - 2 * DeltaError
								bBox_current = list( x_low:y_low x_high:y_high )

						); end if
					)

					( "METAL5"
						if( direction == "horizontal" && ( floor( y_low /4.8 ) - floor( y_high / 4.8 )) !=0 
							then
								if( !inverse
									then
										y_low  = ( ceiling( y_low /4.8 ) * 4.8 ) + 0.36 + DeltaError
									else
										y_low  = ( ceiling( y_low /4.8 ) * 4.8 ) - 0.36 - width - 2 * spacing + DeltaError
								); end if

								y_high = y_low + width + 2 * spacing  - 2 * DeltaError
								bBox_current = list( x_low:y_low x_high:y_high )

						); end if
					)

					( "METAL7"
						if( direction == "horizontal" && ( floor( y_low /4.8 ) - floor( y_high / 4.8 )) !=0 
							then
								if( !inverse
									then
										y_low  = ( ceiling( y_low /4.8 ) * 4.8 ) + 0.60 + DeltaError
									else
										y_low  = ( ceiling( y_low /4.8 ) * 4.8 ) - 0.60 - width - 2 * spacing + DeltaError
								); end if

								y_high = y_low + width + 2 * spacing  - 2 * DeltaError
								bBox_current = list( x_low:y_low x_high:y_high )

						); end if
					)

					( "METAL4"
						if( direction == "vertical" && ( floor( x_low /2.4 ) - floor( x_high / 2.4 )) !=0 
							then
								if( !inverse
									then
										x_low  = ( ceiling( x_low /2.4 ) * 2.4 ) + 0.12 + DeltaError
									else
										x_low  = ( ceiling( x_low /2.4 ) * 2.4 ) - 0.12 - width - 2 * spacing + DeltaError
								); end if

								x_high = x_low + width + 2 * spacing  - 2 * DeltaError
								bBox_current = list( x_low:y_low x_high:y_high )

						); end if
					)

					( "METAL6"
						if( direction == "vertical" && ( floor( x_low /4.8 ) - floor( x_high / 4.8 )) !=0 
							then
								if( !inverse
									then
										x_low  = ( ceiling( x_low /4.8 ) * 4.8 ) + 0.36 + DeltaError
									else
										x_low  = ( ceiling( x_low /4.8 ) * 4.8 ) - 0.36 - width - 2 * spacing + DeltaError
								); end if

								x_high = x_low + width + 2 * spacing  - 2 * DeltaError
								bBox_current = list( x_low:y_low x_high:y_high )

						); end if
					)

				); end case
		); end if

		geRefresh( );

		; Get the Overlapping objects in a TREE List Structure
		; ====================================================
		AnyOverlaps = dbGetTrueOverlaps( geGetWindowCellView( ) bBox_current list( layer nil ) searchDepth )



		; This essentially flatten the return list structure
		; ==================================================
		for( i 0 32
			AnyOverlapsTmp = nil

			foreach( Cell AnyOverlaps

				if( listp( Cell )
					then
						if( !member( car( Cell )->cellName l_ContactCells )
							then
								AnyOverlapsTmp = append( AnyOverlapsTmp cdr( Cell ))
							else
								AnyOverlapsTmp = append( AnyOverlapsTmp list(car( Cell )))
						); end if

					else
						AnyOverlapsTmp = append( AnyOverlapsTmp list( Cell ))
				)

			); end foreach

			if( AnyOverlaps == AnyOverlapsTmp
				then
					i = 33
				else
					AnyOverlaps = AnyOverlapsTmp
			)

		); end for


		; Check for overlapping objects (filter out ONLY labels)
		; ======================================================
		AnyOverlaps = setof( x AnyOverlaps ( x->objType!="textDisplay" && x->objType!="label" && !member( x exceptionList )))



		; In case there are overlaps but are caused just by contact cells
		; ===============================================================
		if( AnyOverlaps && setof( x AnyOverlaps !member( x->cellName l_ContactCells)) == nil && spacing!=viaSpacing
			then
				case( direction
					( "vertical"
						bBox_temp = list(
								list(
									caar( bBox_current ) + spacing - viaSpacing  
									cadar( bBox_current )
									)
								list(
									caadr( bBox_current ) - spacing + viaSpacing  
									cadadr( bBox_current )
									)
								)
					)
					( "horizontal"
						bBox_temp = list(
								list(
									caar( bBox_current )  
									cadar( bBox_current ) + spacing - viaSpacing
									)
								list(
									caadr( bBox_current )  
									cadadr( bBox_current ) - spacing + viaSpacing
									)
								)
					)
				); end case

				AnyOverlaps = dbGetTrueOverlaps( geGetWindowCellView( ) bBox_temp list( layer nil ) searchDepth )

				; This essentially flatten the return list structure
				; ==================================================
				for( i 0 32
					AnyOverlapsTmp = nil

					foreach( Cell AnyOverlaps

						if( listp( Cell )
							then
								if( !member( car( Cell )->cellName l_ContactCells )
									then
										AnyOverlapsTmp = append( AnyOverlapsTmp cdr( Cell ))
									else
										AnyOverlapsTmp = append( AnyOverlapsTmp list(car( Cell )))
								); end if

							else
								AnyOverlapsTmp = append( AnyOverlapsTmp list( Cell ))
						)

					); end foreach

					if( AnyOverlaps == AnyOverlapsTmp
						then
							i = 33
						else
							AnyOverlaps = AnyOverlapsTmp
					)

				); end for


				; Check for overlapping objects (filter out ONLY labels)
				; ======================================================
				AnyOverlaps = setof( x AnyOverlaps ( x->objType!="textDisplay" && x->objType!="label" && !member( x exceptionList )))

				
		); end if







		DebugOn && !AnyOverlaps && printf( "PathFinder --> %3n (%s, %s) path found at %L\n"
						Counter + 1 
						layer
						purpose
						bBox_current ) 

		if( !AnyOverlaps
			then

				Counter = Counter + 1

				if( Counter == numTracks
					then
						isFinish = t
				); end if

				Track_bBox = list( list( 	round( ( x_low  + abs( xDirectionFactor ) * spacing - DeltaError ) / layoutResolution ) * layoutResolution
								round( ( y_low  + abs( yDirectionFactor ) * spacing - DeltaError ) / layoutResolution ) * layoutResolution
								)
						   list(	round( ( x_high - abs( xDirectionFactor ) * spacing + DeltaError ) / layoutResolution ) * layoutResolution
								round( ( y_high - abs( yDirectionFactor ) * spacing + DeltaError ) / layoutResolution ) * layoutResolution
								)
						)

				DrawRectangleHilite( 	layer 
							purpose 
							Track_bBox
							geGetWindowCellView( )
							)				

				x_low  = x_low  + xDirectionFactor * ( width + spacing )
			 	x_high = x_high + xDirectionFactor * ( width + spacing )
				y_low  = y_low  + yDirectionFactor * ( width + spacing )
			 	y_high = y_high + yDirectionFactor * ( width + spacing )

				returnValue = cons( Track_bBox returnValue )
			else
				x_low  = x_low  + xDirectionFactor * resolution
			 	x_high = x_high + xDirectionFactor * resolution 
				y_low  = y_low  + yDirectionFactor * resolution
			 	y_high = y_high + yDirectionFactor * resolution 
				
		)

		bBox_current = list( x_low:y_low x_high:y_high )

		if( !inverse && ( x_high > x_end || y_high > y_end )
			then
				isFinish = t
		)

		if( inverse && ( x_low < x_start || y_low < y_start )
			then
				isFinish = t
		)
				
	); end while

	DebugOn && printf( "PathFinder --> %L\n" reverse( returnValue ))
	PathFinder_GlobalRectList = reverse( returnValue )

	); end let

); end procedure


; ============================================================================================
; This procedure is used to create a hilite rectange group and do nothing
; ============================================================================================

procedure( CreateRectangleHilite( layer purpose d_CellView )

	gePushHilightStack( geCreateHilightSet( d_CellView list( layer purpose ) t ))
	geGetCurrentHilightSet( geGetWindowCellView())->enable = t
)



; ============================================================================================
; This procedure is used to draw a hilite rectange using the specified lpp 
; ============================================================================================

procedure( DrawRectangleHilite( layer purpose l_bBox d_CellView )

	let((	ArrowPoints
		returnValue
		)

	returnValue = t

	if( abs( caadr( l_bBox ) - caar( l_bBox )) > abs( cadadr( l_bBox ) - cadar( l_bBox ))
		then
			ArrowPoints = list( 	caar( l_bBox ) - 3 :( cadadr( l_bBox ) - cadar( l_bBox ) )/2 + cadar( l_bBox )
						caar( l_bBox ):( cadadr( l_bBox ) - cadar( l_bBox ) )/2 + cadar( l_bBox )
						caar( l_bBox ) - 0.3: cadadr( l_bBox )
						caar( l_bBox ) - 0.3: cadar( l_bBox )
						caar( l_bBox ):( cadadr( l_bBox ) - cadar( l_bBox ) )/2 + cadar( l_bBox )
						)
		else
			ArrowPoints = list( 	( caadr( l_bBox ) - caar( l_bBox ) )/2 + caar( l_bBox ):cadar( l_bBox ) - 3
						( caadr( l_bBox ) - caar( l_bBox ) )/2 + caar( l_bBox ):cadar( l_bBox )
						caadr( l_bBox ):cadar( l_bBox ) - 0.3
						caar( l_bBox ):cadar( l_bBox ) - 0.3
						( caadr( l_bBox ) - caar( l_bBox ) )/2 + caar( l_bBox ):cadar( l_bBox )
						)
	)


	geAddHilightLine( geGetCurrentHilightSet( geGetWindowCellView()) ArrowPoints )
	geAddHilightRectangle( geGetCurrentHilightSet( geGetWindowCellView()) l_bBox )
	returnValue 

	); end let

); end procedure



; ============================================================================================
; This is the GUI (User interface)
; ============================================================================================
procedure( TR_PathFinder( )

	let((	TR_PathFinder_layerField
		TR_PathFinder_purposeField
		TR_PathFinder_directionField
		TR_PathFinder_widthField
		TR_PathFinder_spacingField
		TR_PathFinder_searchDepthField
		TR_PathFinder_resolutionField
		TR_PathFinder_onPowerGridField
		TR_PathFinder_currentbBoxField
		TR_PathFinder_grabbBoxField
		TR_PathFinder_theForm
		)

	TR_PathFinder_layerField = hiCreateCyclicField( 
		?name 		`TR_PathFinder_layerField
		?choices	list( "METAL1" "METAL2" "METAL3" "METAL4" "METAL5" "METAL6" )
		?prompt		"Layer"
		?defValue	"METAL1"
		?callback	"TR_PathFinder_layerField_CB( hiGetCurrentForm() )"
		)

	TR_PathFinder_purposeField = hiCreateCyclicField( 
		?name 		`TR_PathFinder_purposeField
		?choices	list( "drawing" )
		?prompt		"Purpose"
		?defValue	"drawing"
		?enabled	nil
		)

	TR_PathFinder_directionField = hiCreateCyclicField( 
		?name 		`TR_PathFinder_directionField
		?choices	list( "horizontal" "vertical" )
		?prompt		"Direction"
		?defValue	"horizontal"
		)

	TR_PathFinder_widthField = hiCreateFloatField( 
		?name 		`TR_PathFinder_widthField
		?prompt		"Width (um)"
		?defValue	0.24
		)

	TR_PathFinder_numTracksField = hiCreateIntField( 
		?name 		`TR_PathFinder_numTracksField
		?prompt		"NumTracks"
		?defValue	0
		)

	TR_PathFinder_spacingField = hiCreateFloatField( 
		?name 		`TR_PathFinder_spacingField
		?prompt		"Wire Spacing"
		?defValue	0.24
		)

	TR_PathFinder_viaSpacingField = hiCreateFloatField( 
		?name 		`TR_PathFinder_viaSpacingField
		?prompt		"Via Spacing"
		?defValue	0.215
		)


	TR_PathFinder_searchDepthField = hiCreateIntField( 
		?name 		`TR_PathFinder_searchDepthField
		?prompt		"Search Dep"
		?defValue	32
		?enabled	nil
		?callback	"TR_PathFinder_searchDepthField_CB( hiGetCurrentForm() )"
		)

	TR_PathFinder_resolutionField = hiCreateFloatField( 
		?name 		`TR_PathFinder_resolutionField
		?prompt		"Res (um)"
		?defValue	nil
		?enabled	t
		)

	TR_PathFinder_onPowerGridField = hiCreateToggleField( 
		?name		`TR_PathFinder_onPowerGridField
		?choices	list( list( `onPowerGrid "Flush-to-Grid" ))
		?defValue	list( nil )
		?enabled	t
		)

	TR_PathFinder_currentbBoxField = hiCreateListField( 
		?name 		`TR_PathFinder_currentbBoxField
		?prompt		"Area"
		?defValue	geGetWindowCellView()->bBox
		?enabled	t
		?callback	"TR_PathFinder_currentbBoxField_CB( hiGetCurrentForm() )"
		)

	TR_PathFinder_grabbBoxField = hiCreateButton( 
		?name 		`TR_PathFinder_grabbBoxField
		?buttonText	"Custom Window"
		?enabled	t
		?callback	"TR_PathFinder_grabbBoxField_CB( hiGetCurrentForm() )"
		)

	TR_PathFinder_theForm = hiCreateAppForm( 
		?name		`TR_PathFinder_theForm
		?initialSize	list( 1000 1500 )
                ?fields		list(	list( TR_PathFinder_layerField          0:0  200:30 80 )
					list( TR_PathFinder_purposeField      200:0  200:30 80 )
					list( TR_PathFinder_directionField    400:0  200:30 80 )

					list( TR_PathFinder_widthField          0:30 170:30 80 )
					list( TR_PathFinder_numTracksField    200:30 170:30 80 )

					list( TR_PathFinder_spacingField        0:60 170:30 80 )
					list( TR_PathFinder_viaSpacingField   200:60 170:30 80 )

					list( TR_PathFinder_searchDepthField    0:90 170:30 80 )
					list( TR_PathFinder_resolutionField   200:90 170:30 80 )
					list( TR_PathFinder_onPowerGridField  400:90 170:30 80 )

					list( TR_PathFinder_currentbBoxField    0:120 370:30 80 )
					list( TR_PathFinder_grabbBoxField     400:120 170:30 0  )

					)
		?callback	"PathFinder( 	?layer		TR_PathFinder_layerField->value
						?purpose	TR_PathFinder_purposeField->value
						?direction	TR_PathFinder_directionField->value
						?width		TR_PathFinder_widthField->value
						?spacing	TR_PathFinder_spacingField->value
						?searchDepth	TR_PathFinder_searchDepthField->value
						?resolution	TR_PathFinder_resolutionField->value
						?onPowerGrid	TR_PathFinder_onPowerGridField->onPowerGrid->value
						?bBox		TR_PathFinder_currentbBoxField->value
						?numTracks	TR_PathFinder_numTracksField->value
						)"
		?buttonLayout	list(  `ApplyCancelDef 
					list( `CrawFishVersionHistory "TR_CrawFish_VersionHistoryField_CB( )")
					list( `ClearHiLites           "GlobalHiliteStack_Clear( )" )
					list( `DreamsComeTrue         "PathFinder_ProcDreamsComeTrue( 
										?layer		TR_PathFinder_layerField->value
										?purpose	TR_PathFinder_purposeField->value
										)" )
					)
	)

	hiDisplayForm( TR_PathFinder_theForm )

	); end let

); end procedure


procedure( TR_PathFinder_layerField_CB( theForm )

	if( member( theForm->TR_PathFinder_layerField->value list( "METAL1" "METAL3" "METAL5" "METAL7" ))
		then 
			theForm->TR_PathFinder_directionField->value = "horizontal"
		else
			theForm->TR_PathFinder_directionField->value = "vertical"
	)

); end procedure


procedure( TR_PathFinder_searchDepthField_CB( theForm )

	if( theForm->TR_PathFinder_searchDepthField->value > 32 ||
	    theForm->TR_PathFinder_searchDepthField->value < 0
		then 
			theForm->TR_PathFinder_searchDepthField->value = theForm->TR_PathFinder_searchDepthField->lastValue 
	)

); end procedure

procedure( TR_PathFinder_currentbBoxField_CB( theForm )

	if( !listp( TR_PathFinder_currentbBoxField->value )
		then 

			theForm->TR_PathFinder_currentbBoxField->value = theForm->TR_PathFinder_currentbBoxField->lastValue

			hiDeleteField( 	theForm 
					`TR_PathFinder_currentbBoxField
					)

			hiAddField( 	theForm 
					list( TR_PathFinder_currentbBoxField    0:120 370:30 80 )
					)
	)

); end procedure

procedure( TR_PathFinder_grabbBoxField_CB( theForm )

	let(( 	bBox
		)
			theForm->TR_PathFinder_currentbBoxField->value = enterBox( )

			hiDeleteField( 	theForm 
					`TR_PathFinder_currentbBoxField
					)

			hiAddField( 	theForm 
					list( TR_PathFinder_currentbBoxField    0:120 370:30 80 )
					)
	); end let

); end procedure


; ====================================================================
; This procedure will turn the hilite rectangles into real rectangles
; ====================================================================
procedure( PathFinder_ProcDreamsComeTrue( @key 
						( layer 	"METAL1" )
						( purpose	"drawing" )
						)	
	
	let((	RectPair
		RectPairList
		)

	; Local Variable
	; ===============
	RectPairList = PathFinder_GlobalRectList
	RectPair     = nil

	GlobalHiliteStack_Clear( )

	foreach( RectPair RectPairList

		dbCreateRect( 	geGetWindowCellView( ) 
				list( layer purpose )
				RectPair
				)

	); end foreach

	); end let

); end procedure
