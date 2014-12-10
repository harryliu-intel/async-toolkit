;Globals
TR_TrackRouter_p1 = nil
TR_TrackRouter_p2 = nil
TR_TrackRouter_exceptionList = nil

procedure( ManhattanRouter( @key
				( point1_db	nil )
				( point2_db	nil )
				( direction	nil )

				( layer		nil )
				( purpose	nil )

				( width		0.24 )
				( viaSpacing	0.215 )
				( spacing	0.24 )
				( allowedLayers	list( "METAL1" "METAL2" "METAL3" ))

				( DebugOn	t )
				( StepByStep	nil )
			)
	let((	OverlappedCells
		)


;	OverlappedCells = dbGetTrueOverlaps( geGetWindowCellView( ) point1_db->bBox list( point1_db->layer nil ) 32 )

	TR_TrackRouter_exceptionList = list( point1_db point2_db )

	TrackRouter( 		?point1_db	point1_db	
				?point2_db	point2_db	
				?direction	direction	
				?layer		layer		
				?purpose	purpose	
				?width		width		
				?spacing	spacing
				?viaSpacing	viaSpacing	
				?allowedLayers	allowedLayers	
				?DebugOn	DebugOn		
				?StepByStep	StepByStep	
				)

	); end let

	
	GlobalHiliteStack_Clear( )


); end procedure
 



procedure( TrackRouter( @key
				( point1_db	nil )
				( point2_db	nil )
				( direction	nil )

				( layer		nil )
				( purpose	nil )

				( width		0.24 )
				( spacing	0.24 )
				( viaSpacing	0.215 )
				( allowedLayers	list( "METAL1" "METAL2" "METAL3" ))

				( DebugOn	t )
				( StepByStep	nil )

			)

	let((	bBox
	 	Track1 Layer1 Purpose1 bBox1 bBox1o
		Track2 Layer2 Purpose2 bBox2 bBox2o
		InverseOn

		direction0 point1_db0 point2_db0 point3_db 
		t1 t2
		Layer0

		direction13 p13_1 p13_2 layer13
		direction32 p32_1 p32_2 layer32

		; These are for the contact cut 
		Contact_cellName
		Contact_instName
		Contact_counter
		Contact_db
		)




		; Localize Input 
		; ====================
		direction0 = direction
		point1_db0 = point1_db
		point2_db0 = point2_db
		Layer0     = layer
		Purpose0   = purpose


		; Initializations
		; =====================
		Contact_counter = 0

		Layer1 = point1_db0->layerName
		Layer2 = point2_db0->layerName

		Purpose1 = point1_db0->purpose	
		Purpose2 = point2_db0->purpose	

		bBox1o = point1_db0->bBox
		bBox2o = point2_db0->bBox

		if( !Purpose0
			then
				Purpose0 = Purpose1
				Purpose0 = "drawing"
		); end if



		; Globals Variables
		; ==================
		; TR_TrackRouter_exceptionList
		;
		if( !member( point1_db TR_TrackRouter_exceptionList )
			then
				TR_TrackRouter_exceptionList = cons( point1_db TR_TrackRouter_exceptionList )
		); end if

		if( !member( point2_db TR_TrackRouter_exceptionList )
			then
				TR_TrackRouter_exceptionList = cons( point2_db TR_TrackRouter_exceptionList )
		); end if



		if( !Layer0
			then
				Layer0 = Layer1
		); end if

		if( direction0 == "nil"
			then
				direction0 = nil
		); end if





		if( StepByStep
			then
				StepByStepDialogBox = hiDisplayAppDBox( 
					?name           'StepByStepDialogBox 
					?dboxText       "Press to Continue"
					?dialogType     hicMessageDialog
					?dialogStyle    'systemModal
					?buttonLayout   'OKCancel
					)
		)




;		t1 = dbCreateRect( geGetWindowCellView() list( "VIA23" "boundary" ) point1_db0->bBox )
;		t2 = dbCreateRect( geGetWindowCellView() list( "VIA23" "boundary" ) point2_db0->bBox )

		isFinish = nil

		Offset = 0



		surprise = width



		while( !isFinish


		; First Pass
		; ==========

		if( cadar( bBox1o ) < cadar( bBox2o ) || cadadr( bBox1o ) < cadadr( bBox2o ) 
			then
				if( direction0 == "horizontal" || ( direction0 == nil && member( Layer0 list( "METAL3" "METAL5" "METAL7" )))
					then
						; Case 1a
						bBox1 = list(	list(	max( caadr( bBox1o ) - width caar( bBox1o )) - surprise
									min( cadar( bBox1o ) - spacing cadadr( bBox1o ) - spacing - width )  
									)
								list(	min( caar( bBox2o ) + width caadr( bBox2o )) - Offset + surprise
									cadar( bBox2o ) + spacing + width
									)
								)
						InverseOn = t
						direction0 = "horizontal"
						DebugOn && printf( "TrackRouter --> Pass1 --> Case 1a \"%s\" p1 is lower than p2 %L \n" direction0 bBox1 )
					else
						;Case 1b
						bBox1 = list(	list(	min( caar( bBox1o ) - spacing  caadr( bBox1o ) - spacing - width )
									max( cadadr( bBox1o ) - width cadar( bBox1o )) - surprise
									)
								list(	caar( bBox2o ) + spacing + width
									min( cadar( bBox2o ) + width cadadr( bBox2o )) - Offset + surprise
									)
								)

						InverseOn = t
						direction0 = "vertical"
						DebugOn && printf( "TrackRouter --> Pass1 --> Case 1b \"%s\" p1 is lower than p2 %L \n" direction0 bBox1 )

				); end case 1a 1b

			else

				if( direction0 == "horizontal" || ( direction0 == nil && member( Layer0 list( "METAL3" "METAL5" "METAL7" )))
					then
						; Case 2a
						bBox1 = list(	list(	max( caadr( bBox1o ) - width caar( bBox1o )) - surprise
									cadadr( bBox2o ) - spacing - width
									)
								list(	min( caar( bBox2o ) + width caadr( bBox2o )) - Offset + surprise
									max( cadadr( bBox1o ) + spacing cadar( bBox1o ) + spacing + width ) 
									)
								)
						InverseOn = nil
						direction0 = "horizontal"
						DebugOn && printf( "TrackRouter --> Pass1 --> Case 2a \"%s\" p1 is higher than p2 %L \n" direction0 bBox1 ) 
					else
						; Case 2b
						bBox1 = list(	list(	min( caar( bBox1o ) - spacing  caadr( bBox1o ) - spacing - width )
									max( cadadr( bBox2o ) - width  cadar( bBox2o )) - surprise
									)
								list(	caar( bBox2o ) + spacing + width
									min( cadar( bBox1o ) + width cadadr( bBox1o )) - Offset + surprise
									)
								)
						InverseOn = t
						direction0 = "vertical"
						DebugOn && printf( "TrackRouter --> Pass1 --> Case 2b \"%s\" p1 is higher than p2 %L \n" direction0 bBox1 ) 

				); end if case 2a 2b

		); end if all Cases


		Track1 = car( PathFinder(	?layer		Layer0
						?purpose	Purpose0
						?direction	direction0
						?width		width
						?spacing	spacing
						?viaSpacing	viaSpacing
						?searchDepth	32
						?resolution	0.01
						?onPowerGrid	nil
						?bBox		bBox1
						?numTracks	1
						?inverse	InverseOn
						?exceptionList	TR_TrackRouter_exceptionList
						?DebugOn	nil
						))

		surprise = width

		; Second Pass
		if( Track1 == nil
			then
				DebugOn && printf( "TrackRouter --> Pass1 Failed --> Go for Pass 2 \n\n") 

		if( cadar( bBox1o ) < cadar( bBox2o ) || cadadr( bBox1o ) < cadadr( bBox2o )
			then
				if( direction0 == "horizontal" || ( direction0 == nil && member( Layer0 list( "METAL3" "METAL5" "METAL7" )))
					then
						; Case 1a
						bBox1 = list(	list(	max( caadr( bBox1o ) - width caar( bBox1o )) - surprise
									cadar( bBox2o ) - spacing
									)
								list(	min( caar( bBox2o ) + width caadr( bBox2o )) - Offset + surprise
									max( cadadr( bBox2o ) + spacing cadar( bBox2o ) + spacing + width )
									)
								)
						InverseOn = nil
						direction0 = "horizontal"
						DebugOn && printf( "TrackRouter --> Pass2 --> Case 1a \"%s\" p1 is lower than p2 %L \n" direction0 bBox1)
					else
						;Case 1b
						bBox1 = list(	list(	caar( bBox2o ) - spacing
									max( cadadr( bBox1o ) - width cadar( bBox1o )) - surprise
									)
								list(	max( caadr( bBox2o ) + spacing caar( bBox2o ) + spacing + width )
									min( cadar( bBox2o ) + width cadadr( bBox2o )) - Offset + surprise
									)
								)

						InverseOn = nil
						direction0 = "vertical"
						DebugOn && printf( "TrackRouter --> Pass2 --> Case 1b \"%s\" p1 is lower than p2 %L \n" direction0 bBox1) 

				); end case 1a 1b

			else

				if( direction0 == "horizontal" || ( direction0 == nil && member( Layer0 list( "METAL3" "METAL5" "METAL7" )))
					then
						; Case 2a
						bBox1 = list(	list(	max( caadr( bBox1o ) - width caar( bBox1o )) - surprise
									min( cadar( bBox2o ) - spacing cadadr( bBox2o ) - spacing - width )
									)
								list(	min( caar( bBox2o ) + width caadr( bBox2o )) - Offset + surprise
									cadadr( bBox2o ) + spacing 
									)
								)

						InverseOn = t
						direction0 = "horizontal"
						DebugOn && printf( "TrackRouter --> Pass2 --> Case 2a \"%s\" p1 is higher than p2 %L \n" direction0 bBox1) 

					else
						; Case 2b
						bBox1 = list(	list(	caar( bBox2o ) - spacing
									max( cadadr( bBox2o ) - width  cadar( bBox2o )) - surprise
									)
								list(	max( caadr( bBox2o ) + spacing caar( bBox2o ) + spacing + width )
									min( cadar( bBox1o ) + width cadadr( bBox1o )) - Offset + surprise
									)
								)
						InverseOn = nil
						direction0 = "vertical"
						DebugOn && printf( "TrackRouter --> Pass2 --> Case 2b \"%s\" p1 is higher than p2 %L \n" direction0 bBox1) 

				); end if case 2a 2b

		); end if all Cases

		Track1 = car( PathFinder(	?layer		Layer0
						?purpose	Purpose0
						?direction	direction0
						?width		width
						?spacing	spacing
						?viaSpacing	viaSpacing
						?searchDepth	32
						?resolution	0.01
						?onPowerGrid	nil
						?bBox		bBox1
						?numTracks	1
						?inverse	InverseOn
						?exceptionList	TR_TrackRouter_exceptionList
						?DebugOn	nil
						))


				if( Track1 == nil
					then
						DebugOn && printf( "TrackRouter --> Pass2 Failed \n\n") 
					else
						DebugOn && printf( "TrackRouter --> Pass2 Passed \n\n") 
				); end if

			else

				DebugOn && printf( "TrackRouter --> Pass1 Passed \n\n") 

		); end if Second Pass



;		if( Track1 || Offset >= 6
		if( Track1
			then
				isFinish = t
			else
				Offset = Offset + width
		)


		); end while isFinish



		DebugOn && printf( "TrackRouter --> ------------------------------------------------------\n" )



		; Shrink Track1 by width to prevent abuttment problem
		; ===================================================
		if( Track1
		then
			case( direction0
				( "horizontal"
					Track1 = list(
							list(
								caar( Track1 ) + width
								cadar( Track1 )
								)
							list(
								caadr( Track1 ) - width
								cadadr( Track1 )
								)
							)

				); end horizontal

				( "vertical"
					Track1 = list(
							list(
								caar( Track1 )
								cadar( Track1 ) + width
								)
							list(
								caadr( Track1 )
								cadadr( Track1 ) - width
								)
							)

				); end vertical

			); end case

		); end if
				








		point3_db = dbCreateRect( geGetWindowCellView() list( Layer0 Purpose0 ) Track1 )



		direction13 = nil

		;=======================
		; point1_db0 Covered
		;=======================
;		if( !member( point1_db0 dbGetOverlaps( geGetWindowCellView( ) Track1 list( Layer0 nil ) 32 ))
		if( !member( point1_db0 dbGetOverlaps( geGetWindowCellView( ) Track1 t 32 ))
			then
				if( direction0 == "horizontal"
					then
						direction13 = "vertical"
					else
						direction13 = "horizontal"
				)

				case( Layer0 

					( "METAL2"
						layer13 = "METAL3"
					)

					( "METAL3"
						layer13 = "METAL2"
					)

				); end case



				DebugOn && printf( "TrackRouter --> --> branch p1 p3 direction = \"%s\" \n" direction13 ) 

				TrackRouter( 	?point1_db 	point1_db0
						?point2_db 	point3_db
						?direction 	direction13
						?layer		layer13
						?allowedLayers 	allowedLayers
						)
			else
				case( direction0
					( "vertical" 
						point1_db0->bBox = list(
								car( point1_db0->bBox )
								list( caar( Track1 ) + width cadadr( point1_db0->bBox ))
								)

						if( cadar( bBox1o ) < cadar( bBox2o ) || cadadr( bBox1o ) < cadadr( bBox2o )
							then
								Contact_origin   = list( 
											caar( Track1 ) + width/2
											cadar( Track1 ) + width/2
											)	
							else
								Contact_origin   = list( 
											caadr( Track1 ) - width/2
											cadadr( Track1 ) - width/2
											)	
						)


					)

					( "horizontal"

						Contact_origin   = list( 
									caar( Track1 ) + width/2
									cadar( Track1 ) + width/2
									)	

						if( cadar( bBox1o ) < cadar( bBox2o ) || cadadr( bBox1o ) < cadadr( bBox2o )
							then
								point1_db0->bBox = list(
									car( point1_db0->bBox )
									list(	caadr( point1_db0->bBox ) 
										min( cadar( Track1 ) + width cadadr( point1_db0->bBox ))
										)
									)
							else
								point1_db0->bBox = list(
									list( 	caar( point1_db0->bBox ) 
										max( cadadr( Track1 ) - width cadar( point1_db0->bBox ))
										)
									cadr( point1_db0->bBox )
									)
						); end if
					)

				); end case

				; Auto Contact Cuts
				Contact_cellName = nil
				case( Layer1

					( "METAL2"
						case( Layer0
							( "METAL1"
								Contact_cellName = "M2_M1"
                                                	)
							( "METAL3"
								Contact_cellName = "M3_M2"
                                               	)
						); end case
					); end "METAL2" case

					( "METAL3"
						case( Layer0
							( "METAL2"
								Contact_cellName = "M3_M2"
                                                	)
							( "METAL4"
								Contact_cellName = "M4_M3"
                                                	)
						); end case
					); end "METAL2" case

				); end case


				if( Contact_cellName
					then
;						Contact_origin   = list( 
;									caar( Track1 ) + width/2
;									cadar( Track1 ) + width/2
;									)	

						Contact_db = dbCreateInstByMasterName(
								geGetWindowCellView( )
								"tsmc13lg"
								Contact_cellName
								"symbolic"
								nil
								Contact_origin
								"R0"
								)
		
						TR_TrackRouter_exceptionList = cons( Contact_db TR_TrackRouter_exceptionList )

				)
		); end if


		direction32 = nil


		;=======================
		; point2_db0 Covered
		;=======================
;		if( !member( point2_db0 dbGetOverlaps( geGetWindowCellView( ) Track1 list( Layer0 nil ) 32 ))
		if( !member( point2_db0 dbGetOverlaps( geGetWindowCellView( ) Track1 t 32 ))
			then
				if( direction0 == "horizontal"
					then
						direction32 = "vertical"
					else
						direction32 = "horizontal"
				)

				case( Layer0 

					( "METAL2"
						layer32 = "METAL3"
					)

					( "METAL3"
						layer32 = "METAL2"
					)

				); end case


				DebugOn && printf( "TrackRouter --> --> branch p3 p2 direction = \"%s\" \n" direction32 ) 

				TrackRouter( 	?point1_db 	point3_db 
						?point2_db 	point2_db0
						?direction 	direction32
						?layer		layer32
						?allowedLayers 	allowedLayers
						)
			else
				case( direction0
					( "vertical"
						point2_db0->bBox = list(
								list( caadr( Track1 ) - width cadar( point2_db0->bBox ))
								cadr( point2_db0->bBox )
								)

						if( cadar( bBox1o ) < cadar( bBox2o ) || cadadr( bBox1o ) < cadadr( bBox2o )
							then
								Contact_origin   = list( 
											caadr( Track1 ) - width/2
											cadadr( Track1 ) - width/2
											)
							else
								Contact_origin   = list( 
											caar( Track1 ) + width/2
											cadar( Track1 ) + width/2
											)
						)
	
					)
					( "horizontal"
						if( cadar( bBox1o ) < cadar( bBox2o ) || cadadr( bBox1o ) < cadadr( bBox2o )
							then
								point2_db0->bBox = list(
									list(	caar( point2_db0->bBox ) 
										max( cadadr( Track1 ) - width cadar( point2_db0->bBox ))
										)
									cadr( point2_db0->bBox )
									)

								Contact_origin   = list( 
											caadr( Track1 ) - width/2
											cadadr( Track1 ) - width/2
											)	
							else
								point2_db0->bBox = list(
									car( point2_db0->bBox )
									list(	caadr( point2_db0->bBox ) 
										min( cadar( Track1 ) + width cadadr( point2_db0->bBox ))
										)
									)

								Contact_origin   = list( 
											caadr( Track1 ) - width/2
											cadadr( Track1 ) - width/2
											)	
						); end if
					)
				); end case

						
				tempWidth = caadr( point2_db0->bBox ) - caar( point2_db0->bBox )
				tempHeight= cadadr( point2_db0->bBox ) - cadar( point2_db0->bBox )

				if( ( 	abs( tempWidth - width ) < 0.00001 && 
					abs( tempHeight - width ) < 0.0001 && 
					nth( 0 reverse(TR_TrackRouter_exceptionList) ) != point2_db0 &&
					nth( 1 reverse(TR_TrackRouter_exceptionList) ) != point2_db0
					)

					then
						println( "Hit!" )

;						tempList = setof( 	x 
;									exists( x reverse(TR_TrackRouter_exceptionList) x==point2_db0 ) 
;									!member( x exists( x reverse(TR_TrackRouter_exceptionList) x==point3_db ))
;									)
;						printf( "tempList=%L" tempList )
;
;						println( tempList )
;
;						foreach( tempCell tempList
;							println( "REMOVE" )
;							dbDeleteObject( tempCell )
;							TR_TrackRouter_exceptionList = remove( tempCell TR_TrackRouter_exceptionList )
;						); end foreach
;
;
;						dbDeleteObject( point2_db0 )
;						TR_TrackRouter_exceptionList = remove( point2_db0 TR_TrackRouter_exceptionList )
					else

						; Auto Contact Cuts
						Contact_cellName = nil
						case( Layer2
		
							( "METAL2"
								case( Layer0
									( "METAL1"
										Contact_cellName = "M2_M1"
		                                                	)
									( "METAL3"
										Contact_cellName = "M3_M2"
                                                			)
								); end case
							); end "METAL2" case

							( "METAL3"
								case( Layer0
									( "METAL2"
										Contact_cellName = "M3_M2"
                                		                	)
									( "METAL4"
										Contact_cellName = "M4_M3"
		                                                	)
								); end case
							); end "METAL2" case

						); end case


						if( Contact_cellName
							then

;								Contact_origin   = list( 
;											caadr( Track1 ) - width/2
;											cadadr( Track1 ) - width/2
;											)	

								Contact_db = dbCreateInstByMasterName(
										geGetWindowCellView( )
										"tsmc13lg"
										Contact_cellName
										"symbolic"
										nil
										Contact_origin
										"R0"
										)
		
								TR_TrackRouter_exceptionList = cons( Contact_db TR_TrackRouter_exceptionList )

						)

				); end if
		); end if

;		dbDeleteObject( t1 )
;		dbDeleteObject( t2 )
	
	); end let

); procedure

;p1 = car( geGetSelectedSet( ))
;p2 = car( geGetSelectedSet( ))
;TrackRouter( ?point1_db p1 ?point2_db p2 ?direction "vertical" )
;TrackRouter( ?point1_db p1 ?point2_db p2 )


;===================
; MAIN GUI
;===================

procedure( TR_TrackRouter( )

	let((	TR_TrackRouter_Add_p1
		p1 p2
		)

	TR_TrackRouter_Add_p1 = hiCreateButton(
		?name           `TR_TrackRouter_Add_p1
		?buttonText     "Add Point 1"
		?enabled        t
		?callback       "TR_TrackRouter_Add_p1_CB( hiGetCurrentForm() )"
                )

 	TR_TrackRouter_Add_p2 = hiCreateButton(
		?name           `TR_TrackRouter_Add_p2
		?buttonText     "Add Point 2"
		?enabled        t
		?callback       "TR_TrackRouter_Add_p2_CB( hiGetCurrentForm() )"
                )

	TR_TrackRouter_DirectionField = hiCreateCyclicField( 
		?name 		`TR_TrackRouter_DirectionField
		?choices	list( "horizontal" "vertical" "nil" )
		?prompt		"Direction"
		?defValue	"nil"
		?callback	""
		)

	TR_TrackRouter_Form = hiCreateAppForm( 
                ?name 		'TR_TrackRouter_Form
		?initialSize  	list( 600 1000 )
                ?fields  	list( 
					list( TR_TrackRouter_Add_p1		  0:0  200:30 0 )
					list( TR_TrackRouter_Add_p2		200:0  200:30 0 )
					list( TR_TrackRouter_DirectionField	  0:30 200:30 100 )
				)
		?formTitle	""
		?callback	"ManhattanRouter(
					?point1_db	TR_TrackRouter_p1
					?point2_db	TR_TrackRouter_p2
					?direction	TR_TrackRouter_DirectionField->value
					?layer		nil
					?purpose	nil
					)"
	        )

	hiDisplayForm( TR_TrackRouter_Form )

	); end let

); end procedure


;===================
; Add Point 1
;===================

procedure( TR_TrackRouter_Add_p1_CB( theForm )

	let((	dummy
		w_windowId
		g_partial
		returnValue
		)

	w_windowId  = geGetCellViewWindow( geGetWindowCellView( ))
	g_partial   = nil
	returnValue = nil

	geDeselectAllFig( )

	if( geAddSelectPoint( w_windowId g_partial )
		then
			returnValue = car( geGetSelectedSet( ))
	); end if

	TR_TrackRouter_p1 = returnValue

	returnValue

	); end let

); end procedure

;===================
; Add Point 2
;===================

procedure( TR_TrackRouter_Add_p2_CB( theForm )

	let((	dummy
		w_windowId
		g_partial
		returnValue
		)

	w_windowId  = geGetCellViewWindow( geGetWindowCellView( ))
	g_partial   = nil
	returnValue = nil

	geDeselectAllFig( )

	if( geAddSelectPoint( w_windowId g_partial )
		then
			returnValue = car( geGetSelectedSet( ))
	); end if

	TR_TrackRouter_p2 = returnValue

	returnValue

	); end let

); end procedure
