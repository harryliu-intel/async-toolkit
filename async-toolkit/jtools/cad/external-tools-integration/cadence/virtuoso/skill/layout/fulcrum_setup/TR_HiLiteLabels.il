; This Script is use to add temp labels for all 
; 1. rect
; 2. polygon
; 3. path
;
; Characteristics
; 1. The corresponding layer must be selected in order for the script to put labels on
; 2. Script only operates on the current screen view, does not always put labels on the 
;    entire layout (this actually minimize a lot of compute time)
; 3. All Labels are volatile => no write permission is required to draw labels
;    Labels are drawn on hilite set
;
; Global Variable
; 1. AddHiLiteLabels_DB
; 2. AddHiLiteLabels_FontSize
; 3. AddHiLiteLabels_NetName

AddHiLiteLabels_NetName  = "ALL"
AddHiLiteLabels_FontSize = 0.02
AddHiLiteLabels_DB = nil

;( SuppressList	list( nil list( "GND" "Vdd" )))


procedure( AddHiLiteLabels( @key
				( DebugOn 	nil )
				( SuppressList	nil )
				( SelectFig	nil )
				) 
		"This Script is use to add temp labels for all"

	let( ( 	l_d_Selected
		returnValue
		FontSize
		NetName
		SuppressedSourceList
		SuppressedTargetList
		)

	returnValue  = t

	AddHiLiteLabels_DB = nil
	FontSize = AddHiLiteLabels_FontSize
	NetName  = AddHiLiteLabels_NetName 
;	SuppressedSourceList = car( SuppressList )
	SuppressedSourceList = list( "GND" "Vdd" "pdd")
	SuppressedTargetList = cadr( SuppressList )

	DebugOn && printf( "==================================================================================\n" )
	DebugOn && printf( "AddHiLiteLabels --> SuppressedSourceList = %L\n" SuppressedSourceList )
	DebugOn && printf( "AddHiLiteLabels --> SuppressedTargetList = %L\n" SuppressedTargetList )
	DebugOn && printf( "==================================================================================\n" )

	; Check to see if there is a hilite Set for this cellview
	if( !geGetCurrentHilightSet( geGetWindowCellView())
	then
			gePushHilightStack( geCreateHilightSet( geGetWindowCellView() list( "hilite" "drawing8" ) t ))			
			geGetCurrentHilightSet( geGetWindowCellView())->enable = t

			geDeselectAllFig()

;			geSingleSelectBox(
;			geAddSelectBox( 
;					geGetCellViewWindow( geGetWindowCellView())
;					t
;					geGetWindowBox( geGetCellViewWindow( geGetWindowCellView()) ))
;
;			geSelectArea( 	geGetCellViewWindow( geGetWindowCellView())
;					geGetWindowBox( geGetCellViewWindow( geGetWindowCellView()) ))


			l_d_Selected = dbGetOverlaps(	geGetWindowCellView()
							geGetWindowBox( geGetCellViewWindow( geGetWindowCellView()) ))


;			l_d_Selected = geGetSelectedSet()
;			geDeselectAllFig()

			foreach( d_Selected l_d_Selected

				SelectFig && geSelectFig( d_Selected )

				case( d_Selected->objType
		
					( "rect"
						if( 	( 	( d_Selected->net->name && NetName == "ALL" )
								|| d_Selected->net->name == NetName
								) 
							&& !member( d_Selected->net->name SuppressedTargetList )
							then
								geAddHilightLabel(
										geGetCurrentHilightSet( geGetWindowCellView())
										car( d_Selected->bBox )
										d_Selected->net->name
										"centerCenter"
										"R0"
										"stick"
										FontSize
										t
										)

								AddHiLiteLabels_DB = cons( car( d_Selected->bBox ) AddHiLiteLabels_DB )
						); end if
					)

					( "path"
						if( 	( 	( d_Selected->net->name && NetName == "ALL" )
								|| d_Selected->net->name == NetName
								) 
							&& !member( d_Selected->net->name SuppressedTargetList )

							then
								geAddHilightLabel(
										geGetCurrentHilightSet( geGetWindowCellView())
										car( d_Selected->points )
										d_Selected->net->name 
										"centerCenter"
										"R0"
										"stick"
										FontSize
										t
										)
								AddHiLiteLabels_DB = cons( car( d_Selected->points ) AddHiLiteLabels_DB )
						); end if
					)

					( "inst" 

						DebugOn && printf( "==================================================================================\n" )
						DebugOn && printf( "AddHiLiteLabels --> Processing Cell instName=\"%s\" cellName=\"%s\"\n"
								d_Selected->name
								d_Selected->cellName
								)

						x_inst = car( d_Selected->xy )
						y_inst = cadr( d_Selected->xy )

						; Go thru each instTerms (IO pins)
						foreach( d_instTerm d_Selected->instTerms


								DebugOn && printf( "AddHiLiteLabels --> --> Processing %12s -> %-12s\t"
										sprintf( nil "\"%s\"" d_instTerm->name )
										sprintf( nil "\"%s\"" d_instTerm->net->name )
										)

								x_instTerm = caar( car( d_instTerm->term->pins )->fig->bBox )
								y_instTerm = cadar( car( d_instTerm->term->pins )->fig->bBox )

								; This is necessary since some gate PCELL has layout change but
								; the terminal remains in the propagate net box
								if( x_instTerm && y_instTerm
									then

								case( d_Selected->orient

									( "MY" 
										xy_instTerm = list( 	( x_inst - x_instTerm )
													( y_inst + y_instTerm ))
									)
									
									( "MX" 
										xy_instTerm = list( 	( x_inst + x_instTerm )
													( y_inst - y_instTerm ))
									)

									( "R90" 
										xy_instTerm = list( 	( x_inst - y_instTerm )
													( y_inst + x_instTerm ))
									)

									( "R180" 
										xy_instTerm = list( 	( x_inst - x_instTerm )
													( y_inst - y_instTerm ))
									)
									
									( t 
										xy_instTerm = list( 	( x_inst + x_instTerm )
													( y_inst + y_instTerm ))
									)


								); end case

								else
									DebugOn && printf( 
									"\nAddHiLiteLabels --> --> --> Warning! No Physical Shape Found for this Terminal!!! Skip!!!\n")

								); end xyTerm

							if( 	( NetName == "ALL" || d_instTerm->net->name == NetName )
								&& !member( d_instTerm->net->name SuppressedTargetList )
								&& !member( d_instTerm->name      SuppressedSourceList )
								&& d_instTerm->net->name != nil

								then 
								geAddHilightLabel(
										geGetCurrentHilightSet( geGetWindowCellView())
										xy_instTerm
										sprintf( nil "%s->%s" 
											 d_instTerm->name
											 d_instTerm->net->name 
											)
										"centerCenter"
										"R0"
										"stick"
										FontSize
										t
										)

								AddHiLiteLabels_DB = cons( xy_instTerm AddHiLiteLabels_DB )

									;printf( "Drawn!!!\n" )

								;else
									;printf( "Supressed!!!\n" )



							);end if


						); end foreach

					); end "inst" subcase


				); end case
			); end foreach

		else

			gePopHilightStack( geGetWindowCellView())

	);end if

	if( AddHiLiteLabels_DB == nil
		then
			gePopHilightStack( geGetWindowCellView())

	);end if

	returnValue
	
	); end let


); end procedure


procedure( ZoomToHiLiteLabels( )

	let( ( 	temp_xy
		temp_bBox
		temp_scale
		returnValue
		)

	returnValue  = t

	; Check to see if there is a hilite Set for this cellview
	if( !geGetCurrentHilightSet( geGetWindowCellView())
		then
			printf( "ZoomToHiLiteLabels( )--> ERROR!!! You do NOT have any hilite labels at all!!!\n" )

		else

			temp_xy = car( AddHiLiteLabels_DB )
			temp_scale = AddHiLiteLabels_FontSize * 5


			temp_bBox = list( 
					list( car( temp_xy )-temp_scale cadr( temp_xy )-temp_scale)
					list( car( temp_xy )+temp_scale cadr( temp_xy )+temp_scale)
					)

			hiZoomIn( geGetCellViewWindow( geGetWindowCellView()) temp_bBox  )

			AddHiLiteLabels_DB = remove( temp_xy AddHiLiteLabels_DB )
			AddHiLiteLabels_DB = reverse( AddHiLiteLabels_DB )
			AddHiLiteLabels_DB = cons( temp_xy AddHiLiteLabels_DB )
			AddHiLiteLabels_DB = reverse( AddHiLiteLabels_DB )
			
	);end if

	returnValue

	);end let

); end procedure


; Simple GUI 

procedure( TR_AddHiLiteLabels( )

        let( (  TR_AddHiLiteLabels 
                TR_AddHiLiteLabels_
                TR_AddHiLiteLabels_ySnap )

        TR_AddHiLiteLabels_NetName = hiCreateStringField( 
                ?name 'TR_AddHiLiteLabels_NetName 
                ?prompt "Enter NetName" 
                ?defValue sprintf( nil "%s" AddHiLiteLabels_NetName ) 
                ?editable t )

        TR_AddHiLiteLabels_FontSize = hiCreateStringField( 
                ?name 'TR_AddHiLiteLabels_FontSize 
                ?prompt "Enter FontSize" 
                ?defValue sprintf( nil "%n" AddHiLiteLabels_FontSize ) 
                ?editable t )

        TR_AddHiLiteLabels_Form = hiCreateAppForm( 
                ?name 'TR_AddHiLiteLabels_Form
                ?fields  '( TR_AddHiLiteLabels_NetName TR_AddHiLiteLabels_FontSize )
                ?callback "TR_AddHiLiteLabels_CB( hiGetCurrentForm() )" 
        )

        hiDisplayForm(  TR_AddHiLiteLabels_Form )

        ); end let

); end procedure


procedure( TR_AddHiLiteLabels_CB( theForm )

        AddHiLiteLabels_NetName = theForm->TR_AddHiLiteLabels_NetName->value
        AddHiLiteLabels_FontSize = evalstring( theForm->TR_AddHiLiteLabels_FontSize->value )

); end procedure





