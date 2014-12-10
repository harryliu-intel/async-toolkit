; This Fuction is used to auto draw well plugs 
; Assume that there is only one layer of each NIMP, PIMP and/or NWELL
; Multiple overlaps of the same layer may crash the script
; For NWELL Plug, it will always draw a plug if there is a NWELL, but will
;		only cut the PIMP if there is any (vice verse for PWELL)

procedure( DrawWellPlugs( )

        ; local Varibales
        let( ( 	
		d_CurrentWindow
		d_cellView
		d_PIMP
		d_NIMP
		d_inst
		l_Path
        	CurrentXY 
		t_libName
		t_cellName
		ix1 ix2 iy1 iy2
		px1 px2 py1 py2
                )

		d_cellView = geGetWindowCellView( )
		d_CurrentWindow = geGetCellViewWindow( geGetWindowCellView())
 		t_libName  = "tsmc13lg"

		; Get the cursor coordinates
		CurrentXY = hiGetPoint( d_CurrentWindow )
		
		d_PIMP = gePointQuery( d_CurrentWindow CurrentXY "DrawWellPlugs_checkPIMP" )
		d_NIMP = gePointQuery( d_CurrentWindow CurrentXY "DrawWellPlugs_checkNIMP" )

		if( gePointQuery( d_CurrentWindow CurrentXY "DrawWellPlugs_checkNWELL" ) && !d_NIMP
			then
				printf( "DrawWellPlugs --> Draw NWELL Plug \n" )

                                t_cellName = "M1_NACTIVE"
                      	        t_viewName = "symbolic"
                                t_instName = nil
                                t_orient   = "R0"
				l_origin = CurrentXY
				NetName = "Vdd"

                                d_inst = dbCreateInstByMasterName(	d_cellView 
                                                			t_libName 
                                                			t_cellName 
                                                			t_viewName 
                                                			t_instName 
                                                			l_origin 
                                                			t_orient 
                                                			)



				NetID = dbMakeNet( d_cellView NetName )


				; Create Connectivity
                                dbCreatePin( NetID d_inst )

				ix1 = caar( d_inst->bBox )
				ix2 = caadr( d_inst->bBox )
				iy1 = cadar( d_inst->bBox )
				iy2 = cadadr( d_inst->bBox )


				if( d_PIMP
					then 
						case( d_PIMP->objType

							( "rect" 

								px1 = caar( d_PIMP->bBox )
								px2 = caadr( d_PIMP->bBox )
								py1 = cadar( d_PIMP->bBox )
								py2 = cadadr( d_PIMP->bBox )

								l_Path = list(	px1:py1
										px1:iy2
										ix2:iy2
										ix2:iy1
										ix1:iy1
										ix1:iy2
										px1:iy2
										px1:py2
										px2:py2
										px2:py1
										px1:py1
										)
							)

							( "polygon"

								px1 = caar( d_PIMP->points )
								px2 = caadr( d_PIMP->points )
								py1 = cadar( d_PIMP->points )
								py2 = cadadr( d_PIMP->points )

								l_Path = list(	px1:py1
										px1:iy2
										ix2:iy2
										ix2:iy1
										ix1:iy1
										ix1:iy2
										px1:iy2
										px1:py2
										px2:py2
										px2:py1
										px1:py1
										)

								l_path = nconc( l_Path cdr( d_PIMP->points ) )

							)

						);end case

						dbDeleteObject( d_PIMP )

						d_PIMP = dbCreatePolygon( d_cellView list( "PIMP" "drawing" ) l_Path )

				);end if
	
				
			else


				if( !gePointQuery( d_CurrentWindow CurrentXY "DrawWellPlugs_checkNWELL" ) && !d_PIMP

					then

				printf( "DrawWellPlugs --> Draw PWELL Plug\n" )

				t_cellName = "M1_PACTIVE"
				t_viewName = "symbolic"
				t_instName = nil
				t_orient   = "R0"
				l_origin = CurrentXY
				NetName = "GND"

				d_inst = dbCreateInstByMasterName(	d_cellView 
                                        				t_libName 
                                        				t_cellName 
                                        				t_viewName 
                                        				t_instName 
                                        				l_origin 
                                        				t_orient 
                                        				)


				NetID = dbMakeNet( d_cellView NetName )

				; Create Connectivity
				dbCreatePin( NetID d_inst )

				if( d_NIMP
					then 
						case( d_NIMP->objType

							( "rect" 

								l_Path = list( 	car( d_NIMP->bBox )
										caar( d_inst->bBox ):cadar( d_NIMP->bBox )
										caar( d_inst->bBox ):cadadr( d_inst->bBox )
										cadr( d_inst->bBox )
										caadr( d_inst->bBox ):cadar( d_inst->bBox )
										car( d_inst->bBox )
										caar( d_inst->bBox ):cadar( d_NIMP->bBox )
										caadr( d_NIMP->bBox ):cadar( d_NIMP->bBox )
										cadr( d_NIMP->bBox )
										caar( d_NIMP->bBox ):cadadr( d_NIMP->bBox )
										)

							)

							( "polygon"

								l_Path = list( 	car( d_NIMP->points )
										caar( d_inst->bBox ):cadar( d_NIMP->points )
										caar( d_inst->bBox ):cadadr( d_inst->bBox )
										cadr( d_inst->bBox )
										caadr( d_inst->bBox ):cadar( d_inst->bBox )
										car( d_inst->bBox )
										caar( d_inst->bBox ):cadar( d_NIMP->points )
										)
								l_path = nconc( l_Path cdr( d_NIMP->points ) )

							)


						);end case

						dbDeleteObject( d_NIMP )

						d_NIMP = dbCreatePolygon( d_cellView list( "NIMP" "drawing" ) l_Path )
				);end if


					else

						printf( "Error!!! You layout sucks!!! \n")


					); end if



		); end if

	); end let

); end procedure


procedure( DrawWellPlugs_checkNIMP( fig ) 

	fig->layerName == "NIMP"

); end procedure

procedure( DrawWellPlugs_checkPIMP( fig ) 

	fig->layerName == "PIMP"

); end procedure

procedure( DrawWellPlugs_checkNWELL( fig ) 

	fig->layerName == "NWELL"

); end procedure

procedure( DropWellPlug(
			@key
			( libName	"tsmc13lg" )
			( cellName	"M1_NWELL" )
			( viewName	"symbolic")
			( netName	"Vdd" )
			( XY  		nil )
			)

	let((	CurrentXY
		CurrentCellView
		instName
		origin
		orient
		)

		CurrentCellView = geGetWindowCellView()

		if( XY
			then
				CurrentXY = XY
			else
				; Get the cursor coordinates
				CurrentXY = hiGetPoint( geGetCellViewWindow( CurrentCellView ))
		);end if

		instName = nil
		origin   = CurrentXY
		orient   = "R0"

		d_inst = dbCreateInstByMasterName(	CurrentCellView
							libName
							cellName
							viewName
							instName
							origin
							orient
							)

		NetID = dbMakeNet( CurrentCellView netName )

		; Create Connectivity
		dbCreatePin( NetID d_inst )
		;dbCreateConnByName( NetID d_inst "pdd" )

	); end let
); end procedure
