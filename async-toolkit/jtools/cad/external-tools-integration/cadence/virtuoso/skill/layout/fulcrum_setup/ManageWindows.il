; This Fuction is resize the window
procedure( ResizeWindow( style )

	if( style == "max"
		then
			XYmin = 0:0
			XYmax = hiGetMaxScreenCoords()
		)

	if( style == "half"
		then
			XYmin = ( floor( car( hiGetMaxScreenCoords()) * 0.25  ):floor( car( cdr( hiGetMaxScreenCoords( ))) * 0.25 )) 
			XYmax = ( floor( car( hiGetMaxScreenCoords()) * 0.75  ):floor( car( cdr( hiGetMaxScreenCoords( ))) * 0.75 ))

			println( XYmin )
			println( XYmax )
		)

	hiResizeWindow( geGetCellViewWindow( geGetWindowCellView( ) ) list(XYmin XYmax))

) ;procedure


procedure( IconifyWindow( )

	hiIconifyWindow( geGetCellViewWindow( geGetWindowCellView( ) ) )

) ;procedure


procedure( DeiconifyWindow( )

	hiDeiconifyWindow( geGetCellViewWindow( geGetWindowCellView( ) ) )

) ;procedure
