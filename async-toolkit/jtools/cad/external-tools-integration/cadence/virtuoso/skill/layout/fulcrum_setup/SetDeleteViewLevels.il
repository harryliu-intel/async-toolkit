; This Fuction is used to set the view levels of selected instances
procedure( SetViewLevels( fromLevel toLevel )

        SelectedCellList = geGetSelSet( )
	SelectedCellList = setof( x SelectedCellList member( x->objType list( "inst" "mosaic" )))

        foreach( SelectedCell SelectedCellList 

			geSetInstViewLevel( 
					geGetCellViewWindow( geGetWindowCellView( ) )
					SelectedCell
					fromLevel
					toLevel
				)

        ) ;foreach


) ;procedure


procedure( DeleteViewLevels( )

        SelectedCellList = geGetSelSet( )
	SelectedCellList = setof( x SelectedCellList member( x->objType list( "inst" "mosaic" )))

        foreach( SelectedCell SelectedCellList  

			geDeleteInstViewLevel( 
					geGetCellViewWindow( geGetWindowCellView( ) )
					SelectedCell
				)

        ) ;foreach


) ;procedure

