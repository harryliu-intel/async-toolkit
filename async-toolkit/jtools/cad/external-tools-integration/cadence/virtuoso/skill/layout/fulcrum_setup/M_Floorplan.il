; boundp( `M_Floorplan ) && hiFixedMenuDown( M_Floorplan )

M_TR_Floorplanner = hiCreateMenuItem( 
			?name		`M_TR_Floorplanner 
			?itemText 	"Spider 3.0.5" 
			?callback 	"TR_Floorplanner( )"
			?disable 	nil
			)

M_TR_FindInstance = hiCreateMenuItem( 
			?name		`M_TR_FindInstance
			?itemText 	"Find" 
			?callback 	"TR_FindInstance( )"
			?disable 	nil
			)

M_TR_FloorplanArray = hiCreateMenuItem( 
			?name		`M_TR_FloorplanArray
			?itemText 	"Array" 
			?callback 	"TR_FloorplanArray( )"
			?disable 	nil
			)

M_Floorplan = hiCreateHorizontalFixedMenu( 'M_Floorplan 
	list( 	M_TR_Floorplanner 
		M_TR_FindInstance 
		M_TR_FloorplanArray) 
	1  
	3 )

; hiDisplayFixedMenu( M_Floorplan "top" )
