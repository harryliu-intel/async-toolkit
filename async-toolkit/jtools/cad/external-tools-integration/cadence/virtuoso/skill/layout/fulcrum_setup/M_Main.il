boundp( `M_Main ) && hiFixedMenuDown( M_Main )

;M_Main = hiCreateHorizontalFixedMenu( 
;	'M_Main
;	list( 	M_TR_NormalLVS 
;		M_TR_FullVirtualLVS 
;		M_TR_PartialVirtualLVS
;		M_TR_InteractiveVirtualLVS
;		M_TR_Floorplanner 
;               M_TR_FindInstance
;		M_TR_FloorplanArray ) 
;	2  
;	4 )

M_TR_CrawFish = hiCreateMenuItem( 
			?name		`M_TR_CrawFish
			?itemText	"CrawFish"
			?callback	"hiDisplayFixedMenu( M_CrawFish \"top\" )"
			?disable 	nil
			)

M_TR_CrawFishVLVS = hiCreateMenuItem( 
			?name		`M_TR_CrawFishVLVS
			?itemText	"CFish-VLVS"
			?callback	"TR_AssuraLVS( )"
			?disable 	nil
			)

M_TR_CrawFishPathFinder = hiCreateMenuItem( 
			?name		`M_TR_CrawFishPathFinder
			?itemText	"CFish-PathFinder"
			?callback	"TR_PathFinder( )"
			?disable 	nil
			)

M_TR_Spider  = hiCreateMenuItem( 
			?name		`M_TR_Spider
			?itemText	"Spider"
			?callback	"TR_Floorplanner"
			?disable 	nil
			)

M_Main = hiCreateHorizontalFixedMenu( 
	'M_Main
	list( 	
		M_TR_CrawFish
		M_TR_Spider
		M_TR_FloorplanArray
		M_TR_FindInstance
		) 
	5  
	1 )

M_CrawFish = hiCreateHorizontalFixedMenu( 
	'M_CrawFish
	list( 	
		M_TR_CrawFishVLVS
		M_TR_CrawFishPathFinder
		) 
	3  
	1 )


hiDisplayFixedMenu( M_Main "top" )
