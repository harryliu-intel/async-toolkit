; boundp( `M_Verify ) && hiFixedMenuDown( M_Verify )

M_TR_NormalLVS = hiCreateMenuItem( 
			?name		`M_TR_NormalLVS 
			?itemText	"Normal LVS"
			?callback	"AssuraLVS(	?runVirtual nil 
							?runBlackBox nil 
							?runPartialVirtual nil)"
			?disable 	nil
			)

M_TR_FullVirtualLVS = hiCreateMenuItem( 
			?name		`M_TR_FullVirtualLVS 
			?itemText	"Full Virtual LVS"
			?callback	"AssuraLVS(	?runVirtual t 
							?runBlackBox nil 
							?runPartialVirtual nil)"
			?disable 	nil
			)

M_TR_PartialVirtualLVS = hiCreateMenuItem( 
			?name		`M_TR_PartialVirtualLVS 
			?itemText	"Partial Virtual LVS"
			?callback	"AssuraLVS(	?runVirtual t 
							?runBlackBox nil 
							?runPartialVirtual t)"
			?disable 	nil
			)

M_TR_InteractiveVirtualLVS = hiCreateMenuItem( 
			?name		`M_TR_InteractiveVirtualLVS 
			?itemText	"Interactive VLVS + Setup"
			?callback	"TR_AssuraLVS( )"
			?disable 	nil
			)


M_Verify = hiCreateHorizontalFixedMenu( 
	'M_Verify
	list( 	M_TR_NormalLVS 
		M_TR_FullVirtualLVS 
		M_TR_PartialVirtualLVS
		M_TR_InteractiveVirtualLVS
		M_TR_Floorplanner 
                M_TR_FindInstance) 
	2  
	4 )

; hiDisplayFixedMenu( M_Verify "top" )
