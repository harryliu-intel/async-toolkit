; =========================================================================
; This procedure is used to count the number of paths with connectivities 
; in a layout
; =========================================================================

; Global Variables
; =================
TR_CountNets_GlobalCounter = 0
TR_CountNets_GlobalPinFile = "./TR_CountNets_GlobalPinFile.txt"

procedure( TR_CountNets( @key
			( d_CellView 	nil )
			( DebugOn	t   )
			)

	let((	l_Obj	Obj
		l_NetName
		d_Cell
		outPort
		)

	; Local Variables
	; ===============
	l_NetName = nil
	d_Cell = geGetWindowCellView( )
	outPort = outfile( TR_CountNets_GlobalPinFile "a" )

	geSelectAllFig( )
	l_Obj = geGetSelectedSet( )
	geDeselectAllFig( )

	l_Obj = setof( x l_Obj x->objType=="path" )

	foreach( Obj l_Obj

		if( Obj->net->name && !member( Obj->net->name l_NetName )
			then
				l_NetName = cons( Obj->net->name l_NetName )
				fprintf( outPort "%s\n" Obj->net->name )
		); end if

	); end foreach

	close( outPort )

	; Return Value
	; ============
	DebugOn && printf( "TR_CountNets -> ====================================================\n" )
	DebugOn && printf( "TR_CountNets -> %s %s %s\n" d_Cell->libName d_Cell->cellName d_Cell->viewName )
	DebugOn && printf( "TR_CountNets -> There are %n unique paths with connectivities\n" length( l_NetName ))
	DebugOn && printf( "TR_CountNets -> ====================================================\n" )

	length( l_NetName )

	); end let

); end procedure


; =======================================================================================
; This is a wrap-around procedure use to parse the entire layout from the top down
; The skil code is setup to recognize only wiring cells with cellName "xxxwiresxxx"
; =======================================================================================

procedure( TR_CountNets_ProcDigHierarchy( @key
						( DebugOn	t )
						)

	let(( 	l_WiringCell WiringCell
		)

	; Refresh Current Window
	; ======================
	geRefresh( )

	geSelectAllFig( )
	l_WiringCell = setof( x geGetSelectedSet() 
				!member( x->libName list( "tsmc13lg" "stack" "gate" )) && x->objType == "inst" )
	geDeselectAllFig( )

	rexCompile( "wires" )
	l_WiringCell = setof( x l_WiringCell rexExecute( x->cellName )) 


	; =======================================================================================================
	; Since the skill code will automatically swap in and swap out the wiring cells in the current window
	; therefore, the "l_WiringCell" should contain the actual dd object instead of the purgeable db objects	
	; =======================================================================================================
	l_WiringCell = foreach( mapcar WiringCell l_WiringCell

		ddGetObj( WiringCell->libName WiringCell->cellName WiringCell->viewName )

	); end foreach


	; Get the Number of Paths with Connectivities on this level
	; =========================================================
	TR_CountNets_GlobalCounter = TR_CountNets( ) + TR_CountNets_GlobalCounter

	foreach( WiringCell l_WiringCell

		DebugOn && printf( "TR_CountNets_ProcDigHierarchy --> Processing ( \"%s\" \"%s\" \"%s\" ) \n" 
				WiringCell->lib->name
				WiringCell->cell->name
				WiringCell->name
				)

		geOpen(	?window         geGetCellViewWindow( geGetWindowCellView( ) )
			?lib            WiringCell->lib->name
			?cell           WiringCell->cell->name
			?view           WiringCell->name
			?viewType       "maskLayout"
			?mode           "r" )

		TR_CountNets_ProcDigHierarchy( )
		
	); end foreach

	); end let

); end procedure


; ========================================================================================
; Main Procedure
; ========================================================================================

procedure( TR_CountNets_ProcMain( @key
					( DebugOn	t )
					)
	let((	Original_cellName 
		Original_libName
		Original_viewName 
		)

	; Erase Global Pin File
	; =====================
	sh( sprintf( nil "rm -rf %s" TR_CountNets_GlobalPinFile ))


	; Initialize Counter
	; ==================
	Original_cellName = geGetWindowCellView( )->cellName
	Original_libName = geGetWindowCellView( )->libName
	Original_viewName = geGetWindowCellView( )->viewName

	; Global Variables
	; =================
	TR_CountNets_GlobalCounter = 0

	; Call the Main Routine
	; =====================
	TR_CountNets_ProcDigHierarchy( ?DebugOn t )

	printf( "TR_CountNets_ProcMain -> ======================================================================\n" )
	printf( "TR_CountNets_ProcMain -> %s %s %s\n" Original_cellName Original_libName Original_viewName )
	printf( "TR_CountNets_ProcMain -> There are %n unique paths with connectivities for ALL hierarchies \n" TR_CountNets_GlobalCounter )
	printf( "TR_CountNets_ProcMain -> ======================================================================\n" )

	geOpen(	?window         geGetCellViewWindow( geGetWindowCellView( ) )
		?lib            Original_libName
		?cell           Original_cellName
		?view           Original_viewName
		?viewType       "maskLayout"
		?mode           "r" )

	); end let

	TR_CountNets_GlobalCounter

); end procedure
