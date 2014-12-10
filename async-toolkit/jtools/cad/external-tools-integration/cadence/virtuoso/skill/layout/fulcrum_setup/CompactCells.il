; This procedure compact/expand the layout horizontally ONLY
;
; Useful Skill Function
; =====================
; dbProduceOverlapInst( geGetWindowCellView( ) car( geGetSelectedSet( ))->bBox )
; setof( x ScanLine_l_Cells_next !member(x ScanLine_l_Cells_current))
;
; Notice
; ======
; The procedure will fail to operate correctly if
; (1) there are instances within another instances 


; Setting Global Snapping

envSetVal("layout" "xSnapSpacing" `float 0.01)
envSetVal("layout" "ySnapSpacing" `float 0.01)


procedure( CompactCells( @key
				( WorkOnAll nil )
				( DebugOn t ) 
				( StepByStep nil )
				( AlignToGrid list( 0.5:9.6 5:9.6 ))
				)

	let( ( 	returnValue 
		ScanLine_l_Cells_current 
		ScanLine_l_OverlappedCells
		OffSet
		SkipLoop 
		IsFinish
		l_ValidCells
		StepByStepDialogBox
		NeighborList NeighborID NeighborOffSet
		tempCell Counter
		IsMidLevelCell overwrite_xy CellUnderTest
		)
		
		ScanLine_l_Cells_current = nil
		IsFinish = nil

		DebugOn && printf( "CompactCells --> Start Procedure\n")

		ScanLine_l_Cells_current = GetOrderedCellList( 	?WorkOnAll 	WorkOnAll 
								?AlignToGrid	AlignToGrid
								)


		; Forming the Valid Cell List
		l_ValidCells = foreach( mapcar d_Cell ScanLine_l_Cells_current

			car( d_Cell )

		); end foreach

		geDeselectAllFig( )


			foreach( ScanLine_Cell ScanLine_l_Cells_current


				println( 888 )

				ScanLine_Cell = car( ScanLine_Cell )

				; =================================
				; Deals with Compaction to the Left
				; =================================

				NeighborList = dbGetNeighbor( 	geGetWindowCellView() 
								list( 	caar(ScanLine_Cell->bBox):cadar(ScanLine_Cell->bBox)+0.001
									caadr(ScanLine_Cell->bBox):cadadr(ScanLine_Cell->bBox)-0.001)
								"left" 
								)

				DebugOn && printf( "CompactCells --> ========================================================================== \n" )	

				if( NeighborList && car( NeighborList ) != 0 
					then
						NeighborID = cadr( NeighborList )
						NeighborOffSet = car( NeighborList )
						
						DebugOn && printf( "CompactCells --> Perform Compaction: \"%s\" has \"%s\" on the left with an OffSet = %num\n" 
								ScanLine_Cell->name
								NeighborID->name
								NeighborOffSet
								)

;						MoveExactAbuttedGroup( ScanLine_Cell -NeighborOffSet 0 ScanLine_l_Cells_current )

						dbMoveFig( ScanLine_Cell nil list(-NeighborOffSet:0 "R0"))

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












				; =================================
				; Post-Compaction Alignment
				; =================================

				; Test to see if it is one of the tempCell 
				if( ScanLine_Cell->viewName == "layout.compactor"
					then
						CellUnderTest = car( leSearchHierarchy(	ScanLine_Cell->master
											ScanLine_Cell->master->bBox
											0
											"inst"
											nil
											))
						overwrite_xy = list( 	car( ScanLine_Cell->xy) + car( CellUnderTest->xy )  
									cadr( ScanLine_Cell->xy) + cadr( CellUnderTest->xy ))
					else
						CellUnderTest = ScanLine_Cell
						overwrite_xy = ScanLine_Cell->xy
				)


				if( AlignToGrid
					then
						IsMidLevelCell = setof( x 
									leSearchHierarchy(	CellUnderTest->master
												CellUnderTest->master->bBox
												0
												"inst"
												nil
												)
									( x->libName != "tsmc13lg" && x->libName != "gate" && x->libName != "stack" )
									)




						; Filter out SLACK
						if( IsMidLevelCell
							then
								rexCompile( "lib.buffer.slack" )
								IsMidLevelCell = !rexExecute( CellUnderTest->cellName )
						)


						; Filter out TOKEN
						if( IsMidLevelCell
							then
								rexCompile( "lib.buffer.token" )
								IsMidLevelCell = !rexExecute( CellUnderTest->cellName )
						)

						; Filter out CTREE
						if( IsMidLevelCell
							then
								rexCompile( "lib.util.ctree" )
								IsMidLevelCell = !rexExecute( CellUnderTest->cellName )
						)


						if( IsMidLevelCell 
							then
								DebugOn && printf( "CompactCells --> Post-Compaction Alignment: \"%s\" is a mid-level cell\n" CellUnderTest->name )
;								MoveAndSnap( 	"ARAV" 
;										?overwrite_fig ScanLine_Cell
;										?overwrite_xSnap caadr( AlignToGrid ) 
;										?overwrite_ySnap cadadr( AlignToGrid ) 
;										?overwrite_xy overwrite_xy
;										)

							else
								DebugOn && printf( "CompactCells --> Post-Compaction Alignment: \"%s\" is a leaf cell\n" CellUnderTest->name )
;								MoveAndSnap( 	"ARAV" 
;										?overwrite_fig ScanLine_Cell
;										?overwrite_xSnap caar( AlignToGrid ) 
;										?overwrite_ySnap cadar( AlignToGrid ) 
;										?overwrite_xy overwrite_xy
;										)

						)

				)



; Hang up here




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









				; =================================
				; Deals with Expansion to the Right
				; =================================
 
;				ScanLine_l_OverlappedCells = dbProduceOverlapInst( geGetWindowCellView( ) ScanLine_Cell->bBox 1:1 )
				ScanLine_l_OverlappedCells = dbProduceOverlapInst( geGetWindowCellView( ) ScanLine_Cell->bBox )

;				println( ScanLine_l_OverlappedCells )



				ScanLine_l_OverlappedCells = setof( x ScanLine_l_OverlappedCells member(x l_ValidCells))

				SkipLoop = nil

				foreach( ScanLine_OverlappedCell ScanLine_l_OverlappedCells

					if( SkipLoop && !member( ScanLine_OverlappedCell l_ValidCells )
						then
							OffSet = 0
						else
							OffSet = GetOffSet( 	ScanLine_Cell 
										ScanLine_OverlappedCell 
										)
					)


					if( OffSet != 0 
						then

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

							if( OffSet > 0
							then

								; MoveExactAbuttedGroup( ScanLine_OverlappedCell OffSet 0 ScanLine_l_Cells_current )


								dbMoveFig( ScanLine_OverlappedCell nil list(OffSet:0 "R0"))

								DebugOn && printf( "\n" )

			 					DebugOn && printf( "CompactCells --> \"%s\" overlapped with \"%s\" ==> \"%s\" is moved right by %n\n" 
									ScanLine_OverlappedCell->name 
									ScanLine_Cell->name
									ScanLine_OverlappedCell->name 
									OffSet)
							else
								; EXCEPTION
			 					printf( "CompactCells --> ERROR!!! OffSet (%n) must be non-negative\n" OffSet) 

							); end if 
					)

				); end foreach

			); end foreach



		; Get Inverse of l_ValidCells 
		geSelectAllFig( )
		l_ValidCells = setof( x geGetSelectedSet( ) !member( x l_ValidCells ))
		geDeselectAllFig( )


		; Flatten All the tempCells 
		geSelectAllFig( )
		foreach( tempCell setof( x geGetSelectedSet( ) x->viewName=="layout.compactor") 
			dbFlattenInst( tempCell 1 )
		)

		; Delete All tempCells in your library
		Counter = 0
		tempCell = t 
		while( tempCell


			tempCell = ddGetObj( 	geGetWindowCellView()->libName
						sprintf( nil "tempCell%n" Counter ) 
						"layout.compactor"
						)

			tempCell && ddDeleteLocal( tempCell )

			Counter = Counter + 1

		)


		; Select back all the ValidCells
		geSelectAllFig( )
		l_ValidCells = setof( x geGetSelectedSet( ) !member( x l_ValidCells ))
		geDeselectAllFig( )


		foreach( ValidCells l_ValidCells

			geSelectFig( ValidCells )

		); end foreach

		dbComputeBBox( geGetWindowCellView())


	); end let

); end procedure


; =============================================================================================================
; This is a simple procedure that you have to run if you want it see if the overlap is due to 
; exact bBox abutment or real overlap
;
; This procedure is NOT meant to use alone. It is serving as a PIPE to the result provided by
; "dbProduceOverlapInst" or similar function
;
; Three Types of returnValue
; (1) >0  - Two objects are overlapped, object on the right will got moved by an offset=returnValue 
; (2) 0   - Two objects are only abutting, should not do anything
; (3) <0  - One Object is within the other one, the smaller object will be moved by an offeset=abs(returnValue)
;

procedure( GetOffSet( 	d_CellA d_CellB 
			@key ( DebugOn nil ))

	let(( 	bBox_A bBox_B 
		returnValue )

		bBox_A = d_CellA->bBox
		bBox_B = d_CellB->bBox


		;==========================
		;|||   Case (2)(3a)(3b) |||
		;==========================
		if( 	caar( bBox_A )	== caadr( bBox_B )  ||
			caar( bBox_B ) 	== caadr( bBox_A )  ||
			cadar( bBox_A ) == cadadr( bBox_B ) ||
			cadar( bBox_B ) == cadadr( bBox_A ) ||
			d_CellA == d_CellB

			then
				returnValue = 0
				; DebugOn && printf( "GetOffSet --> \"%s\" and \"%s\" are abutting\n" d_CellA->name d_CellB->name )

			else

		;===========================
		;|||   Case (1)(3a)(3b)  |||
		;===========================
				returnValue = caadr( bBox_A ) - caar( bBox_B )
				DebugOn && printf( "GetOffSet --> Case (1) \"%s\" and \"%s\" are overlapping\n" d_CellA->name d_CellB->  name )


		); end if Case 2

		returnValue


	); end let

); end procedure




procedure( GetOrderedCellList( @key	( WorkOnAll t )
					( AlignToGrid list( 0.5:9.6 5:9.6 ))
					( DebugOn t )
					( FilterBigCells t )
					( GroupCells t )
					)
	let((	
		l_d_Cell
		bBox_A 
		bBox_B
		SkipLoop
		Counter l_AddedCell l_GroupCell
		returnValue
		)

	d_OverlappedCell = nil

	if( WorkOnAll
		then
			geSelectAllFig( )
	); end if

	; Only get objType == "inst"
	l_d_Cell = setof( x geGetSelectedSet( ) x->objType=="inst" )


	; Align All Objects
	geSelectAllFig( )
	SuperMoveAndSnap( "A" ?AlignToGrid AlignToGrid ?DebugOn nil )
	geDeselectAllFig( )

	; GroupCells
	Counter = 0
	l_AddedCell = nil

	if( GroupCells
		then
			foreach( d_Cell l_d_Cell

			;	l_GroupCell = setof( x l_d_Cell ( caar(d_Cell->bBox)==caar(x->bBox) && caadr(d_Cell->bBox)==caadr(x->bBox) ))

				if( d_Cell->name
					then
						geDeselectAllFig( )
						SelectAbuttedGroup( d_Cell )
						l_GroupCell = geGetSelectedSet( )
						geDeselectAllFig( )

						if( length(l_GroupCell) > 1
							then
								l_AddedCell = cons( leMakeCell(	l_GroupCell 
												geGetWindowCellView()->libName 
												sprintf( nil "tempCell%n" Counter) 
												"layout.compactor" t 
												) 
										    l_AddedCell 
										    )

								Counter = Counter + 1

								l_d_Cell = setof( x l_d_Cell !member( x l_GroupCell )) 
						)	
				)

			); end foreach

			geSelectAllFig( )
			l_AddedCell = setof( x geGetSelectedSet( ) member( x->master l_AddedCell))

			l_d_Cell = append( l_d_Cell l_AddedCell )

	)


	; Apply x-coordinate of the left edge of the bBox for sorting db -> list( x db )
	l_d_Cell = foreach( mapcar d_Cell l_d_Cell

		list( caar( d_Cell->bBox ) d_Cell )

	); end foreach



	; Sort by car
	l_d_Cell = sortcar( l_d_Cell `lessp)


	; Rearrange from list( x db ) to list( db x ) 
	l_d_Cell = foreach( mapcar d_Cell l_d_Cell

		list( cadr( d_Cell ) car( d_Cell )) 

	); end foreach

	if( FilterBigCells
		then
		foreach( d_Cell l_d_Cell

			bBox_A = car(d_Cell)->bBox

			SkipLoop = nil
	
			foreach( CELL dbProduceOverlapInst( geGetWindowCellView( ) bBox_A 0:0 )
			
				if( !SkipLoop
					then
						bBox_B = CELL->bBox

						if( (	caar( bBox_A ) <= caar( bBox_B ) && 
							caadr( bBox_A ) >= caadr( bBox_B ) && 
							cadar( bBox_A ) <= cadar( bBox_B ) && 
							cadadr( bBox_A ) >= cadadr( bBox_B ) &&
							car(d_Cell) != CELL
							)

							then
								DebugOn && printf( "GetOrderedCellList --> \"%s\" is filtered since it completely contains \"%s\" \n" 
											car(d_Cell)->name 
											CELL->name )
 
								l_d_Cell = remove( d_Cell l_d_Cell )
								
								geDeselectAllFig( )
								geSelectFig( car( d_Cell ) )

								Old_x = car( car( d_Cell )->xy)
								Old_y = cadr( car( d_Cell )->xy)

;								if( AlignToGrid
;									then
;										; Align to Grid
;										SuperMoveAndSnap( "A" ?AlignToGrid AlignToGrid ?DebugOn nil )
;										SuperMoveAndSnap( "L" ?AlignToGrid AlignToGrid ?DebugOn nil )
;										SuperMoveAndSnap( "R" ?AlignToGrid AlignToGrid ?DebugOn nil )
;										SuperMoveAndSnap( "U" ?AlignToGrid AlignToGrid ?DebugOn nil )
;										SuperMoveAndSnap( "D" ?AlignToGrid AlignToGrid ?DebugOn nil )
;								)

								geDeselectAllFig( )

								SkipLoop = t
						)
				)

			); end foreach


		); end foreach
	); end if



	if( DebugOn 
		then
			printf( "GetOrderedCellList --> ")
			foreach( d_Cell l_d_Cell
				printf( "%s " car(d_Cell)->name )
			); end foreach
			printf( "\n")

	); end if

	l_d_Cell

	); end let

); end procedure



procedure( SelectAbuttedGroup( 	d_CellA 
				@key 	( ProcessTop t ) 
					( ProcessBottom t ) 
					( LinkedVertical t )
					( DebugOn nil )
					)

	let((	returnValue
		l_d_Cell_Overlapped
		bBox_A bBox_B
		)

	returnValue = nil

	geSelectFig( d_CellA )

	bBox_A = d_CellA->bBox

	l_d_Cell_Overlapped = dbProduceOverlapInst( geGetWindowCellView( ) bBox_A )

	DebugOn && printf( "%s " d_CellA->name )

	foreach( d_CellB l_d_Cell_Overlapped

		bBox_B = d_CellB->bBox

		if( 	cadar( bBox_B ) == cadadr( bBox_A ) && 
			caar( bBox_A )  == caar( bBox_B )   && 
			caadr( bBox_B ) == caadr( bBox_A )  &&
			d_CellA != d_CellB && ProcessTop 

			then
				returnValue = t
				CellTop = SelectAbuttedGroup( d_CellB ?ProcessTop t ?ProcessBottom nil )

			else

		if( 	cadar( bBox_A ) == cadadr( bBox_B ) && 
			caar( bBox_A )  == caar( bBox_B )   && 
			caadr( bBox_B ) == caadr( bBox_A )  && 
			d_CellA != d_CellB  && ProcessBottom 

			then
				returnValue = t
				CellBottom = SelectAbuttedGroup( d_CellB ?ProcessTop nil ?ProcessBottom t )
		); end if

		); end if

	); end foreach

	returnValue

	); end let

); end procedure

;SelectAbuttedGroup( car( geGetSelectedSet( )))







; Global Variable
CellMap = nil

procedure( GenerateCellMap( @key 
					( CellUnderTest geGetWindowCellView( ) )
					( CurrentLevel 0 )
					( Depth 32 )
					( DebugOn nil )
					)

	let((	returnValue
		l_d_Cell
		)

	returnValue = t
	l_d_Cell = nil


	; ==========================
	; Generate List for this level
	; ==========================
	foreach( d_Cell CellUnderTest->instances
		if( !member( d_Cell->master l_d_Cell )
			then
				l_d_Cell = cons( d_Cell->master l_d_Cell )
		)
	)


	; ====================================
	; Continue ONLY if l_d_Cell is not nil
	; ====================================
	if( l_d_Cell

		then

		; Append to Global List
		CellMap = cons( list( CurrentLevel CellUnderTest l_d_Cell ) CellMap )	


		; For Debugging Purpose ONLY
		if( DebugOn 
			then
				printf( "GenerateCellMap --> %2n : %s ==> " CurrentLevel CellUnderTest->cellName )
				foreach( d_Cell l_d_Cell
					printf( "\"%s\" " d_Cell->cellName )
				)
				printf( "\n" )
		); end if
	
		; Resursive Calls
		foreach( d_Cell l_d_Cell

			if( CurrentLevel + 1 <= Depth
				then
					GenerateCellMap( 	?CellUnderTest	d_Cell 
								?CurrentLevel	CurrentLevel+1
								?Depth		Depth
								)
			)

		); end foreach



	); end if

	returnValue

	); end let

); end procedure



procedure( GenerateCellTree( @key 
					( CellUnderTest geGetWindowCellView( ) )
					( Depth 32 )
					( DebugOn nil )
					( WithDuplicate nil )
					)

	let((	cellTree
		currentLevel
		l_d_CellatAllLevels
		)

	cellTree = nil
	currentLevel = Depth
	l_d_CellatAllLevels = nil


	; Globals
	CellMap = nil

	GenerateCellMap( 	?CellUnderTest 	CellUnderTest 
				?Depth		Depth
				)


	declare( l_d_CellatLevel[Depth+1] )

	for( i 0 Depth
		l_d_CellatLevel[i] = nil
	)

	foreach( l_d_Cell CellMap

		if( caddr( l_d_Cell )
			then
				foreach( d_Cell caddr( l_d_Cell )
 					
					if( !member( d_Cell l_d_CellatLevel[car(l_d_Cell)] )
						then
							l_d_CellatLevel[car(l_d_Cell)] = cons( d_Cell l_d_CellatLevel[car(l_d_Cell)] )
					)

					if( !member( d_Cell l_d_CellatAllLevels )
						then
							l_d_CellatAllLevels = cons( d_Cell l_d_CellatAllLevels )
					)

				); end foreach
		); end if
 
	); end foreach


	; Get rid of duplicates on every levels
	if( !WithDuplicate
		then
			while( currentLevel >= 0

				l_d_CellatLevel[currentLevel] = setof( x l_d_CellatLevel[currentLevel] member( x l_d_CellatAllLevels ))
				l_d_CellatAllLevels = setof( x l_d_CellatAllLevels !member( x l_d_CellatLevel[currentLevel] ))
				currentLevel = currentLevel - 1

			); end while
	); end if


	for( i 0 Depth
		cellTree = append( cellTree list( list( i l_d_CellatLevel[i])))
	)

	cellTree

	); end let


); end procedure




; ============================================================================
; Hierarchical Version of CompactCells
; ============================================================================
; Depth = -1: work on current cell only (CellUnderTest)
; Depth = 0:  work on all children cell at current level (CellUnderTest + Level 0 Cells) 
;
; If Depth >=0 ==> WorkOnAll Parameter has NO effect

procedure( CompactCellsResursive( @key
					( Depth 32 )
					( WorkOnAll nil )
					( AlignToGrid list( 0.5:9.6 5:9.6 ))

					( AutoSave nil )
					( CellUnderTest geGetWindowCellView( ) )
					( DebugOn nil )
					( StepByStep nil )
				)

	let((	returnValue
		CellTree
		currentLevel
		)

	libName = CellUnderTest->libName
	cellName = CellUnderTest->cellName
	viewName = CellUnderTest->viewName

	; Local
	returnValue = t
	currentLevel = Depth

	if( Depth >= 0
		then
			CellTree = GenerateCellTree( 	?CellUnderTest 	CellUnderTest 
							?Depth		Depth
							?WithDuplicate	nil
							)
			WorkOnAll = t
	)

	while( currentLevel >= 0 

		foreach( d_Cell cadr( assoc( currentLevel CellTree ))

			DebugOn && printf( "CompactCellsResursive --> CompactCells on \"%s\" \n" d_Cell->cellName )  

			geOpen(	?window		geGetCellViewWindow( geGetWindowCellView( ) )
				?lib		d_Cell->libName
				?cell		d_Cell->cellName
				?view		d_Cell->viewName
				?viewType	"maskLayout"
				?mode		"a"
				)

			CompactCells( 	?WorkOnAll	WorkOnAll
					?AlignToGrid	AlignToGrid
					)

			dbComputeBBox( geGetWindowCellView())

			geRefresh( )

			AutoSave && geSave( )

		); end foreach

		currentLevel = currentLevel - 1

	); end while

	; Work On CellUnderTest

	DebugOn && printf( "CompactCellsResursive --> CompactCells on \"%s\" \n" cellName )  


	if( Depth >= 0
		then
			geOpen(	?window		geGetCellViewWindow( geGetWindowCellView( ) )
				?lib		libName
				?cell		cellName
				?view		viewName
				?viewType	"maskLayout"
				?mode		"a"
				)
	)
	

	CompactCells( 	?WorkOnAll	WorkOnAll
			?AlignToGrid	AlignToGrid
			)

	geRefresh( )
	AutoSave && geSave( )

	returnValue

	); end let

); end procedure



