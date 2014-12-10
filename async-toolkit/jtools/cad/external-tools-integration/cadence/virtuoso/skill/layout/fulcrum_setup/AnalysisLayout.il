; Analysis Layout
;
; Optional Parameters
; 1. WorkOnAll - "Work on All Cells" VS "Work on Selected Cells"
; 2. DebugOn - Print Debugging Messages 
; 3. DatapathHeight
; 4. DatapathThreshold
; 5. ScanLine - Get Cells by performing Scan Lines from Left to Right (to reserve ordering)
;
; Global Variables

TR_Floorplanner_Counter_Data   = 0
TR_Floorplanner_Counter_Ctrl   = 0
TR_Floorplanner_DatapathHeight = 0


procedure( AnalysisLayout( @key
                        ( WorkOnAll t )
			( DebugOn   nil )
			( DatapathHeight 0 )
			( DatapathThreshold 0.4 )
			( ScanLine  nil )			
                        )

	let( (	
		SelectedCellDone
		l_d_Selected l_d_Selected_temp
		l_CellGroup
		l_PredefinedPattern
		FormNewPattern
		D0StartIndex
		Counter_Data Counter_Ctrl
		CellGroup_New CellGroup_Old
		Order_Old Order_New
		ScanLine_bBox_original ScanLine_bBox_current 
		newPattern
		)

	SelectedCellDone    = nil
	FormNewPattern	    = nil
	l_PredefinedPattern = nil
	l_CellGroup         = nil
	Counter_Data        = 0
	Counter_Ctrl        = 0
	ScanLine_l_Cells    = nil

        if( !WorkOnAll && !ScanLine
                then
        		l_d_Selected = geGetSelectedSet()
        )

        if( WorkOnAll && !ScanLine
                then
                        geSelectAllFig()
        		l_d_Selected = geGetSelectedSet()
        )

	; Filter out non-instances
	; =========================
	l_d_Selected = setof( x l_d_Selected x->objType=="inst" )


	if( ScanLine
		then
			l_d_Selected_temp = geGetSelectedSet()

			ScanLine_bBox_original = geGetWindowCellView()->bBox

			DebugOn && printf( "AnalysisLayout --> Running ScanLines\n")

			for( i floor( caar( ScanLine_bBox_original )) floor( caadr( ScanLine_bBox_original ))

				ScanLine_bBox_current = list(
								car( ScanLine_bBox_original )
								list( i+1 cadadr( ScanLine_bBox_original ))
							)
								
				geSingleSelectBox( geGetCellViewWindow( geGetWindowCellView()) t ScanLine_bBox_current )

				l_d_Selected = geGetSelectedSet()

				if( !WorkOnAll
					then
						foreach( d_Selected l_d_Selected
							if( member( d_Selected l_d_Selected_temp ) == nil
								then
									l_d_Selected = remove( d_Selected l_d_Selected )
									geDeselectFig( d_Selected )
							)
			
						)

				); end if

				ScanLine_l_Cells = nconc( ScanLine_l_Cells setof( x l_d_Selected !member(x ScanLine_l_Cells)))
			)
        		l_d_Selected = ScanLine_l_Cells
	)

	foreach( d_Selected l_d_Selected

		SelectedCellDone = nil

		; First match the pre-defined patterns
		for( i 0 length( l_PredefinedPattern )-1 

			rexCompile( nth( i l_PredefinedPattern ))
			if( rexExecute( d_Selected->name )
				then
					if( DebugOn
						then
							printf( "AnalysisLayout --> Pattern %n: %s matches %s\n"
								i + 1 
								nth( i l_PredefinedPattern )
								d_Selected->name )
					)

					SelectedCellDone = t
					evalstring( sprintf( nil "CellGroup_%n = 
							list( 	car(CellGroup_%n)
								cadr(CellGroup_%n)
								caddr(CellGroup_%n)
								cadddr(CellGroup_%n)
								reverse( cons( 	
									d_Selected
									reverse( car( cddddr( CellGroup_%n))))))" 
							i + 1
							i + 1
							i + 1
							i + 1
							i + 1
							i + 1
							))

			)
			 
		); end for

	
		; Deal with 3-D Array
		rexCompile( ConvertWildCardString( "*[*]*[*]*[*]*" ))
		if( rexExecute( d_Selected->name ) && !SelectedCellDone
			then
				DebugOn && printf( "AnalysisLayout --> 3-D Array Found %s\n" d_Selected->name )
				SelectedCellDone = t
				FormNewPattern = t
				NewPatternDimension = 3
				NewPatternType = "Data"
				NewPatternOrientation = "NA"
				NewPatternHeight = cadadr( d_Selected->bBox ) - cadar( d_Selected->bBox )
		)

		; Deal with 2-D Array
		rexCompile( ConvertWildCardString( "*[*]*[*]*" ))
		if( rexExecute( d_Selected->name ) && !SelectedCellDone
			then
				DebugOn && printf( "AnalysisLayout --> 2-D Array Found %s\n" d_Selected->name )
				SelectedCellDone = t
				FormNewPattern = t
				NewPatternDimension = 2
				NewPatternType = "Data"
				NewPatternOrientation = "HV"
				NewPatternHeight = cadadr( d_Selected->bBox ) - cadar( d_Selected->bBox )
		)

		; Deal with 1-D Array
		rexCompile( ConvertWildCardString( "*[*]*" ))
		if( rexExecute( d_Selected->name ) && !SelectedCellDone
			then
				DebugOn && printf( "AnalysisLayout --> 1-D Array Found %s\n" d_Selected->name )
				SelectedCellDone = t
				FormNewPattern = t
				NewPatternDimension = 1
				NewPatternType = "Data"
				NewPatternOrientation = "V"
				NewPatternHeight = cadadr( d_Selected->bBox ) - cadar( d_Selected->bBox )
		) 

		; Deal with the remaining cells
		if( !SelectedCellDone
			then
				DebugOn && printf( "AnalysisLayout --> Single Cell Found %s\n" d_Selected->name )
				SelectedCellDone = t
				FormNewPattern = t
				NewPatternDimension = 0
				NewPatternType = "Ctrl"
				NewPatternOrientation = "NA"
				NewPatternHeight = cadadr( d_Selected->bBox ) - cadar( d_Selected->bBox )
		)

		; Compiling New Pattern for matching
		if( FormNewPattern
			then
				; Creat a New Pattern
				rexCompile( ConvertWildCardString( "[+]" ?FullString nil ))

				newPattern = rexReplace( 	d_Selected->name 
								"\\\\[[0-9\\\\-]*\\\\]" 
								0 )

				newPattern = list( strcat( newPattern "$" ) )

				l_PredefinedPattern = append( l_PredefinedPattern newPattern )



;				l_PredefinedPattern = reverse( 	cons( 	rexReplace( 	d_Selected->name 
;											"\\\\[[0-9\\\\-]*\\\\]" 
;											0 ) 
;								reverse( l_PredefinedPattern ) 
;								))



				evalstring( sprintf( nil "CellGroup_%n = list( 	list( %n \"%s\" nil ) 
										list( \"%s\" -1 ) 
										%n \"%s\" list( d_Selected ))" 
					length( l_PredefinedPattern )
					NewPatternDimension
					NewPatternOrientation
					NewPatternType
					NewPatternHeight
					rexReplace( d_Selected->name "[+]" 0 )
					))

				FormNewPattern = nil
		)

	); end foreach



	; ======================================================================================
	; Concat all the CellGroup_n
	; ======================================================================================
	l_CellGroup = nil

	for( i 0 length( l_PredefinedPattern )-1 
		evalstring( sprintf( nil "l_CellGroup = xcons( l_CellGroup CellGroup_%n )" i+1 ))
	) ;end for
	l_CellGroup = reverse( l_CellGroup )


	; ======================================================================================
	; Post Processing 
	; ======================================================================================

	; Calculate the Datapath Height
	if( DatapathHeight <= 0 

		then
	
			TR_Floorplanner_DatapathHeight = 0
			
			for( i 0 length(l_CellGroup)-1

				TR_Floorplanner_DatapathHeight = max(TR_Floorplanner_DatapathHeight caddr(nth(i l_CellGroup))*length(car(cddddr(nth(i l_CellGroup)))))
				
			); end foreach	

		else
			TR_Floorplanner_DatapathHeight = DatapathHeight


	); end if

	DebugOn && printf( "AnalysisLayout --> TR_Floorplanner_DatapathHeight = %n\n" TR_Floorplanner_DatapathHeight )
	DebugOn && printf( "AnalysisLayout --> DatapathThreshold = %n\n" DatapathThreshold )

	; Changing Cells Types "Data" <-> "Ctrl" if necessary
	for( i 0 length(l_CellGroup)-1 

		Height = caddr(nth(i l_CellGroup))*length(car(cddddr(nth(i l_CellGroup))))

		; Changing from "Ctrl" to "Data"
		if(  Height > ( TR_Floorplanner_DatapathHeight*DatapathThreshold )
			then

				CellGroup_Old = nth(i l_CellGroup )
				CellGroup_New = subst( "Data" "Ctrl" nth(i l_CellGroup))
				l_CellGroup = subst( CellGroup_New CellGroup_Old l_CellGroup )	
		)
	
		; Changing from "Data" to "Ctrl"
		if(  Height < ( TR_Floorplanner_DatapathHeight*DatapathThreshold )
			then

				CellGroup_Old = nth(i l_CellGroup )
				CellGroup_New = subst( "Ctrl" "Data" nth(i l_CellGroup))
				l_CellGroup = subst( CellGroup_New CellGroup_Old l_CellGroup )	
		)
	
	)

	; Add Ordering	
	for( i 0 length(l_CellGroup)-1 

		case( caadr( nth(i l_CellGroup ))

			( "Data"
				Counter_Data = Counter_Data + 1
				Counter_Current = Counter_Data
			)
				
			( "Ctrl"
				Counter_Ctrl = Counter_Ctrl + 1
				Counter_Current = Counter_Ctrl
			)

		); end case
				
		CellGroup_Old = nth( i l_CellGroup )
		Order_Old = cadr( CellGroup_Old )
		Order_New = list( caadr( CellGroup_Old ) Counter_Current )

		CellGroup_New = subst( Order_New Order_Old nth(i l_CellGroup))
		l_CellGroup = subst( CellGroup_New CellGroup_Old l_CellGroup )	

	)


	TR_Floorplanner_Counter_Data = Counter_Data
	TR_Floorplanner_Counter_Ctrl = Counter_Ctrl

	l_CellGroup	


	);end let

); end procedure



