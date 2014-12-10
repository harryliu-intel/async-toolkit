; This Fuction is used to create a resonable floorplan for a given initial layout
; It will retain the rotation properties of the cells 
; Assumption
; All Bit-slice cells are in the form "AAA[NNN][NNN]" or "AAA[NNN]" 
; All non-bit-slice cells can be named anything but not containing "[" or "]"

procedure( Floorplan( )

	; local Varibales
	let( ( 	l_d_SelectedCell
		l_d_NonArrayCell
		x_org x xOffset
		y_org y yOffset
		matchPattern
		returnValue
		maxDataPathHeight 
		NonArrayCellHeight 
		bitHeight)

		l_d_SelectedCell = geGetSelectedSet( )
		l_d_NonArrayCell = nil
		returnValue = t
		x_org = 0
		y_org = 0
		x = x_org
		y = y_org
		maxDataPathHeight = 0
		NonArrayCellHeight = 0
		bitHeight = 19.2

	geDeselectAllFig( geGetWindowCellView() )

	; Filter out Non-bit-slice Cells
;  	rexCompile( "\\[[0-9]+\\]$" )
  	rexCompile( "\\[[0-9]+\\][\\.]*[a-zA-Z0-9]*$" )
	foreach( d_SelectedCell l_d_SelectedCell
		
		if( rexExecute( d_SelectedCell->name ) == nil
			then
				l_d_SelectedCell = remove( d_SelectedCell l_d_SelectedCell )
				l_d_NonArrayCell = cons( d_SelectedCell l_d_NonArrayCell )
		); end if


	); end foreach  


	while( l_d_SelectedCell != nil

		geDeselectAllFig( geGetWindowCellView() )


		matchPattern = car( l_d_SelectedCell )->name
		rexCompile( "\\[" )
		matchPattern = rexReplace( matchPattern "\\\\\\[" 0 )
		rexCompile( "\\]" )
		matchPattern = rexReplace( matchPattern "\\\\\\]" 0 )
;		rexCompile( "\\[[0-9]+\\\\\\]$" )
		rexCompile( "\\[[0-9]+\\\\\\][\\.]*[a-zA-Z0-9]*$" )
;		matchPattern = rexReplace( matchPattern "[[0-9]+\\\\\\]$" 0 )
		matchPattern = rexReplace( matchPattern "[[0-9]+\\\\\\][\\.]*[a-zA-Z0-9]*$" 0 )

  		rexCompile( matchPattern )

		xOffset = abs( car( car( car( l_d_SelectedCell )->bBox)) - car( car( cdr( car( l_d_SelectedCell )->bBox))))

		printf( "Floorplan --> Start forming DataPath for Pattern %s \n" matchPattern )
		printf( "Floorplan --> " )

		; Re-initialize Variable
		numArrayCell = 0

		foreach( d_SelectedCell l_d_SelectedCell
	
			if( rexExecute( d_SelectedCell->name )
				then
					geSelectFig( d_SelectedCell )
					l_d_SelectedCell = remove( d_SelectedCell l_d_SelectedCell )
					numArrayCell = numArrayCell + 1
					heightArrayCell = abs( car( cdr( car( d_SelectedCell->bBox ))) - car( cdr( car( cdr( d_SelectedCell->bBox )))))
					printf( "%s " d_SelectedCell->name)

			); end if			

		); end foreach
	
		maxDataPathHeight = max( maxDataPathHeight numArrayCell*heightArrayCell )

		printf( "\n" )		
		printf( "Floorplan --> End forming DataPath for Pattern %s \n" matchPattern )

		AlignCellsV( x y 0 nil ) 

		x = x + xOffset

	); end while

	; Report maxDataPathHeight
	printf( "Floorplan --> maxDataPathHeight = %n \n" maxDataPathHeight )
	printf( "Floorplan --> Height of 8 bits  = %n \n" bitHeight*8 )
	printf( "Floorplan --> Any non-array cells with height > min( %n %n ) will be treated as part of Datapath \n" maxDataPathHeight*0.45 bitHeight*8 )




	; Doing non-array cells that's part of DataPath

	geDeselectAllFig( geGetWindowCellView() )

	foreach( d_NonArrayCell l_d_NonArrayCell

		NonArrayCellHeight = abs( car( cdr( car( d_NonArrayCell->bBox ))) - car( cdr( car( cdr( d_NonArrayCell->bBox )))))
		if( NonArrayCellHeight > min( maxDataPathHeight*0.45 bitHeight*8 ) 

			then
				printf( "Floorplan --> Non-array Cell \"%s\" with a height of %n is being added into the Datapath \n" d_NonArrayCell->name NonArrayCellHeight )
				geSelectFig( d_NonArrayCell )
				l_d_NonArrayCell = remove( d_NonArrayCell l_d_NonArrayCell )
				AlignCellsV( x y 0 nil ) 
				xOffset = abs( car( car( d_NonArrayCell->bBox )) - car( car( cdr( d_NonArrayCell->bBox ))))
				x = x + xOffset
				geDeselectAllFig( geGetWindowCellView() )

		) ;end if

	); end foreach


	
	; Doing non-array cells that's NOT part of DataPath
	; Reset Variables
	geDeselectAllFig( geGetWindowCellView() )
	x = x_org
	y = y_org

	foreach( d_NonArrayCell l_d_NonArrayCell
		printf( "Floorplan --> Non-array Cell \"%s\" is being added into Control \n" d_NonArrayCell->name )
		yOffset = abs( car( cdr( car( d_NonArrayCell->bBox ))) - car( cdr( car( cdr( d_NonArrayCell->bBox )))))
		y = y_org - yOffset

		geSelectFig( d_NonArrayCell )
		AlignCellsV( x y 0 nil )
		geDeselectAllFig( geGetWindowCellView() )

		xOffset = abs( car( car( d_NonArrayCell->bBox )) - car( car( cdr( d_NonArrayCell->bBox ))))
		x = x + xOffset

	); end foreach


	; Return Value
	returnValue

	) ; end let

); end procedure

; ============================================================
; Useful Pattern Matching Code
; ============================================================
;A = "BUF[9]"

;rexCompile( "\\[" )
;A = rexReplace( A "\\\\\\[" 0 )
;rexCompile( "\\]" )
;A = rexReplace( A "\\\\\\]" 0 )


;rexCompile( "\\[[0-9]+\\\\\\]$" )
;matchPattern = rexReplace( A "[[0-9]+\\\\\\]$" 0 )

; ============================================================






