; This Fuction is used to align all the selected cells on to form a vertical datapath
; It only works if the selected cells are of the same master
; It will retain the rotation properties of the cells 
procedure( AlignCellsV( x y h stickX )
	
	; local Varibales
	let( ( 	l_d_SelectedCell 
		xOffset x_Selected
		yOffset y_Selected 
		returnValue)

		l_d_SelectedCell = geGetSelectedSet( )

		returnValue = t

	;Verifying the validity of the input
	;Check to make sure that the user has selected at least 1 cell (inst)
	if( length( l_d_SelectedCell ) > 0 && car( l_d_SelectedCell )->objType == "inst"
		then
			d_masterCell = car( l_d_SelectedCell )->master

			;Check to make sure that they are the same master
			foreach( d_SelectedCell l_d_SelectedCell

				if( d_SelectedCell->master != d_masterCell

					then 
						returnValue = nil
						println( "AlignCellsV ---> Error!!! Instances selected belong to different cell types" )
				) ;end if

			) ;end for

		else

			returnValue = nil
			println( "AlignCellsV ---> Error!!! You must select at least ONE instance" )

	) ;end if
	

	;Do the Vertical DataPath Alignment
	if( returnValue == t 
		then
			l_d_SelectedCell = dbSortDBList( l_d_SelectedCell )

			printf( "AlignCellsV ---> New ordering from dbSortDBList\n")
			printf( "AlignCellsV ---> ")

			foreach( d_SelectedCell l_d_SelectedCell

				printf( "%s " d_SelectedCell->name)

         		      	x_Selected  = car( car( d_SelectedCell->bBox ) )
        		        y_Selected  = car( cdr( car( d_SelectedCell->bBox ) ) )


				if( stickX
					then
						xOffset = 0
					else
						xOffset = x - x_Selected
				); end if

       		         	yOffset = y - y_Selected

       		         	dbMoveFig( d_SelectedCell, nil, list(xOffset:yOffset "R0"))

				if( h == 0
					then
						y = car( cdr( car( cdr( d_SelectedCell->bBox ) ) ) )
					else
						y = y + h
				); end if
 		
			) ;end for

			printf( "\n")

	) ;end if

	; Return Value
	returnValue

	); end let

); end procedure


; This Fuction is an advanced GUI Version of AlignCellsV

; Create Form Entries 
trAlignCellsVbitPitch = hiCreateStringField( 
   	?name 'trAlignCellsVbitPitch 
        ?prompt "Enter Desired Bit Pitch" 
        ?defValue "19.2" 
        ?editable t 
)

trAlignCellsVnumBits = hiCreateRadioField( 
        ?name 'trAlignCellsVnumBits 
        ?choices '( "1" "2" "3" "4" "6" "8" "16")
        ?prompt "Number of Bits for each Cell"
        ?defValue "1" 
)

trAlignCellsVstickXY = hiCreateToggleField( 
	?name 'trAlignCellsVstickXY
        ?choices list( list( `stickX "x" ) )
        ?prompt "Stick Coordinates for"
        ?defValue list( t ) 
)


trAlignCellsVForm = hiCreateAppForm( 
        ?name 'trAlignCellsVForm
        ?fields  '( trAlignCellsVbitPitch trAlignCellsVnumBits trAlignCellsVstickXY )
        ?callback "trAlignCellsVCallBack( hiGetCurrentForm() )" 
)
	
; This is the callback function
procedure( trAlignCellsVCallBack( theForm )

        let( (  x
                y
		bitPitch
		numBits
                ArrayHeight
		stickX )

	bitPitch = evalstring( theForm->trAlignCellsVbitPitch->value )
	numBits = evalstring( theForm->trAlignCellsVnumBits->value )
	ArrayHeight = bitPitch * numBits
	stickX = trAlignCellsVForm->trAlignCellsVstickXY->stickX->value

	ArrayHeight = bitPitch * numBits
	x = 0
	y = 0

	println( ArrayHeight )

	AlignCellsV( x y ArrayHeight stickX )

	); end let
); end procedure




