procedure( TR_GetManhattanDistance_ProcMain( @key
						( DebugOn	t )
						( WorkOnAll	t )
						( Filename	"./TR_GetManhattanDistance.output.txt" )
						( DumpFile	t )
						)
	let((	outPort
		l_Cell	Cell
		l_Pin	Pin
		l_MatchPin
		l_return
		P1	P1x	P1y
		P2	P2x	P2y
		distance
		)

	; Perform on All Objects
	; ====================== 
	WorkOnAll && geSelectAllFig( )

	; Local Variables
	; ===============
	l_Cell = geGetSelectedSet( )
	l_Cell = setof( x l_Cell x->objType == "inst" )
	l_return = nil
	outPort = outfile( Filename )

	; Bring Up Inplace Pins
	; =====================
	foreach( Cell l_Cell

		DrawIO_ProcDrawInPlacePinsAndLabels( Cell )

	); end foreach

	; Get Only Objects with Connectivity
	; ==================================
	geSelectAllFig( )
	l_Pin = geGetSelectedSet( )
	l_Pin = setof( x l_Pin x->net->name )

	; Sort the List
	; =============
	l_Pin = foreach( mapcar Pin l_Pin
			list( Pin->net->name Pin )
			)
	l_Pin = sortcar( l_Pin `alphalessp )	
	l_Pin = foreach( mapcar Pin l_Pin
			cadr( Pin )
			)


	; Actual Processing
	; =================
	while( l_Pin

		l_MatchPin = setof( x l_Pin x->net->name == car( l_Pin )->net->name)
		l_Pin      = setof( x l_Pin !member( x l_MatchPin ))
		l_return   = append( l_return list( l_MatchPin ))

		if( DumpFile

			then

			case( length( l_MatchPin )
	
				( 2 
					P1 = car( nth( 0 l_MatchPin )->bBox )
					P2 = car( nth( 1 l_MatchPin )->bBox )
	
					P1x = car( P1 )
					P1y = cadr( P1 )
	
					P2x = car( P2 )
					P2y = cadr( P2 )

;					distance = round( sqrt( (P2y-P1y)*(P2y-P1y) + (P2x-P1x)*(P2x-P1x) ))
					distance = round( abs(P2y-P1y) + abs(P2x-P1x))
					fprintf( outPort "%-35s %-4n\n" car( l_MatchPin )->net->name distance )

				)

			); end case

		); end if

	); end while

	close( outPort )

	; Return Value
	; ============
	l_return

	); end let

); end procedure

