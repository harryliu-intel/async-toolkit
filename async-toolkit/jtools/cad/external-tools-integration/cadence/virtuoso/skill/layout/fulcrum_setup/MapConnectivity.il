procedure( MapConnectivity( @key
				( Filename	"/home/user/samson/myskill/IO.map.x6" )
				( WarningOn	t )
				)
	let((	inPort Arg1 Arg2 l_Arg nextLine
		NetParent
		NetChild
		NetID
		CurrentCell
		l_d_Cell Cell
		)

	inPort = infile( Filename )
	CurrentCell = nil

	geSelectAllFig( )
	l_d_Cell = geGetSelectedSet( )
	geDeselectAllFig( )


	when( inPort

		while( inPort && gets( IOEntry inPort )

		; Filter out for Comment Line
		rexCompile( "^;" )

		if( parseString( IOEntry ) && !rexExecute( IOEntry )
			then
				IOEntryList = parseString( IOEntry ) 

				Arg0 = nth( 0 IOEntryList )
				Arg1 = nth( 1 IOEntryList )
				Arg2 = nth( 2 IOEntryList )
				Arg3 = nth( 3 IOEntryList )
				Arg4 = nth( 4 IOEntryList )

				if( Arg2
					then
						LoopConstruct = Arg2
						LoopStart     = evalstring( Arg3 )
						LoopEnd       = evalstring( Arg4 )
					else
						LoopConstruct = "$$"
						LoopStart     = -1
						LoopEnd       = -1
				); end if


				if( Arg0 == "c"
					then
						TargetCell = Arg1
						LoopConstructCell = LoopConstruct
						LoopStartCell     = LoopStart
						LoopEndCell       = LoopEnd
					else

						for( i LoopStartCell LoopEndCell

							rexCompile( sprintf( nil "\\$%s" LoopConstructCell ))
							CurrentCell = rexReplace( TargetCell sprintf( nil "%n" i ) 0 )

							foreach( Cell l_d_Cell
								if( Cell->name == CurrentCell
									then
										for( j LoopStart LoopEnd
											rexCompile( sprintf( nil "\\$%s" LoopConstructCell ))
											NetParent = rexReplace( Arg0 sprintf( nil "%n" i ) 0 )
											NetChild  = rexReplace( Arg1 sprintf( nil "%n" i ) 0 )

											rexCompile( sprintf( nil "\\$%s" LoopConstruct ))
											NetParent = rexReplace( NetParent sprintf( nil "%n" j ) 0 )
											NetChild  = rexReplace( NetChild sprintf( nil "%n" j ) 0 )
											NetID = dbMakeNet( geGetWindowCellView() NetChild )
											dbCreateConnByName( NetID Cell NetParent )
										); end for									
								); end if
							); end foreach
						); end for
				); end if
		);end if


		); end while
	);end when

	close( inPort )

	); end let

); end procedure
