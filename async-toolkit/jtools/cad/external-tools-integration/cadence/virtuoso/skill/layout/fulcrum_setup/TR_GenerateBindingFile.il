; ======================================================================================================
; This procedure is used to produce ("merge") the original binding file with the cellName.map file
; (obtained from the Cadence PIPO process)
; ======================================================================================================

procedure( TR_GenerateBindingFile( @key
					( InputCellMapFile	"/home/user/samson/cds_wd/x8/cellName.map" )
					( InputBindingFile	"/home/user/samson/myassura/tsmc_pdk_v1.4/patched/bind.rul" ) 	
					( OutputBindingFile	"/home/user/samson/cds_wd/x8/bind.rul" )
					( DebugOn		t )
					)				
	let((	Line	LineList
		currentBindedCellList
		inPort
		outPort
		skipLoop
		)

	outPort  = outfile( OutputBindingFile )

	; Processing the Original Binding File
	; ========================================================
	currentBindedCellList = nil
	inPort = infile( InputBindingFile )
	println( inPort )
	when( inPort
		while( gets( Line inPort )

			fprintf( outPort "%s" Line )

	                rexCompile( "^c" )
			if( rexExecute( Line )
				then
					LineList = parseString( Line )
					currentBindedCellList = cons( nth( 1 LineList ) currentBindedCellList )
			); end if
		); end while
	); end when
	close( inPort )


	; Processing the cellName.map file
	; ========================================================
	inPort   = infile( InputCellMapFile )
	skipLoop = nil

	fprintf( outPort "; ====================================================\n" ) 
	fprintf( outPort "; The following Lines are inserted automatically\n"       )
	fprintf( outPort "; by the Cadence Skill Script <TR_GenerateBindingFile>\n" ) 
	fprintf( outPort "; ====================================================\n" ) 

	when( inPort

		while( gets( Line inPort )

			skipLoop = nil

			; Filter out cells from "gate" and "stack"
			; ========================================
	                rexCompile( "^gate" )
			if( rexExecute( Line ) then skipLoop = t )

	                rexCompile( "^stack" )
			if( rexExecute( Line ) then skipLoop = t )

			; Filter out Comment Line
			; =======================
	                rexCompile( "^\\#" )
			if( rexExecute( Line ) then skipLoop = t )

			LineList = parseString( Line ) 

			if( !skipLoop && !member( nth( 0 LineList ) currentBindedCellList )
				then
					fprintf( 	outPort 
							"c %s %s\n" 
							nth( 0 LineList )
							nth( 2 LineList )
							)
			); end if

                ); end while
                                
        ); end when

	close( inPort )
	close( outPort )

	); end let

); end procedure
