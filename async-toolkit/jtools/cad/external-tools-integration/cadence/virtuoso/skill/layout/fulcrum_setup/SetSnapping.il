procedure( SetSnapping( @key
			( xSnap		0.01 )
			( ySnap 	0.01 )
			( DebugOn	nil )
			)

	let((	outPort
		outFilename
		outFilenameHash
                tempdir
		)

	; Local Variables
	; ===============
        tempdir = ConfigFileGetValue( TheCDSConfigTable "TEMP" )
	outFilename = sprintf( nil "%s/SetSnapping.replayFile" tempdir )
	outPort     = outfile( outFilename ) 

	; Dealing with Special Characters in the Filename
	; ===============================================
	rexCompile( "\\." )
	outFilenameHash = rexReplace( outFilename  "\\\\." 0 )
	rexCompile( "\\/" )
	outFilenameHash = rexReplace( outFilenameHash  "\\\\/" 0 )

	; Delete Previous File
	; ====================
	sh( sprintf( nil "rm -rf $s" outFilenameHash ))

	; Create Replay File
	; ==================
	fprintf( outPort "leHiEditDisplayOptions()\n" )
	fprintf( outPort "leDisplayOptionsForm->xSnap->value = %n\n" xSnap )
	fprintf( outPort "leDisplayOptionsForm->ySnap->value = %n\n" ySnap )
	fprintf( outPort "hiFormDone(leDisplayOptionsForm)\n" )

	close( outPort )

	; Execute the Replay File
	; =======================
	hiReplayFile( outFilename )

	; Delete Replay File
	; ====================
	sh( sprintf( nil "rm -rf $s" outFilenameHash ))

	); end let

); end procedure




procedure( InitCycleSnap()
  CycleSnap = 0
)
InitCycleSnap()

procedure( CycleSnapGrids()
  when( mod(CycleSnap 4)==0
    SetSnapping( ?xSnap MfgGrid ?ySnap MfgGrid )
  )
  when( mod(CycleSnap 4)==1
    SetSnapping( ?xSnap pitch_m1 ?ySnap pitch_m2 )
  )
  when( mod(CycleSnap 4)==2
    SetSnapping( ?xSnap pitch_m3 ?ySnap pitch_m4 )
  )
  when( mod(CycleSnap 4)==3
    SetSnapping( ?xSnap (car gridAlignment["lego"]) ?ySnap (cadr gridAlignment["lego"]) )
  )
  CycleSnap = CycleSnap+1
)

