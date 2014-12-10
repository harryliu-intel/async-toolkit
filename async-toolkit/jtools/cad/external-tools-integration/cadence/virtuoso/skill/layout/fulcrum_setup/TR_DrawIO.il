; ======================================================
; This is a procedure used to compile global variables
; ======================================================
; Project Files (with Global Setup )
; 1. "TR_DrawIO_Globals_demo.il"
; 2. "TR_DrawIO_Globals_x8.il"

procedure( DrawIO_ProcCompileGlobals( @key 
					( Project	"x8" )
					( DriverPitch	40.0 )
					( DebugOn	t )
					)

	evalstring( sprintf( nil "DrawIO_ProcCompileGlobals_%s( ?DriverPitch %n )" Project DriverPitch ))

	DebugOn && printf( "=================================================================================\n")	
	DebugOn && printf( "DrawIO_ProcCompileGlobals --> Compiled Global Variables for Project \"%s\"       \n" Project )
	DebugOn && printf( "=================================================================================\n")	

); end procedure



; ========================================================================
; This procedure is used to read input from file only
; ========================================================================
procedure( DrawIO_ProcReadInput( @key 
					( DebugOn	t   )
					( DeepTrace	nil )
				)

	let((	
		inPort
		IOEntry IOEntryList
		IOGroup		
		IOType
		IOName
		IOSignalList

		)

	; Global Variables
	DrawIO_GlobalRawIOList = nil

	; Local Variables
	inPort = infile( DrawIO_GlobalRawIOFile )

	DebugOn && printf( "DrawIO_ProcReadInput --> Start reading file %s \n" DrawIO_GlobalRawIOFile )	

	when( inPort

		while( gets( IOEntry inPort )

		; Filter out for Comment Line
		rexCompile( "^;" )

		if( parseString( IOEntry ) && !rexExecute( IOEntry )
			then
				IOEntryList = parseString( IOEntry "\t" ) 

				IOGroup= nth( 0 IOEntryList )
				IOType = nth( 1 IOEntryList )
				IOName = nth( 2 IOEntryList )
				IOSignalList = evalstring( nth( 3 IOEntryList ))

				DeepTrace && printf( "DrawIO_ProcReadInput --> Read --> %s %s %s %L \n" IOGroup IOType IOName IOSignalList )

				DrawIO_GlobalRawIOList = cons( list( IOGroup IOType IOName IOSignalList ) DrawIO_GlobalRawIOList )
		); end if

		); end while
				
	); end when

	DebugOn && printf( "DrawIO_ProcReadInput --> Finished reading file %s \n" DrawIO_GlobalRawIOFile )	
	DebugOn && printf( "DrawIO_ProcReadInput --> ----------------------------------------------------------\n")	
	DebugOn && printf( "DrawIO_ProcReadInput --> RAW DATA\n" )	
	DebugOn && printf( "DrawIO_ProcReadInput --> ----------------------------------------------------------\n")	
	DebugOn && printf( "DrawIO_ProcReadInput --> Number of Signal Drivers   = %n \n" length( setof( x DrawIO_GlobalRawIOList cadr(x)=="S")))	
	DebugOn && printf( "DrawIO_ProcReadInput --> Number of IO VDD Drivers   = %n \n" length( setof( x DrawIO_GlobalRawIOList cadr(x)=="PRH")))	
	DebugOn && printf( "DrawIO_ProcReadInput --> Number of IO GND Drivers   = %n \n" length( setof( x DrawIO_GlobalRawIOList cadr(x)=="PRL")))	
	DebugOn && printf( "DrawIO_ProcReadInput --> Number of Core VDD Drivers = %n \n" length( setof( x DrawIO_GlobalRawIOList cadr(x)=="PCH")))	
	DebugOn && printf( "DrawIO_ProcReadInput --> Number of Core GND Drivers = %n \n" length( setof( x DrawIO_GlobalRawIOList cadr(x)=="PCL")))	
	DebugOn && printf( "DrawIO_ProcReadInput --> Number of Dummy Drivers    = %n \n" length( setof( x DrawIO_GlobalRawIOList cadr(x)=="D")))	
	DebugOn && printf( "DrawIO_ProcReadInput --> ----------------------------------------------------------\n")	

	DrawIO_GlobalRawIOList = reverse( DrawIO_GlobalRawIOList )

	); end let

); end procedure




; ========================================================================
; This procedure is completely optional. 
; ========================================================================
procedure( DrawIO_ProcPerformAliasing( @key
					( Filename	"/home/user/samson/myskill/IO.input.x6.alias" )
					( DebugOn	nil )
					)
	let((	inPort
		IOEntry 	IOEntryList

		MasterName	MasterNameList
		TargetName	TargetNameList
		TargetPackageName

		MappingList

		LoopConstruct
		LoopStart
		LoopEnd

		i j k

		RawIO
		)
	
	; Global Variables
	; ================
	DrawIO_GlobalRawIOList
	DrawIO_GlobalRawIOList_Alias  = nil
	DrawIO_GlobalAliasList        = nil

	; Local Variables
	; ===============
	inPort = infile( DrawIO_GlobalAliasFile )

	when( inPort

		while( inPort && gets( IOEntry inPort )

		; Filter out for Comment Line
		rexCompile( "^;" )

		if( parseString( IOEntry ) && !rexExecute( IOEntry )
			then
				IOEntryList = parseString( IOEntry ) 

				case( length( IOEntryList )

					; Command Mode
					; ============
					( 1
						case( nth( 0 IOEntryList )
							( "BREAK"
								DebugOn && printf( "DrawIO_ProcPerformAliasing --> User Break Operation!!!\n" )
								inPort = nil
							)
							( "DEBUG_ON"
								DebugOn = t
							)
							( "DEBUG_OFF"
								DebugOn = nil
							)
						); end case
			
					)

					; Normal Mode (Two Arguments)
					; ===========================
					( 2
						MasterName = nth( 0 IOEntryList )
						TargetName = nth( 1 IOEntryList )
						TargetPackageName = TargetName

						DebugOn && printf( "DrawIO_ProcPerformAliasing --> %25s ==> %-25s ( %s )\n"
								MasterName
								TargetName
								TargetPackageName
								)

						DrawIO_GlobalAliasList =  append( DrawIO_GlobalAliasList list( list( MasterName TargetName TargetPackageName )))
					)
				
					; Normal Mode (Three Arguments)
					; ===========================
					( 3
						MasterName = nth( 0 IOEntryList )
						TargetName = nth( 1 IOEntryList )
						TargetPackageName = nth( 2 IOEntryList )

						DebugOn && printf( "DrawIO_ProcPerformAliasing --> %25s ==> %-25s ( %s )\n"
								MasterName
								TargetName 
								TargetPackageName
								)							

						DrawIO_GlobalAliasList =  append( DrawIO_GlobalAliasList list( list( MasterName TargetName TargetPackageName )))
					)
				
					; Normal Mode (Five Arguments with Loop)
					; ======================================
					( 5
						LoopConstruct     = nth( 2 IOEntryList )
						LoopStart         = evalstring( nth( 3 IOEntryList ))
						LoopEnd           = evalstring( nth( 4 IOEntryList ))

						for( i LoopStart LoopEnd

							rexCompile( sprintf( nil "\\$%s" LoopConstruct ))

							MasterName = rexReplace( nth( 0 IOEntryList ) sprintf( nil "%n" i ) 0 )
							TargetName = rexReplace( nth( 1 IOEntryList ) sprintf( nil "%n" i ) 0 )
							TargetPackageName = TargetName

							DebugOn && printf( "DrawIO_ProcPerformAliasing --> %25s ==> %-25s ( %s )\n"
									MasterName
									TargetName
									TargetPackageName
									)

							DrawIO_GlobalAliasList =  append( DrawIO_GlobalAliasList list( list( MasterName TargetName TargetPackageName )))

						); end for

					)
				
					; Normal Mode (Five Arguments with Loop with Aliased Package Name)
					; ================================================================
					( 6
						LoopConstruct     = nth( 2 IOEntryList )
						LoopStart         = evalstring( nth( 3 IOEntryList ))
						LoopEnd           = evalstring( nth( 4 IOEntryList ))

						for( i LoopStart LoopEnd

							rexCompile( sprintf( nil "\\$%s" LoopConstruct ))

							MasterName = rexReplace( nth( 0 IOEntryList ) sprintf( nil "%n" i ) 0 )
							TargetName = rexReplace( nth( 1 IOEntryList ) sprintf( nil "%n" i ) 0 )
							TargetPackageName = rexReplace( nth( 5 IOEntryList ) sprintf( nil "%n" i ) 0 )

							DebugOn && printf( "DrawIO_ProcPerformAliasing --> %25s ==> %-25s ( %s )\n"
									MasterName
									TargetName
									TargetPackageName 
									)

							DrawIO_GlobalAliasList =  append( DrawIO_GlobalAliasList list( list( MasterName TargetName TargetPackageName )))

						); end for

					)
				
				); end case

		); end if

		); end while
				
	); end when


	foreach( RawIO DrawIO_GlobalRawIOList

		MasterNameList = nth( 3 RawIO )
		TargetNameList = nil

		foreach( MasterName MasterNameList

			MappingList = setof( x DrawIO_GlobalAliasList car(x)==MasterName );

			case( length( MappingList )

				; No Alias Defined
				; ================
				( 0
;					TargetNameList = append( TargetNameList list( MasterName ))
					TargetNameList = append( TargetNameList list( list( MasterName MasterName MasterName )))
				)

				; One Alias Mapping Found (1-to-1 Map)
				; ====================================
				( 1
;					TargetNameList = append( TargetNameList cdr( nth( 0 MappingList )))
					TargetNameList = append( TargetNameList list( append( list( MasterName ) cdr( nth( 0 MappingList )))))
				)

				; Error Found (1-to-many Map)
				; ====================================
				( t
					printf( "ERROR !!! You are trying to map \"%L\"\n" MappingList )
					printf( "ERROR !!! You can NOT have more than one Alias Names for One Master Name!!! \n" )
				)

			); end case

		); end foreach

		DrawIO_GlobalRawIOList_Alias = append( DrawIO_GlobalRawIOList_Alias 
							list(	list( 	nth( 0 RawIO )
									nth( 1 RawIO )
									nth( 2 RawIO )
									TargetNameList
								))
							)

	);end foreach

	DrawIO_GlobalRawIOList = DrawIO_GlobalRawIOList_Alias
	
	); end let

); end procedure








; ========================================================================
; This procedure is used to run analysis of the read-in input
;
;	Supported Modes
;	---------------
;	1. Normal
;	2. FixCount <== Fill gaps with feeders
;	3. FixPitch <== Fill gaps with power drivers (PRH PRL PCH PCL)
; ========================================================================
procedure( DrawIO_ProcAnalysisInput( @key
					( Mode 		"FixCount" )
					( DebugOn	t )
					)

	let((	MaxNumPadsHorizontal MaxNumPadsVertical MaxNumPadsChip
		EffectiveWidth EffectiveHeight
		BondPadChipCounter BondPadSideCounter
		CurrentOrient
		RequiredFeederWidth
		RawIO
		FixCountAddedFeederList
		NumBondPadPerDriver
		ExtraWidth
		DrawIO_LocalRawIOList
		RawIO_Save
		)

	case( Mode
		( "FixCount"
			FixCountAddedFeederList = DrawIO_ProcCalculateNewDriverPitch( )
		)
	); end case

	; Local Variables
	; ===============
	EffectiveWidth  = DrawIO_GlobalChipWidth - 2 * DrawIO_GlobalChipOffset
	EffectiveHeight = DrawIO_GlobalChipHeight - 2 * DrawIO_GlobalChipOffset

	MaxNumPadsHorizontal = EffectiveWidth / DrawIO_GlobalDriverPitch - 2.0
	MaxNumPadsVertical   = EffectiveHeight / DrawIO_GlobalDriverPitch - 2.0
	MaxNumPadsChip       = ( floor( MaxNumPadsHorizontal ) + floor( MaxNumPadsVertical )) * 2

	BondPadChipCounter = 1
	BondPadSideCounter = 1
	CurrentOrient = "horizontal"
	RequiredFeederWidth = 0

	ExtraWidth = 0.0
	DrawIO_LocalRawIOList = DrawIO_GlobalRawIOList

	RawIO_Save = nil

	; Global Variables
	; =================
	DrawIO_GlobalFullIOList = nil


	; Debugging Message
	; =================
	DebugOn && printf( "DrawIO_ProcAnalysisInput --> ----------------------------------------------------------\n")	
	DebugOn && printf( "DrawIO_ProcAnalysisInput --> Default Driver Pitch        = %n \n" DrawIO_GlobalDriverPitch )
	DebugOn && printf( "DrawIO_ProcAnalysisInput --> Max Number of Bond Pads     = %n \n" MaxNumPadsChip )
	DebugOn && printf( "DrawIO_ProcAnalysisInput --> ----------------------------------------------------------\n")	
	DebugOn && DrawIO_PrintStat( )


	while( DrawIO_LocalRawIOList != nil


		RawIO = car( DrawIO_LocalRawIOList )
		DrawIO_LocalRawIOList = cdr( DrawIO_LocalRawIOList )

		; Auto Corner Pad Insertion
		; =================================================
		if( BondPadSideCounter == 1 && ExtraWidth == 0
			then
				DrawIO_GlobalFullIOList = append( DrawIO_GlobalFullIOList list( caar( setof( x DrawIO_GlobalCornerPadList caar( x )==car( RawIO )))))
		); end if


		; Post Corner Pad Auto Feeder Insertion for Horizontal Direction
		; ==============================================================
		if( 	( CurrentOrient == "horizontal" && BondPadSideCounter == 1 && ExtraWidth == 0)
			then
				
				RequiredFeederWidth = ( MaxNumPadsHorizontal - floor( MaxNumPadsHorizontal ) + 2 ) * DrawIO_GlobalDriverPitch / 2
				DebugOn && printf( "DrawIO_ProcAnalysisInput --> Serving %L\n" RawIO )
				DebugOn && printf( "DrawIO_ProcAnalysisInput --> Added Extra Feeder %num after  Pad Number %n\n" RequiredFeederWidth BondPadChipCounter )
				DebugOn && printf( "DrawIO_ProcAnalysisInput --> =====================================================\n")

				DrawIO_GlobalFullIOList = append( DrawIO_GlobalFullIOList DrawIO_ProcInsertFeeder( car( RawIO ) RequiredFeederWidth )) 

			else

		; Post Corner Pad Auto Feeder Insertion for Vertical Direction
		; ============================================================
		if( 	( CurrentOrient == "vertical" && BondPadSideCounter == 1 && ExtraWidth == 0)
			then
				
				RequiredFeederWidth = ( MaxNumPadsVertical - floor( MaxNumPadsVertical ) + 2 ) * DrawIO_GlobalDriverPitch / 2
				DebugOn && printf( "DrawIO_ProcAnalysisInput --> Serving %L\n" RawIO )
				DebugOn && printf( "DrawIO_ProcAnalysisInput --> Added Extra Feeder %num after  Pad Number %n\n" RequiredFeederWidth BondPadChipCounter )
				DebugOn && printf( "DrawIO_ProcAnalysisInput --> =====================================================\n")
				DrawIO_GlobalFullIOList = append( DrawIO_GlobalFullIOList DrawIO_ProcInsertFeeder( car( RawIO ) RequiredFeederWidth )) 

			else
				RequiredFeederWidth = 0

		); end if 
		); end if


		; Insert Ring Breaker to break the IO Ring
		; ========================================
		if( car( RawIO ) != car( RawIO_Save ) && RawIO_Save != nil
			then
				DrawIO_GlobalFullIOList = append( DrawIO_GlobalFullIOList list( list( "RingBreaker" "F" "RB3" ))) 				
				ExtraWidth = ExtraWidth + 3
				printf( "DrawIO_ProcAnalysisInput --> Insert Ring Breaker after Pad Number %n : ExtraWidth = %3.3f\n" BondPadChipCounter ExtraWidth )
		); end if

		RawIO_Save = RawIO


		; Process the actual RawIO
		; ===============================

		case( cadr( RawIO )

			( "S"
				NumBondPadPerDriver = nth( 4 car( setof( x DrawIO_GlobalSignalDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO ))))) 
				RequiredFeederWidth = NumBondPadPerDriver * DrawIO_GlobalDriverPitch - 
					car( nth( 2 car( setof( x DrawIO_GlobalSignalDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO ))))))

				ExtraWidth = ExtraWidth - min( RequiredFeederWidth 0.0 )
				RequiredFeederWidth = max( RequiredFeederWidth 0.0 )

			)

			( "PRH"
				NumBondPadPerDriver = nth( 4 car( setof( x DrawIO_GlobalPowerRingHighDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO )))))

				RequiredFeederWidth = NumBondPadPerDriver * DrawIO_GlobalDriverPitch - 
					car( nth( 2 car( setof( x DrawIO_GlobalPowerRingHighDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO ))))))

				ExtraWidth = ExtraWidth - min( RequiredFeederWidth 0.0 )
				RequiredFeederWidth = max( RequiredFeederWidth 0.0 )
			)

			( "PRL"
				NumBondPadPerDriver = nth( 4 car( setof( x DrawIO_GlobalPowerRingLowDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO )))))

				RequiredFeederWidth = NumBondPadPerDriver * DrawIO_GlobalDriverPitch - 
					car( nth( 2 car( setof( x DrawIO_GlobalPowerRingLowDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO ))))))

				ExtraWidth = ExtraWidth - min( RequiredFeederWidth 0.0 )
				RequiredFeederWidth = max( RequiredFeederWidth 0.0 )
			)

			( "PCH"
				NumBondPadPerDriver = nth( 4 car( setof( x DrawIO_GlobalPowerCoreHighDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO )))))

				RequiredFeederWidth = NumBondPadPerDriver * DrawIO_GlobalDriverPitch - 
					car( nth( 2 car( setof( x DrawIO_GlobalPowerCoreHighDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO ))))))

				ExtraWidth = ExtraWidth - min( RequiredFeederWidth 0.0 )
				RequiredFeederWidth = max( RequiredFeederWidth 0.0 )
			)

			( "PCL"
				NumBondPadPerDriver = nth( 4 car( setof( x DrawIO_GlobalPowerCoreLowDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO )))))

				RequiredFeederWidth = NumBondPadPerDriver * DrawIO_GlobalDriverPitch - 
					car( nth( 2 car( setof( x DrawIO_GlobalPowerCoreLowDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO ))))))

				ExtraWidth = ExtraWidth - min( RequiredFeederWidth 0.0 )
				RequiredFeederWidth = max( RequiredFeederWidth 0.0 )
			)

			( "D"
				NumBondPadPerDriver = nth( 4 car( setof( x DrawIO_GlobalDummyDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO )))))
				; RequiredFeederWidth = NumBondPadPerDriver * DrawIO_GlobalDriverPitch 
				; ExtraWidth = ExtraWidth - min( RequiredFeederWidth 0.0 )
				; RequiredFeederWidth = max( RequiredFeederWidth 0.0 )

				RequiredFeederWidth = NumBondPadPerDriver * DrawIO_GlobalDriverPitch -
					car( nth( 2 car( setof( x DrawIO_GlobalDummyDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO ))))))
				ExtraWidth = ExtraWidth - min( RequiredFeederWidth 0.0 )

				RequiredFeederWidth = max( RequiredFeederWidth 0.0 ) + 
					car( nth( 2 car( setof( x DrawIO_GlobalDummyDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO ))))))

;				if(  RequiredFeederWidth > 100
;					then
;						println( ExtraWidth ) 
;				)

			)

		); end case

		DrawIO_GlobalFullIOList = append( DrawIO_GlobalFullIOList DrawIO_ProcInsertFeeder( car( RawIO ) RequiredFeederWidth / 2 )) 
		DrawIO_GlobalFullIOList = append( DrawIO_GlobalFullIOList list( RawIO ))
		DrawIO_GlobalFullIOList = append( DrawIO_GlobalFullIOList DrawIO_ProcInsertFeeder( car( RawIO ) RequiredFeederWidth / 2 )) 



		; Print Error Message for Unsupported Modes
		; =========================================
		if( ExtraWidth != 0 && Mode == "FixCount"
			then
				printf( "DrawIO_ProcAnalysisInput --> ERROR!!! \n" )
				printf( "DrawIO_ProcAnalysisInput -->       DriverWidth > DriverPitch * NumPads \n")
				printf( "DrawIO_ProcAnalysisInput -->       -- Unspported for \"FixCount\" \n")
				printf( "DrawIO_ProcAnalysisInput -->       -- Pls switch to \"Normal\" Mode \n")
		); end if
 

		; Deal with the "ExtraWidth" Buffer for oversized drivers
		; ========================================================
		if( floor( ExtraWidth ) >= DrawIO_GlobalDriverPitch
			then
				BondPadSideCounter = BondPadSideCounter + 1
				ExtraWidth = ExtraWidth - DrawIO_GlobalDriverPitch
		); end if

		if( ( BondPadSideCounter == floor( MaxNumPadsHorizontal ) - 1 || BondPadSideCounter == floor( MaxNumPadsVertical ) - 1 ) && ExtraWidth != 0
			then
				DebugOn && printf( "DrawIO_ProcAnalysisInput --> ExtraWidth = %3.3f : Added %n um feeders!\n"
						ExtraWidth 
						DrawIO_GlobalDriverPitch - ExtraWidth )
				RequiredFeederWidth = DrawIO_GlobalDriverPitch - ExtraWidth
				BondPadSideCounter = BondPadSideCounter + 1
				DrawIO_GlobalFullIOList = append( DrawIO_GlobalFullIOList DrawIO_ProcInsertFeeder( car( RawIO ) RequiredFeederWidth ))
				ExtraWidth = 0.0
		); end if



		; Pre Corner Pad Auto Feeder Insertion for Horizontal Direction
		; ===============================================================
		if( 	( CurrentOrient == "horizontal" && BondPadSideCounter == floor( MaxNumPadsHorizontal ))
			then
				
				RequiredFeederWidth = ( MaxNumPadsHorizontal - floor( MaxNumPadsHorizontal ) + 2 ) * DrawIO_GlobalDriverPitch / 2
				DebugOn && printf( "DrawIO_ProcAnalysisInput --> Serving %L\n" RawIO )
				DebugOn && printf( "DrawIO_ProcAnalysisInput --> Added Extra Feeder %num before Pad Number %n\n" RequiredFeederWidth BondPadChipCounter )
				DebugOn && printf( "DrawIO_ProcAnalysisInput --> =====================================================\n")
				DrawIO_GlobalFullIOList = append( DrawIO_GlobalFullIOList DrawIO_ProcInsertFeeder( car( RawIO ) RequiredFeederWidth )) 

			else

		; Pre Corner Pad Auto Feeder Insertion for Vertical Direction
		; ===============================================================
		if( 	( CurrentOrient == "vertical" && BondPadSideCounter == floor( MaxNumPadsVertical ))
			then
				
				RequiredFeederWidth = ( MaxNumPadsVertical - floor( MaxNumPadsVertical ) + 2 ) * DrawIO_GlobalDriverPitch / 2
				DebugOn && printf( "DrawIO_ProcAnalysisInput --> Serving %L\n" RawIO )
				DebugOn && printf( "DrawIO_ProcAnalysisInput --> Added Extra Feeder %num before Pad Number %n\n" RequiredFeederWidth BondPadChipCounter )
				DebugOn && printf( "DrawIO_ProcAnalysisInput --> =====================================================\n")
				DrawIO_GlobalFullIOList = append( DrawIO_GlobalFullIOList DrawIO_ProcInsertFeeder( car( RawIO ) RequiredFeederWidth )) 

			else
				RequiredFeederWidth = 0

		); end if 
		); end if


		; Continue to seal the IO ring with dummy pads if all signal pads are gone
		; ========================================================================
		if( 	DrawIO_LocalRawIOList == nil && 
			!( 	CurrentOrient == "vertical" 
				&& BondPadSideCounter == floor( MaxNumPadsVertical ) 
				&& BondPadChipCounter > ( floor( MaxNumPadsVertical ) + floor( MaxNumPadsHorizontal )))

			then

				; Seal the ring with 1-Driverpitch-Wide Dummy Driver
				; ==================================================
				DrawIO_LocalRawIOList = append( DrawIO_LocalRawIOList 
								list( append( 	caar( setof( x DrawIO_GlobalDummyDriverList ( caar( x )==car( RawIO ) && nth( 4 x )==1))) 
										list( nil )
										)
									)
								)

		); end if
			

		; Increment Both Pad Counters for Horizontal Direction
		; ======================================================
		if( CurrentOrient == "horizontal" && BondPadSideCounter == floor( MaxNumPadsHorizontal )
			then
				CurrentOrient = "vertical"
				BondPadSideCounter = 1
			else


		; Increment Both Pad Counters for Vertical Direction
		; ======================================================
		if( CurrentOrient == "vertical" && BondPadSideCounter == floor( MaxNumPadsVertical )
			then
				CurrentOrient = "horizontal"
				BondPadSideCounter = 1

			else
				BondPadSideCounter = BondPadSideCounter + NumBondPadPerDriver 

		); end if
		); end if

		BondPadChipCounter = BondPadChipCounter + NumBondPadPerDriver

	); end foreach

	
	; Print Error Message if there are more pads than space
	; =====================================================
	if( MaxNumPadsChip < BondPadChipCounter - 1
		then
			printf( "DrawIO_ProcAnalysisInput --> ERROR!!! \n" )
			printf( "DrawIO_ProcAnalysisInput -->       Too many pads!!! \n" ) 
			printf( "DrawIO_ProcAnalysisInput -->       -- Max Number of Pads Allowed           = %n \n" MaxNumPadsChip )
			printf( "DrawIO_ProcAnalysisInput -->       -- Current Number of Pads Instantiated  = %n \n" BondPadChipCounter - 1 )
	); end if


	t

	); end let

); end procedure 





; ========================================================================
; This procedure is used to insert feeders
; ========================================================================
procedure( DrawIO_ProcInsertFeeder( 	IOGroup 
					RequiredFeederWidth 
					@key	( DebugOn nil )
					)

	let((	AvaliableFeederWidthList 
		CurrentFeederWidth
		AvaliableFeederList
		CurrentFeeder
		NumFeederNeeded
		returnValue 
		DeltaError
		)

	; Local Variables
	; ==================

	AvaliableFeederList = setof( x DrawIO_GlobalFeederList caar( x )==IOGroup )

	AvaliableFeederWidthList = foreach( mapcar CurrentFeeder AvaliableFeederList

		car( nth( 2 CurrentFeeder ))		

	); end foreach

	AvaliableFeederWidthList = sort( AvaliableFeederWidthList `greaterp )

	NumFeederNeeded = 0

	returnValue = nil

	DeltaError = 0.00001

	; Debugging Messages
	; ==================

	DebugOn && printf( "DrawIO_ProcInsertFeeder --> IOGroup = \"%s\" AvaliableFeederWidthList = %L\n" IOGroup AvaliableFeederWidthList )


	foreach( CurrentFeederWidth AvaliableFeederWidthList

		RequiredFeederWidth = float( round( RequiredFeederWidth / DeltaError ) * DeltaError)

		NumFeederNeeded = floor( RequiredFeederWidth / float( CurrentFeederWidth ) )

		RequiredFeederWidth = RequiredFeederWidth - NumFeederNeeded * CurrentFeederWidth

		CurrentFeeder = car( setof( x AvaliableFeederList ( caar( x )==IOGroup && car( nth( 2 x ))==CurrentFeederWidth )))

		DebugOn && printf( "DrawIO_ProcInsertFeeder --> Add %n Feeder \"%s\"\n" NumFeederNeeded caddar(CurrentFeeder))

		for( i 0 NumFeederNeeded-1 

			returnValue = cons( car(CurrentFeeder) returnValue )

		); end for

	); end foreach

	DebugOn && printf( "DrawIO_ProcInsertFeeder --> Procedure Done!!! \n" )

	returnValue

	); end let

); end procedure




; ========================================================================
; This procedure is used to calculate new driver pitch
; ========================================================================
procedure( DrawIO_ProcCalculateNewDriverPitch( @key
					( DebugOn	t )
					)

	let((	MaxNumPadsHorizontal MaxNumPadsVertical MaxNumPadsChip
		EffectiveWidth EffectiveHeight
		Resolution
		NumFeederAdded
		CurrentNumPadsChip NumBondPadPerDriver
		RawIO
		)

	; Local Variables
	Resolution = 0.001
	CurrentNumPadsChip = 0

	EffectiveWidth  = DrawIO_GlobalChipWidth - 2 * DrawIO_GlobalChipOffset
	EffectiveHeight = DrawIO_GlobalChipHeight - 2 * DrawIO_GlobalChipOffset

	; Get a first estimate on the new "DrawIO_GlobalDriverPitch"
	; This estimate is ALWAYS bigger than the actual value ("ceiling") 
	; ===================================================================
	DrawIO_GlobalDriverPitch = ( 2 * ( EffectiveWidth + EffectiveHeight )) / ( length( DrawIO_GlobalRawIOList ) + 8.0 )
	DrawIO_GlobalDriverPitch = ceiling( DrawIO_GlobalDriverPitch / Resolution ) * Resolution

	MaxNumPadsHorizontal = EffectiveWidth / DrawIO_GlobalDriverPitch - 2.0
	MaxNumPadsVertical   = EffectiveHeight / DrawIO_GlobalDriverPitch - 2.0
	MaxNumPadsChip       = ( floor( MaxNumPadsHorizontal ) + floor( MaxNumPadsVertical )) * 2


	; Debugging Massages
	; ===================
	DebugOn && printf( "========================================================================================================\n" ) 
	DebugOn && printf( "DrawIO_ProcCalculateNewDriverPitch --> First Estimate \"DrawIO_GlobalDriverPitch\" = %n um\n" DrawIO_GlobalDriverPitch ) 

	foreach( RawIO DrawIO_GlobalRawIOList

		case( cadr( RawIO )

			( "S"
				NumBondPadPerDriver = nth( 4 car( setof( x DrawIO_GlobalSignalDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO ))))) 
			)

			( "PRH"
				NumBondPadPerDriver = nth( 4 car( setof( x DrawIO_GlobalPowerRingHighDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO )))))
			)

			( "PRL"
				NumBondPadPerDriver = nth( 4 car( setof( x DrawIO_GlobalPowerRingLowDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO )))))
			)

			( "PCH"
				NumBondPadPerDriver = nth( 4 car( setof( x DrawIO_GlobalPowerCoreHighDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO )))))
			)

			( "PCL"
				NumBondPadPerDriver = nth( 4 car( setof( x DrawIO_GlobalPowerCoreLowDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO )))))
			)

			( "D"
				NumBondPadPerDriver = nth( 4 car( setof( x DrawIO_GlobalDummyDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO )))))
			)

		); end case

		; Calculate the total number of pads the chip has currently
		; =========================================================
		CurrentNumPadsChip = CurrentNumPadsChip + NumBondPadPerDriver		

	); end foreach


	; Increase Feeder width by "Resolution" um at a time
	; Once feeder width increases, recalcuate the "MaxNumPadsChip" the chip can now hold
	; continue the process only if "MaxNumPadsChip"
	; =================================================================================================== 
	while( MaxNumPadsChip < CurrentNumPadsChip

		MaxNumPadsHorizontal = EffectiveWidth / DrawIO_GlobalDriverPitch - 2.0
		MaxNumPadsVertical   = EffectiveHeight / DrawIO_GlobalDriverPitch - 2.0
		MaxNumPadsChip       = ( floor( MaxNumPadsHorizontal ) + floor( MaxNumPadsVertical )) * 2

		DrawIO_GlobalDriverPitch = DrawIO_GlobalDriverPitch - Resolution

	); end while



	; Debugging Messages
	; ===================
	DebugOn && printf( "DrawIO_ProcCalculateNewDriverPitch --> Final \"DrawIO_GlobalDriverPitch\" = %n um\n" DrawIO_GlobalDriverPitch ) 
	DebugOn && printf( "========================================================================================================\n" ) 


	; NumFeederAdded = number of 1-driverpitch-wide dummy drivers needed to be added
	; ==============================================================================
	NumFeederAdded = MaxNumPadsChip - CurrentNumPadsChip

	for( i 1 NumFeederAdded

		DrawIO_GlobalRawIOList = append( DrawIO_GlobalRawIOList 
						list( caar( 
							setof( 	x 
								DrawIO_GlobalDummyDriverList 
								( caar( x )==car( nth( length( DrawIO_GlobalRawIOList )-1 DrawIO_GlobalRawIOList )) && nth( 4 x )==1))))
						)




	); end for

	t

	); end let

); end procedure



; ========================================================================
; This procedure is used to print stat from DrawIO_GlobalRawIOList
; ========================================================================
procedure( DrawIO_PrintStat( @key
					( DebugOn	t )
					)

	let((	RawIO
		NumBondPadPerDriver

		CounterDriverSignalSinglePad
		CounterDriverSignalDoublePad
		CounterDriverSignalTriplePad
		CounterDriverPRHSinglePad
		CounterDriverPRHDoublePad
		CounterDriverPRLSinglePad
		CounterDriverPCHSinglePad
		CounterDriverPCLSinglePad
		CounterDriverDummySinglePad
		CounterDriver
		CounterBondPad
		)


	; Local Variables
	; ===============
	CounterDriverSignalSinglePad = 0
	CounterDriverSignalDoublePad = 0
	CounterDriverSignalTriplePad = 0
	CounterDriverPRHSinglePad = 0
	CounterDriverPRHDoublePad = 0
	CounterDriverPRLSinglePad = 0
	CounterDriverPCHSinglePad = 0
	CounterDriverPCLSinglePad = 0
	CounterDriverDummySinglePad = 0
	CounterDriver = 0
	CounterBondPad = 0


	foreach( RawIO DrawIO_GlobalRawIOList

		case( cadr( RawIO )

			( "S"
				NumBondPadPerDriver = nth( 4 car( setof( x DrawIO_GlobalSignalDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO )))))
				case( NumBondPadPerDriver
					( 1
						CounterDriverSignalSinglePad = CounterDriverSignalSinglePad + 1
					)
					( 2
						CounterDriverSignalDoublePad = CounterDriverSignalDoublePad + 1
					)
					( 3
						CounterDriverSignalTriplePad = CounterDriverSignalTriplePad + 1
					)
				); end case
						  
			)

			( "PRH"
				NumBondPadPerDriver = nth( 4 car( setof( x DrawIO_GlobalPowerRingHighDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO )))))
				case( NumBondPadPerDriver
					( 1
						CounterDriverPRHSinglePad = CounterDriverPRHSinglePad + 1
					)
					( 2
						CounterDriverPRHDoublePad = CounterDriverPRHDoublePad + 1
					)
				); end case
			)

			( "PRL"
				NumBondPadPerDriver = nth( 4 car( setof( x DrawIO_GlobalPowerRingLowDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO )))))
				case( NumBondPadPerDriver
					( 1
						CounterDriverPRLSinglePad = CounterDriverPRLSinglePad + 1
					)
				); end case
			)

			( "PCH"
				NumBondPadPerDriver = nth( 4 car( setof( x DrawIO_GlobalPowerCoreHighDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO )))))
				case( NumBondPadPerDriver
					( 1
						CounterDriverPCHSinglePad = CounterDriverPCHSinglePad + 1
					)
				); end case
			)

			( "PCL"
				NumBondPadPerDriver = nth( 4 car( setof( x DrawIO_GlobalPowerCoreLowDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO )))))
				case( NumBondPadPerDriver
					( 1
						CounterDriverPCLSinglePad = CounterDriverPCLSinglePad + 1
					)
				); end case
			)

			( "D"
				NumBondPadPerDriver = nth( 4 car( setof( x DrawIO_GlobalDummyDriverList ( caar( x )==car( RawIO ) && caddar( x )==caddr( RawIO )))))
				case( NumBondPadPerDriver
					( 1
						CounterDriverDummySinglePad = CounterDriverDummySinglePad + 1
					)
				); end case
			)

			(t
				DebugOn && printf( "DrawIO_PrintStat --> UnSupported Input %s!!! \n" cadr( RawIO ))
			)

		); end case

		CounterDriver = CounterDriver + 1
		CounterBondPad = CounterBondPad + NumBondPadPerDriver

	); end foreach


	printf( "DrawIO_PrintStat -->                              |   NumDriver   |   NumBondPad  |\n")
	printf( "DrawIO_PrintStat --> --------------------------------------------------------------\n")
	printf( "DrawIO_PrintStat --> Signal Drivers (Single-Paded)|      %3n      |      %3n      |\n" 
		CounterDriverSignalSinglePad 
		CounterDriverSignalSinglePad
		)
	printf( "DrawIO_PrintStat --> Signal Drivers (Double-Paded)|      %3n      |      %3n      |\n" 
		CounterDriverSignalDoublePad 
		CounterDriverSignalDoublePad * 2
		)
	printf( "DrawIO_PrintStat --> Signal Drivers (Triple-Paded)|      %3n      |      %3n      |\n" 
		CounterDriverSignalTriplePad 
		CounterDriverSignalTriplePad * 3
		)
	printf( "DrawIO_PrintStat --> IOVDD Drivers (Single-Paded) |      %3n      |      %3n      |\n" 
		CounterDriverPRHSinglePad 
		CounterDriverPRHSinglePad
		)
	printf( "DrawIO_PrintStat --> IOVDD Drivers (Double-Paded) |      %3n      |      %3n      |\n" 
		CounterDriverPRHDoublePad 
		CounterDriverPRHDoublePad * 2
		)
	printf( "DrawIO_PrintStat --> IOVSS Drivers (Single-Paded) |      %3n      |      %3n      |\n" 
		CounterDriverPRLSinglePad 
		CounterDriverPRLSinglePad
		)
	printf( "DrawIO_PrintStat --> CoreVDD Drivers(Single-Paded)|      %3n      |      %3n      |\n" 
		CounterDriverPCHSinglePad 
		CounterDriverPCHSinglePad
		)
	printf( "DrawIO_PrintStat --> CoreVSS Drivers(Single-Paded)|      %3n      |      %3n      |\n" 
		CounterDriverPCLSinglePad 
		CounterDriverPCLSinglePad
		)
	printf( "DrawIO_PrintStat --> Dummy Drivers (Single-Paded) |      %3n      |      %3n      |\n" 
		CounterDriverDummySinglePad 
		CounterDriverDummySinglePad
		)
	printf( "DrawIO_PrintStat --> --------------------------------------------------------------\n")
	printf( "DrawIO_PrintStat --> Total                        |      %3n      |      %3n      |\n" 
		CounterDriver
		CounterBondPad
		)
	printf( "DrawIO_PrintStat --> --------------------------------------------------------------\n")


	); end let

	t

); end procedure













; ========================================================================
; This procedure is used to draw the actial padframe
; ========================================================================
procedure( DrawIO_ProcDrawIO(	@key	( DebugOn nil )
				)

	let((	CurrentDriverX	CurrentDriverY
		CounterAllDriver
		CounterBondPad
		CounterSignalDriver
		CounterPowerRingHighDriver
		CounterPowerRingLowDriver
		CounterPowerCoreHighDriver
		CounterPowerCoreLowDriver
		CounterFeeder
		CounterCornerPad

		FullIO	ActualIO
		BondPad	BondPad_origin BondPadOld BondPad_instName
		TEXT_origin TEXT_orient TEXT_just TEXT_size TEXT_gap
		PackageIO_origin
		d_cellView
		t_libName
		t_cellName
		t_viewName
		t_instName
		l_origin
		t_orient

		NumBondPadPerDriver
		NextBondPadOffset

		NetID	
		NetParent
		NetChild

		Filename_Package
		Filename_CDL
		Filename_CDL_tmp
		Filename_Cast
		Filename_Cast_tmp
		Filename_Spec
		Filename_Spec_tmp
		Filename_Binding_Rule
		
		outPort_Package
		outPort_CDL
		outPort_CDL_tmp
		outPort_Cast
		outPort_Cast_tmp
		outPort_Spec
		outPort_Spec_tmp
		outPort_Binding_Rule

		l_IOPort IOPort
		l_IOType IOType
		ExceptList

		PadNameList
		PadName

		CastCellName_S
		CastCellName_L

		)

	; Local Variables
	; ====================
	CurrentDriverX = 0
	CurrentDriverY = 0

	CounterAllDriver = 0
	CounterBondPad = 0
	CounterSignalDriver = 0
	CounterPowerRingHighDriver = 0
	CounterPowerRingLowDriver  = 0
	CounterPowerCoreHighDriver = 0
	CounterPowerCoreLowDriver  = 0
	CounterFeeder = 0
	CounterCornerPad = 0


	BondPad_origin   = list( 0.0 0.0 )
	PackageIO_origin = list( 0.0 0.0 )

	TEXT_size = 35
	TEXT_gap = 100

	t_orient   = "R0" 

	Filename_Package  = "./PADFRAME.package"
	Filename_CDL      = "./PADFRAME.cdl"
	Filename_CDL_tmp  = "./PADFRAME.cdl.tmp"
	Filename_Cast     = "./PADFRAME.cast"
	Filename_Cast_tmp = "./PADFRAME.cast.tmp"
	Filename_Spec     = "./PADFRAME.spec"
	Filename_Spec_tmp = "./PADFRAME.spec.tmp"
	Filename_Binding_Rule = "./PADFRAME.bind.rul"

	outPort_Package  = outfile( Filename_Package )
	outPort_CDL      = outfile( Filename_CDL )
	outPort_CDL_tmp  = outfile( Filename_CDL_tmp )
	outPort_Cast     = outfile( Filename_Cast )
	outPort_Cast_tmp = outfile( Filename_Cast_tmp )
	outPort_Spec     = outfile( Filename_Spec )
	outPort_Spec_tmp = outfile( Filename_Spec_tmp )
	outPort_Binding_Rule = outfile( Filename_Binding_Rule )

	l_IOPort = nil
	l_IOType = nil

	ExceptList = nil

	PadNameList = nil
	PadName     = nil

	CastCellName_S = nil
	CastCellName_L = nil



	; Global Variables
	; ====================
	; DrawIO_GlobalFullIOList
	

	; Actual Processing
	; ======================

	foreach( FullIO DrawIO_GlobalFullIOList 

		; Table Lookup for the Actual IO Cell
		; ===================================
		case( cadr( FullIO )

			( "S"
				ActualIO = car( setof( x DrawIO_GlobalSignalDriverList ( caar( x )==car( FullIO ) && caddar( x )==caddr( FullIO ))))
				NumBondPadPerDriver = nth( 4 ActualIO )
				CounterSignalDriver = CounterSignalDriver + 1
				CounterAllDriver = CounterAllDriver + 1
;				t_instName = sprintf( nil "S%n" CounterSignalDriver )
;				t_instName = sprintf( nil "S%n" CounterBondPad + 1 )
				t_instName = sprintf( nil "zzzS%n" CounterBondPad + 1 )
			)

			( "PRH"
				ActualIO = car( setof( x DrawIO_GlobalPowerRingHighDriverList ( caar( x )==car( FullIO ) && caddar( x )==caddr( FullIO ))))
				NumBondPadPerDriver = nth( 4 ActualIO )
				CounterPowerRingHighDriver = CounterPowerRingHighDriver + 1
				CounterAllDriver = CounterAllDriver + 1
;				t_instName = sprintf( nil "PRH%n" CounterPowerRingHighDriver )
;				t_instName = sprintf( nil "S%n" CounterBondPad + 1 )
				t_instName = sprintf( nil "zzzS%n" CounterBondPad + 1 )
			)

			( "PRL"
				ActualIO = car( setof( x DrawIO_GlobalPowerRingLowDriverList ( caar( x )==car( FullIO ) && caddar( x )==caddr( FullIO ))))
				NumBondPadPerDriver = nth( 4 ActualIO )
				CounterPowerRingLowDriver = CounterPowerRingLowDriver + 1
				CounterAllDriver = CounterAllDriver + 1
;				t_instName = sprintf( nil "PRL%n" CounterPowerRingLowDriver )
;				t_instName = sprintf( nil "S%n" CounterBondPad + 1 )
				t_instName = sprintf( nil "zzzS%n" CounterBondPad + 1 )
			)

			( "PCH"
				ActualIO = car( setof( x DrawIO_GlobalPowerCoreHighDriverList ( caar( x )==car( FullIO ) && caddar( x )==caddr( FullIO ))))
				NumBondPadPerDriver = nth( 4 ActualIO )
				CounterPowerCoreHighDriver = CounterPowerCoreHighDriver + 1
				CounterAllDriver = CounterAllDriver + 1
;				t_instName = sprintf( nil "PCH%n" CounterPowerCoreHighDriver )
;				t_instName = sprintf( nil "S%n" CounterBondPad + 1 )
				t_instName = sprintf( nil "zzzS%n" CounterBondPad + 1 )
			)

			( "PCL"
				ActualIO = car( setof( x DrawIO_GlobalPowerCoreLowDriverList ( caar( x )==car( FullIO ) && caddar( x )==caddr( FullIO ))))
				NumBondPadPerDriver = nth( 4 ActualIO )
				CounterPowerCoreLowDriver = CounterPowerCoreLowDriver + 1
				CounterAllDriver = CounterAllDriver + 1
;				t_instName = sprintf( nil "PCL%n" CounterPowerCoreLowDriver )
;				t_instName = sprintf( nil "S%n" CounterBondPad + 1 )
				t_instName = sprintf( nil "zzzS%n" CounterBondPad + 1 )
			)

			( "C"
				ActualIO = car( setof( x DrawIO_GlobalCornerPadList ( caar( x )==car( FullIO ) && caddar( x )==caddr( FullIO ))))
				NumBondPadPerDriver = nth( 4 ActualIO )
				CounterCornerPad = CounterCornerPad + 1
				t_instName = sprintf( nil "C%n" CounterCornerPad )
			)

			( "F"
				ActualIO = car( setof( x DrawIO_GlobalFeederList ( caar( x )==car( FullIO ) && caddar( x )==caddr( FullIO ))))
				NumBondPadPerDriver = nth( 4 ActualIO )
				CounterFeeder = CounterFeeder + 1
				t_instName = sprintf( nil "F%n" CounterFeeder )
			)

			( "D"

				ActualIO = car( setof( x DrawIO_GlobalDummyDriverList 
						( 	caar( x )==car( FullIO ) &&
							cadar( x )==cadr( FullIO ) && 
							caddar( x )==caddr( FullIO )
							)))
				NumBondPadPerDriver = nth( 4 ActualIO )
				CounterAllDriver = CounterAllDriver + 1
			)

		); end case

		d_cellView = geGetWindowCellView( )
		t_libName  = car( nth( 1 ActualIO ))
		t_cellName = cadr( nth( 1 ActualIO ))
		t_viewName = caddr( nth( 1 ActualIO ))



		; Table Lookup for the appropiate Bonding Cell
		; ==============================================
		if( member( cadr( FullIO ) list( "S" "PRH" "PRL" "PCH" "PCL" ))
			then
				BondPad = nth(	mod( CounterBondPad 2 )
						setof( x DrawIO_GlobalBondPadList ( caar( x )==car( FullIO ) ))
						)
			else
				BondPad = nil
		); end if



		; Rotate 90 degree counter-clockwise if not the first corner pad
		; ==================================================================
		if( cadr( FullIO ) == "C" && CounterBondPad != 0
			then
				case( t_orient
					
					( "R0"
						t_orient   = "R90" 
						CurrentDriverX = CurrentDriverX + cadr( nth( 2 ActualIO ))
					)

					( "R90"
						t_orient   = "R180" 
						CurrentDriverY = CurrentDriverY + cadr( nth( 2 ActualIO ))
					)

					( "R180"
						t_orient   = "R270" 
						CurrentDriverX = CurrentDriverX - cadr( nth( 2 ActualIO ))
					)


					( "R270"
						t_orient   = "R0" 
					)

				); end case

		); end if


		; Calculate the origin of the current IO
		; =================================================================
		; Increment the x/y coordinates for the next IO
		; =================================================================

		if( member( cadr( FullIO ) list( "S" "PRH" "PRL" "PCH" "PCL" "F" "C" ))
		then

		case( t_orient

			( "R0"
				l_origin   = list( 	CurrentDriverX - car( nth( 3 ActualIO )) 
							CurrentDriverY - cadr( nth( 3 ActualIO )) 
							)

				TEXT_origin = list( CurrentDriverX CurrentDriverY - TEXT_gap )
				TEXT_orient = "R90"
				TEXT_just = "upperLeft"

				if( BondPad
					then
						BondPad_origin = list( 	CurrentDriverX - car( nth( 3 BondPad )) + car( nth( 5 ActualIO )) 
									CurrentDriverY - cadr( nth( 3 BondPad )) 
									)
				); end if

				if( caddar( BondPad ) == "BI"
					then
						PackageIO_origin = list( car( BondPad_origin ) + 17.5 cadr( BondPad_origin ) + 104.5 )
					else
						PackageIO_origin = list( car( BondPad_origin ) + 17.5 cadr( BondPad_origin ) + 27.5 )
				); end if

 				CurrentDriverX = CurrentDriverX + car( nth( 2 ActualIO ))
			)

			( "R90"
				l_origin   = list( 	CurrentDriverX + cadr( nth( 3 ActualIO )) 
							CurrentDriverY - car( nth( 3 ActualIO )) 
							)

				TEXT_origin = list( CurrentDriverX + TEXT_gap CurrentDriverY + 20 )
				TEXT_orient = "R180"
				TEXT_just = "lowerLeft"

				if( BondPad
					then
						BondPad_origin = list( 	CurrentDriverX + cadr( nth( 3 BondPad )) 
									CurrentDriverY - car( nth( 3 BondPad )) + car( nth( 5 ActualIO ))
									)
				); end if

				if( caddar( BondPad ) == "BI"
					then
						PackageIO_origin = list( car( BondPad_origin ) - 104.5 cadr( BondPad_origin ) + 17.5 )
					else
						PackageIO_origin = list( car( BondPad_origin ) - 27.5 cadr( BondPad_origin ) + 17.5 )
				); end if

				CurrentDriverY = CurrentDriverY + car( nth( 2 ActualIO ))
			)
	
			( "R180"
				l_origin   = list( 	CurrentDriverX + car( nth( 3 ActualIO )) 
							CurrentDriverY + cadr( nth( 3 ActualIO )) 
							)

				TEXT_origin = list( CurrentDriverX - 20  CurrentDriverY + TEXT_gap )
				TEXT_orient = "R270"
				TEXT_just = "lowerLeft"

				if( BondPad
					then
						BondPad_origin = list( 	CurrentDriverX + car( nth( 3 BondPad )) - car( nth( 5 ActualIO ))
									CurrentDriverY + cadr( nth( 3 BondPad )) 
									)
				); end if

				if( caddar( BondPad ) == "BI"
					then
						PackageIO_origin = list( car( BondPad_origin ) - 17.5 cadr( BondPad_origin ) - 104.5 )
					else
						PackageIO_origin = list( car( BondPad_origin ) - 17.5 cadr( BondPad_origin ) - 27.5 )
				); end if


				CurrentDriverX = CurrentDriverX - car( nth( 2 ActualIO )) 
			)

			( "R270"
				l_origin   = list( 	CurrentDriverX - cadr( nth( 3 ActualIO )) 
							CurrentDriverY + car( nth( 3 ActualIO )) 
							)

				TEXT_origin = list( CurrentDriverX - TEXT_gap CurrentDriverY )
				TEXT_orient = "R0"
				TEXT_just = "upperLeft"

				if( BondPad
					then
						BondPad_origin = list( 	CurrentDriverX - cadr( nth( 3 BondPad )) 
									CurrentDriverY + car( nth( 3 BondPad )) - car( nth( 5 ActualIO )) 
									)
				); end if

				if( caddar( BondPad ) == "BI"
					then
						PackageIO_origin = list( car( BondPad_origin ) + 104.5 cadr( BondPad_origin ) - 17.5 )
					else
						PackageIO_origin = list( car( BondPad_origin ) + 27.5 cadr( BondPad_origin ) - 17.5 )
				); end if

				CurrentDriverY = CurrentDriverY - car( nth( 2 ActualIO ))
			)
	
		); end case

		);end if

		; Perform Actual Instantantion (Driver/Feeder)
		; ============================================
		if( member( cadr( FullIO ) list( "S" "PRH" "PRL" "PCH" "PCL" "F" "C" ))
			then
				d_inst = dbCreateInstByMasterName(
						d_cellView
						t_libName
						t_cellName
						t_viewName
						t_instName
						l_origin
						t_orient
						)
		); end if



		; Add the IOType into the List if it's not already there
		;    -- used mainly for Cast Generation
		; ======================================================
		if( member( cadr( FullIO ) list( "S" "PRH" "PRL" "PCH" "PCL" ))
			then
				IOType = cadr( cadr( ActualIO ))
				rexCompile( "\\.0" )
				IOType = rexReplace( IOType "" 0 )
				rexCompile( "\\.1" )
				IOType = rexReplace( IOType "" 0 )

				if( !member( IOType l_IOType )
					then
						DebugOn && printf( "DrawIO_ProcDrawIO --> Found New Cadence Cellname \"%s\"\n" IOType )
						l_IOType = cons( IOType l_IOType )
				); end if
		); end if


		; Create Connectivities (Fill up the "Propagate Net" Box)
		; ==========================================================
		for( i 0 length( nth( 3 FullIO ))-1

			NetChild = cadr( nth( i nth( 3 FullIO )))

;			NetChild = nth( i nth( 3 FullIO ))


			NetParent = nth( i nth( 6 ActualIO ))

			; In case the NetElement is just a simple string
			; Position Mapping
			; ===============================================
			if( stringp( NetChild ) && stringp( NetParent )
				then
					NetID = dbMakeNet( geGetWindowCellView() NetChild )
					dbCreateConnByName( NetID d_inst NetParent )
					if( !member( NetChild l_IOPort )
						then
							l_IOPort = cons( NetChild l_IOPort )
							fprintf( outPort_Binding_Rule "changeLabel( \"%s\" \"%s.%s\" ) \n" 
									NetChild 
									t_instName 
									NetParent
									)
					); end if
			); end if 
;Samson

		); end for

		; Draw inplace pins and labels for this driver
		; ==============================================

		DrawIO_ProcDrawInPlacePinsAndLabels( d_inst )


		; Instantantion of Bond Pad if necessary
		; =======================================
		for( i 1 NumBondPadPerDriver

			CounterBondPad = CounterBondPad + 1

			; Make sure that a Bond Pad is needed for this driver
			; ===================================================
			if( member( cadr( FullIO ) list( "S" "PRH" "PRL" "PCH" "PCL" ))
			then
				BondPad_instName = sprintf( nil "B%n" CounterBondPad )
				t_libName  = car( nth( 1 BondPad ))
				t_cellName = cadr( nth( 1 BondPad ))
				t_viewName = caddr( nth( 1 BondPad ))

				d_inst = dbCreateInstByMasterName(
						d_cellView
						t_libName
						t_cellName
						t_viewName
						BondPad_instName
						BondPad_origin
						t_orient
						)

				dbCreateLabel(	d_cellView
						list( "TEXT" "drawing" )
						TEXT_origin
						sprintf( nil "%n" CounterBondPad )
						TEXT_just
						TEXT_orient 
						"roman" 
						TEXT_size
						)



			; Dealing with the next BondPad for the current Driver 
			; Currently, only LVDS Drivers will have TWO BondPads
			; ======================================================================

			BondPadOld = BondPad
			PackageIO_originOld = PackageIO_origin

			BondPad = nth(	mod( CounterBondPad 2 )
					setof( x DrawIO_GlobalBondPadList ( caar( x )==car( FullIO ) ))
					)


			if( nth( i nth( 5 ActualIO )) == nil
				then
					NextBondPadOffset = 0
				else
					NextBondPadOffset = nth( i nth( 5 ActualIO ))
			); end if

			case( t_orient

				( "R0"
					BondPad_origin = list( 	car( BondPad_origin ) + NextBondPadOffset - car( nth( 3 BondPad )) + car( nth( 3 BondPadOld ))
								cadr( BondPad_origin ) - cadr( nth( 3 BondPad )) + cadr( nth( 3 BondPadOld ))
								)
					TEXT_origin = list( car( TEXT_origin ) + NextBondPadOffset cadr( TEXT_origin ))

					if( caddar( BondPad ) == "BI"
						then
							PackageIO_origin = list( car( BondPad_origin ) + 17.5 cadr( BondPad_origin ) + 104.5 )
						else
							PackageIO_origin = list( car( BondPad_origin ) + 17.5 cadr( BondPad_origin ) + 27.5 )
					); end if

				)
	
				( "R90"
					BondPad_origin = list( 	car( BondPad_origin ) + cadr( nth( 3 BondPad )) - cadr( nth( 3 BondPadOld ))
								cadr( BondPad_origin ) + NextBondPadOffset - car( nth( 3 BondPad )) + car( nth( 3 BondPadOld )) 
								)
					TEXT_origin = list( car( TEXT_origin ) cadr( TEXT_origin ) + NextBondPadOffset )

					if( caddar( BondPad ) == "BI"
						then
							PackageIO_origin = list( car( BondPad_origin ) - 104.5 cadr( BondPad_origin ) + 17.5 )
						else
							PackageIO_origin = list( car( BondPad_origin ) - 27.5 cadr( BondPad_origin ) + 17.5 )
					); end if

				)
		
				( "R180"
					BondPad_origin = list( 	car( BondPad_origin ) - NextBondPadOffset + car( nth( 3 BondPad )) - car( nth( 3 BondPadOld )) 
								cadr( BondPad_origin ) + cadr( nth( 3 BondPad )) - cadr( nth( 3 BondPadOld ))
								)
					TEXT_origin = list( car( TEXT_origin ) - NextBondPadOffset cadr( TEXT_origin ))

					if( caddar( BondPad ) == "BI"
						then
							PackageIO_origin = list( car( BondPad_origin ) - 17.5 cadr( BondPad_origin ) - 104.5 )
						else
							PackageIO_origin = list( car( BondPad_origin ) - 17.5 cadr( BondPad_origin ) - 27.5 )
					); end if

				)
	
				( "R270"
					BondPad_origin = list( 	car( BondPad_origin ) - cadr( nth( 3 BondPad )) + cadr( nth( 3 BondPadOld ))
								cadr( BondPad_origin ) - NextBondPadOffset + car( nth( 3 BondPad )) - car( nth( 3 BondPadOld )) 
								)
					TEXT_origin = list( car( TEXT_origin ) cadr( TEXT_origin ) - NextBondPadOffset )

					if( caddar( BondPad ) == "BI"
						then
							PackageIO_origin = list( car( BondPad_origin ) + 104.5 cadr( BondPad_origin ) - 17.5 )
						else
							PackageIO_origin = list( car( BondPad_origin ) + 27.5 cadr( BondPad_origin ) - 17.5 )
					); end if

				)

			); end case

			); end if



			; ================================
			; Output Packaging File
			; ================================
			GroupName = car( car( ActualIO ))
			GroupType = caddr( car( ActualIO ))

			; Hash the Symbol Name to get the original cellname
			; =================================================

			case( GroupName 

				( "LVDS"
					case( GroupType
						
						( "IN"
							GroupType = "pnl_sp_se_in"
						)

						( "OUT_VC"
							GroupType = "pnl_lvds85_out_vc"
						)

						( "OUT_VO"
							GroupType = "pnl_lvds85_out_vo"
						)

						( "OUT_VOP"
							GroupType = "pnl_lvds85_out_vop"
						)

						( "OUT_GC"
							GroupType = "pnl_lvds85_out_gc"
						)

						( "OUT_GO"
							GroupType = "pnl_lvds85_out_go"
						)

						( "OUT_VREF"
							GroupType = "pnl_lvds85_out_vref"
						)

						( "VREF"
							GroupType = "pnl_vref_lvds"
						)

						( "VP"
							GroupType = "pnl_vp_lvds"
						)

						( "VO"
							GroupType = "pnl_vo_lvds"
						)

						( "VOP"
							GroupType = "pnl_vop_lvds"
						)

						( "GO"
							GroupType = "pnl_go_lvds"
						)

						( "VC"
							GroupType = "pnl_vc_lvds"
						)

						( "GCS"
							GroupType = "pnl_gcs_lvds"
						)

						( "D1"
							GroupType = "Dummy(noBondPad)"
						)

					); end case

				)

				( "LVTTL"

					case( GroupType
						
						( "INnn2"
							GroupType = "pnl_it2nn2"
						)

						( "INpd2"
							GroupType = "pnl_it2pd2"
						)

						( "INpd2_2"
							GroupType = "pnl_it2pd2(withPI)"
						)

						( "INpu2"
							GroupType = "pnl_it2pu2"
						)

						( "INnn8"
							GroupType = "pnl_it2nn8"
						)

						( "INpd8"
							GroupType = "pnl_it2pd8"
						)

						( "INOUT2nn2"
							GroupType = "pnl_tf02it0nn2"
						)

						( "INOUT4nn2"
							GroupType = "pnl_tf04it0nn2"
						)

						( "INOUT8nn2"
							GroupType = "pnl_tf08it0nn2"
						)

						( "AIO"
							GroupType = "pnl_aio"
						)

						( "VP"
							GroupType = "pnl_vp"
						)

						( "VOO"
							GroupType = "pnl_voo"
						)

						( "VOP"
							GroupType = "pnl_vop"
						)

						( "VOP_2"
							GroupType = "pnl_vop"
						)

						( "VP_2"
							GroupType = "pnl_vp"
						)

						( "GO"
							GroupType = "pnl_go"
						)

						( "GO_2"
							GroupType = "pnl_go"
						)

						( "VC"
							GroupType = "pnl_vc"
						)

						( "GCS"
							GroupType = "pnl_gcs"
						)

						( "D1"
							GroupType = "Dummy(noBondPad)"
						)

					); end case


				)

			); end case






;			println( GroupName )
;			println( GroupType )
;			println( ActualIO )
;			println( NumBondPadPerDriver )

			case( NumBondPadPerDriver

				( 1
					rexCompile( "^pad_" )
					PadNameList    = setof( x nth( 3 FullIO ) rexExecute( car( x )))
					PadName        = cadr( car( PadNameList ))
					PadPackageName = caddr( car( PadNameList ))

					; Error Checking
					; ==============
					if( length( PadNameList ) != 1 && cadr( FullIO ) != "D"
						then
							printf( "ERROR!!! Number of Bondpad must be 1 for single-paded drivers\n" ) 
							printf( "ERROR!!! Trying to search a padname prefixed \"pad_\" in the List %L \n" nth( 3 FullIO ))
							printf( "ERROR!!! Search failed and return %L \n" PadNameList )
							printf( "========================================================================= \n" )
					); end if 

				)

				( 2
					rexCompile( nth( i-1 list( "^padp_" "^padn_" )))
					PadNameList    = setof( x nth( 3 FullIO ) rexExecute( car( x )))
					PadName        = cadr( car( PadNameList ))
					PadPackageName = caddr( car( PadNameList ))

				)

				( 3
					rexCompile( nth( i-1 list( "^padp_" "^pad_" "^padn_" )))
					PadNameList = setof( x nth( 3 FullIO ) rexExecute( car( x )))
					PadName        = cadr( car( PadNameList ))
					PadPackageName = caddr( car( PadNameList ))

				)

			); end case


			; Deals with Dummy Pads "D"
			; =========================
			if( PadNameList == nil
				then
					PadName        = "No_Pad_Name"
					PadPackageName = "No_Package_Name"
				else

					; Deals with Printout for Inner/Outer Bondpads
					; ============================================
					if( caddar( BondPadOld ) == "BI"
						then
							BondPadDirection = "Inner"
						else
							BondPadDirection = "Outer"
					);end if
			); end if


;			println( PadNameList )
;			println( PadName )
;			println( "++++++++++++++++++++++" )

			
			if( PadName != "No_Pad_Name"
				then
					fprintf( outPort_Package "%-4n\t%3s\t%5s\t%22s\t%-35s\t%-35s\t%-8.3n\t%-8.3n\t%-8s\n"
						CounterBondPad
						t_orient
						GroupName
						GroupType
						PadName
						PadPackageName
						car( PackageIO_originOld )
						cadr( PackageIO_originOld )
						BondPadDirection
						)
				else
					fprintf( outPort_Package "%-4n\t%3s\t%5s\t%22s\t%-35s\t%-35s\t%-11s\t%-11s\t%-8s\n"
						CounterBondPad
						t_orient
						GroupName
						GroupType
						PadName
						PadPackageName
						"XXXXXX"
						"YYYYYY"
						"Dummy"
						)
			); end if

			geRefresh( )


		); end for End Drawing Bondpad



;Samson3
		; Output CDL File
		; ================================
		if( member( cadr( FullIO ) list( "S" "PRH" "PRL" "PCH" "PCL" ))
			then
				fprintf( outPort_CDL_tmp "X%s\t" t_instName )

				for( i 0 length( nth( 3 FullIO ))-1
			
;					NetChild = nth( i nth( 3 FullIO ))
					NetChild = cadr( nth( i nth( 3 FullIO )))
					; NetParent = nth( i nth( 6 ActualIO ))

					; In case the NetElement is just a simple string
					; Position Mapping
					; ===============================================
					if( stringp( NetChild )
						then
							fprintf( outPort_CDL_tmp "%s\t" NetChild )
					); end if 
		
				); end for

				fprintf( outPort_CDL_tmp "%s\n" cadr( nth( 1 ActualIO )))
		); end if

		; Output Cast File and Spec File
		; ================================
		if( member( cadr( FullIO ) list( "S" "PRH" "PRL" "PCH" "PCL" ))
			then

				CastCellName_L = cadr( nth( 1 ActualIO ))
				CastCellName_M = cadr( nth( 1 ActualIO ))
				CastCellName_S = cadr( nth( 1 ActualIO ))

				rexCompile( strcat( car( nth( 1 ActualIO )) "." ))
				CastCellName_S = rexReplace( CastCellName_S "" 0 )
				rexCompile( "\\.0" )
				CastCellName_S = rexReplace( CastCellName_S "" 0 )
				rexCompile( "\\.1" )
				CastCellName_S = rexReplace( CastCellName_S "" 0 )

				fprintf( outPort_Cast_tmp "%s %s(" CastCellName_S t_instName )

				for( i 0 length( nth( 3 FullIO ))-1
			
;Samson4
					; NetChild = nth( i nth( 3 FullIO ))
					NetChild = cadr( nth( i nth( 3 FullIO )))
					; NetParent = nth( i nth( 6 ActualIO ))

					; In case the NetElement is just a simple string
					; Position Mapping
					; ===============================================
					if( stringp( NetChild )
						then
							fprintf( outPort_Cast_tmp "%s" NetChild )
							i!=length( nth( 3 FullIO ))-1 && fprintf( outPort_Cast_tmp "," )
					); end if 

		
				); end for

				fprintf( outPort_Cast_tmp ");\n" )


				; Output Spec File
				; ================
				
				rexCompile( "\\.0" )
				CastCellName_M = rexReplace( CastCellName_M "" 0 )
				rexCompile( "\\.1" )
				CastCellName_M = rexReplace( CastCellName_M "" 0 )

				fprintf( outPort_Spec_tmp "\t%s :> \n" CastCellName_M )
				fprintf( outPort_Spec_tmp "\t\t%s %s; \n" CastCellName_L t_instName )

		); end if

	); end foreach

	fprintf( outPort_CDL_tmp ".ENDS PADFRAME \n" )
	close( outPort_Package )
	close( outPort_CDL_tmp )
	close( outPort_Cast_tmp )
	close( outPort_Spec_tmp )
	close( outPort_Binding_Rule )


	; Create the Final CDL File
	; ==============================
	l_IOPort = sort( l_IOPort `alphalessp )

	fprintf( outPort_CDL ".SUBCKT PADFRAME \n" )

	foreach( IOPort l_IOPort
		fprintf( outPort_CDL "+ %s\n" IOPort )
	); end foreach

	close( outPort_CDL )

	sh( sprintf( nil "cat %s >> %s" Filename_CDL_tmp Filename_CDL ))


	; Create the Final Cast File
	; ==============================
	l_IOPort = sort( l_IOPort `alphalessp )
	l_IOType = sort( l_IOType `alphalessp )

	fprintf( outPort_Cast "/* Automatically generated.  Modify at your own risk. */\n")
	fprintf( outPort_Cast "/* \n" )
	fprintf( outPort_Cast "  Created by Octopus (Fulcrum Padframe Compiler) \n" )
	fprintf( outPort_Cast "*/ \n" )

	fprintf( outPort_Cast "module chip.x6.padframe;\n\n" )

	foreach( IOType l_IOType
		fprintf( outPort_Cast "import %s;\n" IOType )
	); end foreach

	fprintf( outPort_Cast "define PADFRAME ()\n" )
	fprintf( outPort_Cast "(\n" )

	foreach( IOPort l_IOPort
		if( IOPort != nth( length( l_IOPort )-1 l_IOPort )
			then
				fprintf( outPort_Cast "node %s;\n" IOPort )
			else
				fprintf( outPort_Cast "node %s\n" IOPort )
		); end if 
	); end foreach

	fprintf( outPort_Cast ") <: NULL \n" )

	close( outPort_Cast )

	sh( sprintf( nil "echo \"{\" >> %s" Filename_Cast ))
	sh( sprintf( nil "echo \"subcells {\" >> %s" Filename_Cast ))
	sh( sprintf( nil "cat %s >> %s" Filename_Cast_tmp Filename_Cast ))
	sh( sprintf( nil "echo \"}\" >> %s" Filename_Cast ))
	sh( sprintf( nil "echo \"}\" >> %s" Filename_Cast ))



	; Create the Final Spec File
	; ==============================
	l_IOPort = sort( l_IOPort `alphalessp )

	fprintf( outPort_Spec "/* Automatically generated.  Modify at your own risk. */\n")
	fprintf( outPort_Spec "/* \n" )
	fprintf( outPort_Spec "  Created by Octopus (Fulcrum Padframe Compiler) \n" )
	fprintf( outPort_Spec "*/ \n" )

	fprintf( outPort_Spec "module chip.x6.padframe.PADFRAME;\n" )
	fprintf( outPort_Spec "define \"0\"()(\n" )

	foreach( IOPort l_IOPort
		if( IOPort != nth( length( l_IOPort )-1 l_IOPort )
			then
				fprintf( outPort_Spec "\tnode %s;\n" IOPort )
			else
				fprintf( outPort_Spec "\tnode %s\n" IOPort )
		); end if 
	); end foreach

	close( outPort_Spec )

	sh( sprintf( nil "echo \") <: chip.x6.padframe.PADFRAME {\" >> %s"	Filename_Spec ))
	sh( sprintf( nil "echo \"  subtypes {\" >> %s"				Filename_Spec ))
	sh( sprintf( nil "cat %s >> %s" Filename_Spec_tmp 			Filename_Spec ))
	sh( sprintf( nil "echo \"  }\" >> %s"					Filename_Spec ))
	sh( sprintf( nil "echo \"}  \" >> %s"					Filename_Spec ))


	t

	); end let

); end procedure




; ========================================================================
; This procedure is used to draw inplace pins
; ========================================================================
procedure( DrawIO_ProcDrawInPlacePinsAndLabels( Cell @key 
				( Pin   	list( "drawing" t )) 
				( Label 	list( "drawing" t ))
				( ExceptList	nil )
				( DebugOn 	nil )
			)

let( ( 	x_inst 		y_inst 
	xlow_instTerm 	ylow_instTerm
	xhigh_instTerm 	yhigh_instTerm 
	xlow_instTerm_new 	ylow_instTerm_new
	xhigh_instTerm_new 	yhigh_instTerm_new 
	d_Pin		DrawPin		PinLayer	PinPurpose 
	d_Label 	DrawLabel	LabelLayer	LabelPurpose
	d_Net		l_Net
	bBox_instTerm	xy_instTerm
	)

	DrawPin   = cadr( Pin )
	DrawLabel = cadr( Label )
	PinPurpose   = car( Pin )
	LabelPurpose = car( Label )

	if( Cell->objType == "inst" && Cell->viewName != "symbolic"
		then

			DebugOn && printf( "DrawIO_ProcDrawInPlacePinsAndLabels --> Start cell \"%s\" \"%s\" \n" Cell->cellName Cell->name )

			x_inst = car( Cell->xy )
			y_inst = cadr( Cell->xy )

			; Go thru each instTerms (IO pins)
			foreach( d_instTerm Cell->instTerms

				DebugOn && printf( "DrawIO_ProcDrawInPlacePinsAndLabels --> --> Start instTerms = %s -> %s \n" d_instTerm->name d_instTerm->net->name )
					
				foreach( d_instTermPin d_instTerm->term->pins

				; Filter out Symbolic Pins
				; ========================
				if( d_instTermPin->fig->objType != "inst" 
					then

					xlow_instTerm  = caar( d_instTermPin->fig->bBox )
					ylow_instTerm  = cadar( d_instTermPin->fig->bBox )
					xhigh_instTerm = caadr( d_instTermPin->fig->bBox )
					yhigh_instTerm = cadadr( d_instTermPin->fig->bBox )

					if( member( d_instTermPin->fig->objType list( "polygon" "path" ))
						then
						 	xlow_instTerm   = car( car( d_instTermPin->fig->points ) ) - 0.01
							ylow_instTerm   = cadr( car( d_instTermPin->fig->points ) ) - 0.01
							xhigh_instTerm  = car( car( d_instTermPin->fig->points ) ) + 0.01
							yhigh_instTerm  = cadr( car( d_instTermPin->fig->points ) ) + 0.01
					); end if
							
                                        case( Cell->orient

						( "R0" 
							xlow_instTerm   = x_inst + xlow_instTerm 
							ylow_instTerm   = y_inst + ylow_instTerm 
							xhigh_instTerm  = x_inst + xhigh_instTerm 
							yhigh_instTerm  = y_inst + yhigh_instTerm 
                                                )
 
						( "R90" 
							xlow_instTerm_new   = x_inst - ylow_instTerm 
							ylow_instTerm_new   = y_inst + xlow_instTerm 
							xhigh_instTerm_new  = x_inst - yhigh_instTerm 
							yhigh_instTerm_new  = y_inst + xhigh_instTerm 

							xlow_instTerm   = xlow_instTerm_new 
							ylow_instTerm   = ylow_instTerm_new 
							xhigh_instTerm  = xhigh_instTerm_new 
							yhigh_instTerm  = yhigh_instTerm_new 
							
                                                )
 
 						( "R180" 
							xlow_instTerm   = x_inst - xlow_instTerm 
							ylow_instTerm   = y_inst - ylow_instTerm 
							xhigh_instTerm  = x_inst - xhigh_instTerm 
							yhigh_instTerm  = y_inst - yhigh_instTerm 
 							
                                                )

						( "R270" 
							xlow_instTerm_new   = x_inst + ylow_instTerm 
							ylow_instTerm_new   = y_inst - xlow_instTerm 
							xhigh_instTerm_new  = x_inst + yhigh_instTerm 
							yhigh_instTerm_new  = y_inst - xhigh_instTerm 

							xlow_instTerm   = xlow_instTerm_new 
							ylow_instTerm   = ylow_instTerm_new 
							xhigh_instTerm  = xhigh_instTerm_new 
							yhigh_instTerm  = yhigh_instTerm_new 
							
                                                )
 
                                               	( "MY" 
 							xlow_instTerm   = x_inst - xlow_instTerm 
							ylow_instTerm   = y_inst + ylow_instTerm 
							xhigh_instTerm  = x_inst - xhigh_instTerm 
							yhigh_instTerm  = y_inst + yhigh_instTerm 
                                                )

						( "MYR90" 
							xlow_instTerm_new   = x_inst - ylow_instTerm 
							ylow_instTerm_new   = y_inst - xlow_instTerm 
							xhigh_instTerm_new  = x_inst - yhigh_instTerm 
							yhigh_instTerm_new  = y_inst - xhigh_instTerm 

							xlow_instTerm   = xlow_instTerm_new 
							ylow_instTerm   = ylow_instTerm_new 
							xhigh_instTerm  = xhigh_instTerm_new 
							yhigh_instTerm  = yhigh_instTerm_new 
							
                                                )
 
                                                                         
                                                ( "MX" 
 							xlow_instTerm   = x_inst + xlow_instTerm 
							ylow_instTerm   = y_inst - ylow_instTerm 
							xhigh_instTerm  = x_inst + xhigh_instTerm 
							yhigh_instTerm  = y_inst - yhigh_instTerm 
                                                )
                                                                        
						( "MXR90" 
							xlow_instTerm_new   = x_inst + ylow_instTerm 
							ylow_instTerm_new   = y_inst + xlow_instTerm 
							xhigh_instTerm_new  = x_inst + yhigh_instTerm 
							yhigh_instTerm_new  = y_inst + xhigh_instTerm 

							xlow_instTerm   = xlow_instTerm_new 
							ylow_instTerm   = ylow_instTerm_new 
							xhigh_instTerm  = xhigh_instTerm_new 
							yhigh_instTerm  = yhigh_instTerm_new 
							
                                                )
 
                                                ( t 
						 	xlow_instTerm   = x_inst + xlow_instTerm 
							ylow_instTerm   = y_inst + ylow_instTerm 
							xhigh_instTerm  = x_inst + xhigh_instTerm 
							yhigh_instTerm  = y_inst + yhigh_instTerm 
                                                )

					); end case


					bBox_instTerm   = list(
								xlow_instTerm:ylow_instTerm
								xhigh_instTerm:yhigh_instTerm
								)
					xy_instTerm	= list( 
								( xlow_instTerm+xhigh_instTerm )/2
								( ylow_instTerm+yhigh_instTerm )/2
								)
					if( DrawPin && !member( d_instTerm->net->name ExceptList )
						then
							PinLayer = d_instTermPin->fig->layerName

							; First, draw the pin
							d_Pin = dbCreateRect( geGetWindowCellView( ) list( PinLayer PinPurpose ) bBox_instTerm )

							; Second, Fill up Connectivity
							d_Net = dbMakeNet( geGetWindowCellView() d_instTerm->net->name )
							dbCreatePin( d_Net d_Pin )
					); end if


					if( DrawLabel && !member( d_instTerm->net->name ExceptList )
						then
							LabelLayer = d_instTermPin->fig->layerName

							d_Label = dbCreateLabel( 
								geGetWindowCellView( )
								list( LabelLayer LabelPurpose )
								xy_instTerm
								sprintf( nil "%s" d_instTerm->net->name )
								"centerCenter"
								"R0"
								"stick"
								0.2
								)
					);end if

				); end if filter out inst type pins

				);foreach d_instTermPin

			); foreach instTerm

			DebugOn && printf( "DrawIO_ProcDrawInPlacePinsAndLabels --> Finish Cell \"%s\" \"%s\" \n"  Cell->cellName Cell->name )

	); end if

	xy_instTerm

); end let

);end procedure




; ========================================================================
; This procedure is the MAIN GUI
; ========================================================================
procedure( DrawIO_ProcWrapper(	@key	( DebugOn 	nil )
				)

	let((	TR_DrawIO_ModeField
		TR_DrawIO_DriverPitchField
		TR_DrawIO_CompileGlobalsField
		TR_DrawIO_ReadInputField
		TR_DrawIO_PerformAliasingField
		TR_DrawIO_AnalysisInputField
		TR_DrawIO_DrawIOField
		TR_DrawIO_theForm
		)


	TR_DrawIO_ProjectField = hiCreateStringField(
			?name 		`TR_DrawIO_ProjectField
			?prompt 	"Current Project"
			?defValue 	"x6"
			?enabled 	t
			)

	TR_DrawIO_ModeField = hiCreateRadioField( 
			?name 		`TR_DrawIO_ModeField
			?choices 	list( "Normal" "FixCount" )
			?prompt 	"Mode" 
			?defValue 	"Normal"
			?enabled 	t
			?callback	list( "TR_DrawIO_theForm->TR_DrawIO_DriverPitchField->enabled = t" "TR_DrawIO_theForm->TR_DrawIO_DriverPitchField->enabled = nil" ) 
			)

	TR_DrawIO_DriverPitchField = hiCreateFloatField(
			?name 		`TR_DrawIO_DriverPitchField
			?defValue 	35.0
			?enabled 	t
			?callback	"TR_DrawIO_DriverPitchField_CB( hiGetCurrentForm() ?DebugOn t )"
			)

	TR_DrawIO_CompileGlobalsField = hiCreateButton( 
			?name 		`TR_DrawIO_CompileGlobalsField
			?buttonText	"Compile Global Variables" 
			?enabled 	t
			?callback	"DrawIO_ProcCompileGlobals( 	?Project 	TR_DrawIO_theForm->TR_DrawIO_ProjectField->value 
									?DriverPitch 	TR_DrawIO_theForm->TR_DrawIO_DriverPitchField->value 
									)"
			)

	TR_DrawIO_ReadInputField = hiCreateButton( 
			?name 		`TR_DrawIO_ReadInputField
			?buttonText	"Read Input" 
			?enabled 	t
			?callback	"DrawIO_ProcReadInput( )"
			)

	TR_DrawIO_PerformAliasingField = hiCreateButton( 
			?name 		`TR_DrawIO_PerformAliasingField
			?buttonText	"Perform Name Aliasing" 
			?enabled 	t
			?callback	"DrawIO_ProcPerformAliasing( )"
			)

	TR_DrawIO_AnalysisInputField = hiCreateButton( 
			?name 		`TR_DrawIO_AnalysisInputField
			?buttonText	"Analysis Input" 
			?enabled 	t
			?callback	"DrawIO_ProcAnalysisInput( ?Mode TR_DrawIO_ModeField->value )"
			)

	TR_DrawIO_DrawIOField = hiCreateButton( 
			?name 		`TR_DrawIO_DrawIOField
			?buttonText	"Instantiate Padframe" 
			?enabled 	t
			?callback	"DrawIO_ProcDrawIO( )"
			)

	TR_DrawIO_theForm = hiCreateAppForm(
		?name		`TR_DrawIO_theForm
		?initialSize	list( 300 500 )
		?fields		list(   
					list( TR_DrawIO_ProjectField		  0:0   300:30 200)

					list( TR_DrawIO_ModeField		  0:30  200:30 50 )
					list( TR_DrawIO_DriverPitchField	200:30  100:30  0 )

					list( TR_DrawIO_CompileGlobalsField	  0:60  300:30  0 )
					list( TR_DrawIO_ReadInputField		  0:90  300:30  0 )
					list( TR_DrawIO_PerformAliasingField	  0:120 300:30  0 )
					list( TR_DrawIO_AnalysisInputField	  0:150 300:30  0 )
					list( TR_DrawIO_DrawIOField		  0:180 300:30  0 )
					)
		?callback       ""
		)


	hiDisplayForm( TR_DrawIO_theForm )

	); end let


); end procedure




; ================================================
; Callback Function for TR_DrawIO_DriverPitchField
; ================================================
procedure( TR_DrawIO_DriverPitchField_CB( theForm @key ( DebugOn t ))

	DrawIO_GlobalDriverPitch = float( theForm->TR_DrawIO_DriverPitchField->value )
	DebugOn && printf( "TR_DrawIO_DriverPitchField_CB --> DrawIO_GlobalDriverPitch = %n\n" DrawIO_GlobalDriverPitch ) 

); end procedure

