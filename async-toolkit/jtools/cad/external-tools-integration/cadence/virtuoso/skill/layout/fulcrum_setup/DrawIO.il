; This Fuction is used to initialize the entire IO Pad Frame 
; This first function does nothing but just create and display the form
procedure( TR_DrawIO( )

	let( ( 	TR_DrawIO_SignalListFile
		TR_DrawIO_ChipHeight
		TR_DrawIO_ChipWidth
		TR_DrawIO_IOVDD
		TR_DrawIO_IOVSS
		TR_DrawIO_CoreVDD
		TR_DrawIO_CoreVSS
		TR_DrawIO_theForm
		)

	; Create Fields for the Form
	TR_DrawIO_SignalListFile = hiCreateStringField( 
        	?name 'TR_DrawIO_SignalListFile 
        	?prompt "Signal List File" 
        	?defValue "tc3_pinout.txt" 
        	?editable t )

	TR_DrawIO_ChipHeight = hiCreateStringField( 
        	?name 'TR_DrawIO_ChipHeight
        	?prompt "Chip Height (in millimeter)" 
        	?defValue "5" 
        	?editable t )

	TR_DrawIO_ChipWidth = hiCreateStringField( 
        	?name 'TR_DrawIO_ChipWidth
        	?prompt "Chip Width (in millimeter)" 
        	?defValue "5" 
        	?editable t )

	TR_DrawIO_MinDriverSpacing = hiCreateStringField( 
        	?name 'TR_DrawIO_MinDriverSpacing
        	?prompt "MinDriverSpacing (in micron)" 
        	?defValue "60" 
        	?editable t )


	; This is the Format
	; IOGenericType = list( libName cellName viewName prOrigin prWidth prHeight list( PinName x:y ))


	TR_DrawIO_IOVDD = hiCreateStringField( 
        	?name 'TR_DrawIO_IOVDD
        	?prompt "IO Pad VDD Info " 
        	?defValue "IOVDD = list( \"fb_tpz013g2_110c\" \"PVDD2DGZ\" \"layout\" 0:0 35 246 \"VD33\" )" 
        	?editable nil )

	TR_DrawIO_IOVSS = hiCreateStringField( 
        	?name 'TR_DrawIO_IOVSS
        	?prompt "IO Pad VSS Info " 
        	?defValue "IOVSS = list( \"fb_tpz013g2_110c\" \"PVSS2DGZ\" \"layout\" 0:0 35 246 \"VSSPST\" )" 
        	?editable nil )

	TR_DrawIO_CoreVDD = hiCreateStringField( 
        	?name 'TR_DrawIO_CoreVDD
        	?prompt "Core VDD Info " 
        	?defValue "CoreVDD = list( \"fb_tpz013g2_110c\" \"PVDD1DGZ\" \"layout\" 0:0 35 246 \"Vdd\" list( \"VDD\" 17.6:245.9 ) )" 
        	?editable nil )

	TR_DrawIO_CoreVSS = hiCreateStringField( 
        	?name 'TR_DrawIO_CoreVSS
        	?prompt "Core VSS Info " 
        	?defValue "CoreVSS = list( \"fb_tpz013g2_110c\" \"PVSS1DGZ\" \"layout\" 0:0 35 246 \"GND\" list( \"VSS\" 17.6:245.9 ) )" 
        	?editable nil )

	TR_DrawIO_DigitalSignalInputType1 = hiCreateStringField( 
        	?name 'TR_DrawIO_DigitalSignalInputType1
        	?prompt "Digital Signal Input Type1 " 
        	?defValue "DSI1 = list( \"fb_tpz013g2_110c\" \"PDIDGZ\" \"layout\" 0:0 35 246 list( \"C\" 8.2:245.9 ) list( \"PAD\" 17.2:-5.6 ) )" 
        	?editable nil )

	TR_DrawIO_DigitalSignalOutputType1 = hiCreateStringField( 
        	?name 'TR_DrawIO_DigitalSignalOutputType1
        	?prompt "Digital Signal Output Type1 " 
        	?defValue "DSO1 = list( \"fb_tpz013g2_110c\" \"PDO02CDG\" \"layout\" 0:0 35 246 list( \"I\" 4.6:245.9 ) list( \"PAD\" 17.2:-5.6 ) )" 
        	?editable nil )

	TR_DrawIO_AnalogSignalType1 = hiCreateStringField( 
        	?name 'TR_DrawIO_AnalogSignalType1
        	?prompt "Analog Signal Type1 " 
        	?defValue "list( \"chip.tc3.padframe\" \"PRT08DGZ\" \"layout\" 0:0 35 246 list( \"I\" 4.6:245.9 ) list( \"PAD\" 17.2:-5.6 ) list( \"OEN\" 18.6:245.9 )" 
        	?editable nil )

	TR_DrawIO_Topology = hiCreateStringField( 
	       	?name 'TR_DrawIO_Topology
     		?prompt "Enter Desired Topology" 
  		?defValue "list( \"S\" \"P\" )" 
    		?editable t )

	TR_DrawIO_TopologyAdvancedSpacing = hiCreateRadioField(
        	?name 'TR_DrawIO_TopologyAdvancedSpacing
        	?choices list( "None" "Max Feeder" "Max Power" )
        	?defValue "None"
        	?prompt "Advanced Spacing?" 
		?callback list( "TR_DrawIO_TopologyAdvancedSpacing_CB( hiGetCurrentForm() ) "
				"TR_DrawIO_TopologyAdvancedSpacing_CB( hiGetCurrentForm() ) "
				"TR_DrawIO_TopologyAdvancedSpacing_CB( hiGetCurrentForm() ) " )
		)

	TR_DrawIO_CreatePadList = hiCreateToggleField( 
        	?name 'TR_DrawIO_CreatePadList
        	?choices list(  list( `IOVDD "IOVDD" ) list( `IOVSS "IOVSS" ) list( `CoreVDD "CoreVDD" ) list( `CoreVSS "CoreVSS" ) )
		?numSelect 4
        	?defValue list( t t t t )
        	?prompt "Create Power Pad List" 
		)

	TR_DrawIO_GenCDL = hiCreateToggleField( 
        	?name 'TR_DrawIO_GenCDL
        	?choices list(  list( `GenCDL nil ) )
		?numSelect 1
        	?defValue list( nil )
        	?prompt "Generate CDL File?" 
		?callback list( "TR_DrawIO_GenCDL_CB( hiGetCurrentForm() ) ")
		)

	TR_DrawIO_GenCAST = hiCreateToggleField( 
        	?name 'TR_DrawIO_GenCAST
        	?choices list(  list( `GenCAST nil ) )
		?numSelect 1
        	?defValue list( nil )
        	?prompt "Generate CAST File?" 
		?callback list( "TR_DrawIO_GenCAST_CB( hiGetCurrentForm() ) ")
		)

	; Create the Form
	TR_DrawIO_theForm = hiCreateAppForm( 
        	?name 'TR_DrawIO_theForm
        	?fields  '( 	TR_DrawIO_SignalListFile
				TR_DrawIO_ChipHeight
				TR_DrawIO_ChipWidth
				TR_DrawIO_MinDriverSpacing
				TR_DrawIO_IOVDD
				TR_DrawIO_IOVSS
				TR_DrawIO_CoreVDD
				TR_DrawIO_CoreVSS
				TR_DrawIO_DigitalSignalInputType1
				TR_DrawIO_DigitalSignalOutputType1
				TR_DrawIO_AnalogSignalType1
				TR_DrawIO_Topology
				TR_DrawIO_TopologyAdvancedSpacing
				TR_DrawIO_CreatePadList
				TR_DrawIO_GenCDL
				TR_DrawIO_GenCAST
				)
		?initialSize t
		?callback "TR_DrawIO_processDataCB( hiGetCurrentForm() )"
	)

	; Display the Form	
	hiDisplayForm( TR_DrawIO_theForm )

	); end let

); end procedure


; This second function is the actual one that process the data
procedure( TR_DrawIO_processDataCB( theForm )

	let( (	SignalListFile
		ChipHeight
		ChipWidth
		IOVDD
		IOVSS
		CoreVDD
		CoreVSS
		DSI1
		DSO1
		AS1
		inPort 
		H
		x 
		y 
		CoreVDDNetID CoreVDDNetName CoreVDDPin
		CoreVSSNetID CoreVSSNetName CoreVSSPin )

	SignalListFile = theForm->TR_DrawIO_SignalListFile->value
	ChipHeight_um  = evalstring( theForm->TR_DrawIO_ChipHeight->value ) * 1000
	ChipWidth_um   = evalstring( theForm->TR_DrawIO_ChipWidth->value ) * 1000
	MinDriverSpacing = evalstring( theForm->TR_DrawIO_MinDriverSpacing->value )
	evalstring( theForm->TR_DrawIO_IOVDD->value )
	evalstring( theForm->TR_DrawIO_IOVSS->value ) 
	evalstring( theForm->TR_DrawIO_CoreVDD->value )
	evalstring( theForm->TR_DrawIO_CoreVSS->value )
	evalstring( theForm->TR_DrawIO_DigitalSignalInputType1->value )
	evalstring( theForm->TR_DrawIO_DigitalSignalOutputType1->value )
	AS1 = evalstring( theForm->TR_DrawIO_AnalogSignalType1->value )

	; Power Pad Ordering and Counter
	PowerPadList = nil

	if( theForm->TR_DrawIO_CreatePadList->CoreVSS->value
		then
			PowerPadList = cons( "CoreVSS" PowerPadList )
	)	
	if( theForm->TR_DrawIO_CreatePadList->CoreVDD->value
		then
			PowerPadList = cons( "CoreVDD" PowerPadList )
	)	
	if( theForm->TR_DrawIO_CreatePadList->IOVDD->value
		then
			PowerPadList = cons( "IOVDD" PowerPadList )
	)	
	if( theForm->TR_DrawIO_CreatePadList->IOVSS->value
		then
			PowerPadList = cons( "IOVSS" PowerPadList )
	)	

	; PowerPadList = list( "IOVDD" "IOVSS" "CoreVDD" "CoreVSS" )


	if(  theForm->TR_DrawIO_Topology->enabled == t 
		then
			l_IO_Topology    = evalstring( theForm->TR_DrawIO_Topology->value )
		else
			l_IO_Topology    = list( "S" "P" )
	)

	case( theForm->TR_DrawIO_TopologyAdvancedSpacing->value
		("Max Feeder"
			MaxFeeder = t
			MaxPower = nil
		)
		("Max Power"
			MaxFeeder = t
			MaxPower = t
		)
		("None"
			MaxFeeder = nil
			MaxPower = nil
		)
		( t
			MaxFeeder = nil
			MaxPower = nil
		)

	); end case


	GenCDL  = theForm->TR_DrawIO_GenCDL->GenCDL->value
	GenCDL && ( CDLFileName = theForm->TR_DrawIO_CDLFileName->value )
	GenCAST = theForm->TR_DrawIO_GenCAST->GenCAST->value
	GenCAST && ( CASTFileName = theForm->TR_DrawIO_CASTFileName->value )

	; Need to be put in the Form later

	BONDIZ40 = list( "fb_tpz013g2_110c" "PADIZ40" "layout" 17:0 35 88.8 )
	CORPAD   = list( "fb_tpz013g2_110c" "PCORNERDGZ" "layout" 0:0 246 246 89.2 89.2 )

	F0_005 = list( "fb_tpz013g2_110c" "PFEED0_005Z" "layout" 0:0 0.005 246 )
	F0_01  = list( "fb_tpz013g2_110c" "PFEED0_01Z"  "layout" 0:0 0.01  246 )
	F0_1   = list( "fb_tpz013g2_110c" "PFEED0_1Z"   "layout" 0:0 0.1   246 )
	F1     = list( "fb_tpz013g2_110c" "PFEED1Z"     "layout" 0:0 1     246 )
	F5     = list( "fb_tpz013g2_110c" "PFEED5Z"     "layout" 0:0 5     246 )
	F10    = list( "fb_tpz013g2_110c" "PFEED10Z"    "layout" 0:0 10    246 )
	F20    = list( "fb_tpz013g2_110c" "PFEED20Z"    "layout" 0:0 20    246 )
	F60    = list( "testspace2" "PFEED60Z"    "layout" 0:0 60    246 )

	;=================================================================================
	; COUNTERS
	;=================================================================================
	FeederCellCounter = 0
	TotalNumberOfSignals = 0
	PowerPadCounter = 0
	IOVDDCounter = 0
	IOVSSCounter = 0
	CoreVDDCounter = 0
	CoreVSSCounter = 0
	SignalPadCounter = 0
	TempCounter = 0
	
	;=================================================================================


	H = "TR_DrawIO_processDataCB --> "
	x = 0
	y = 0 
	drawFirstCorner = t
	currentOri = "R0"
	nextOri = "R0"

	; This defines the values for parameters related to the pads for the Core Power
	CoreVDDNetName = car( cdddr( cdddr( CoreVDD )))
	CoreVSSNetName = car( cdddr( cdddr( CoreVSS )))
	CoreVDDNetID = nil
	CoreVSSNetID = nil
	CoreVDDPin = cadr( cdddr( cdddr( CoreVDD )))
	CoreVSSPin = cadr( cdddr( cdddr( CoreVSS )))

	; This defines the values for parameters related to the pads for the IO Power
	IOVDDNetName = car( cdddr( cdddr( IOVDD )))
	IOVSSNetName = car( cdddr( cdddr( IOVSS )))
	
	; This defines the values for parameters related to the pads for DSO1
	DSO1_IPin = car( cdddr( cdddr( DSO1 )))
	DSO1_PADPin = cadr( cdddr( cdddr( DSO1 )))

	; This defines the values for parameters related to the pads for DSI1
	DSI1_CPin = car( cdddr( cdddr( DSI1 )))
	DSI1_PADPin = cadr( cdddr( cdddr( DSI1 )))

	; This defines the values for parameters related to the pads for AS1
	AS1_CPin = car( cdddr( cdddr( AS1 )))
	AS1_PADPin = cadr( cdddr( cdddr( AS1 )))
	AS1_OENPin = caddr( cdddr( cdddr( AS1 )))

	; This is used to check if it is the end of file
	EndOfFile = nil

	; These parameters are used to deal with corner pads
	CORPAD_extraOffsetX = cadddr( cdddr( CORPAD ))
	CORPAD_extraOffsetY = cadddr( cdddr( cdr( CORPAD )))
	CORPAD_prWidth        = cadddr( cdr( CORPAD ))
	CORPAD_prHeight       = cadddr( cddr(  CORPAD ))
	
	; We need to offset these parameters 'coz there is a certain part of the CORPAD which is out of 
	; the bounding box but yet count towards the height and width
	currentWidth  = CORPAD_extraOffsetX
	currentHeight = CORPAD_extraOffsetY


	; This is the tolerance for the error when dealing with floating point math
	Delta = 0.0000000001

	StopDrawing = nil
	
	; =================================================================================================
	; CDL OUTPUT
	; =================================================================================================
	; Print out the SUBCKT and port part of the CDL file
	if( GenCDL
		then
			; File I/O Port Definitions
			inPort = infile( SignalListFile ) 
			outPort = outfile( CDLFileName ) 

			fprintf( outPort ".SUBCKT PADFRAME\n" )
			fprintf( outPort "    + %-20s\n" CoreVDDNetName )
			fprintf( outPort "    + %-20s\n" CoreVSSNetName )

			when( inPort 

				while( fscanf( inPort "%s " signalName )
					if( 	signalName != "IOVDD" &&
						signalName != "IOVSS" &&
						signalName != "CoreVDD" &&
						signalName != "CoreVSS"
						then
							fscanf( inPort "%s\n" ioActualType)
							; CDL Output
							fprintf( outPort "    + %-20s\n" signalName )
						)

				); end while

				close( inPort )


;			when( inPort 
;				while( fscanf( inPort "%s %s\n" signalName ioActualType )
;
; 
;					; CDL Output
;					fprintf( outPort "    + %-20s\n" signalName )
;
;				); end while
;
;			close( inPort )

			); end when

			close( outPort )

	); end if

	; =================================================================================================
	; CAST OUTPUT
	; =================================================================================================
	; Print out the header part of the cast file 

	if( GenCAST
		then
			; File I/O Port Definitions
			outPort = outfile( CASTFileName ) 

			fprintf( outPort "define PADFRAME()\n" )
			fprintf( outPort "   ( \n" )
			fprintf( outPort "     // Wake up and Fill out the port list yourself!!! \n" )
			fprintf( outPort "     // I have no CAST knowledge to fill out this for you!!! \n" )
			fprintf( outPort "   ) \n" )
			fprintf( outPort "{ \n" )
	
			close( outPort )

	); end if


	; =================================================================================================
	; FEEDER INSERTION 
	; =================================================================================================
	; This routine is used to insert sufficient feeder pads to satisfy the minimum driver spacing 
	; set by the user (MinDriverSpacing)

	l_IO_Topology_new = list( )
	foreach( IO_Topology l_IO_Topology

		l_IO_Topology_new = reverse( cons( IO_Topology reverse( l_IO_Topology_new )))

		if( IO_Topology == "S" || IO_Topology == "P"
			then	
				l_IO_Topology_new = reverse( cons( evalstring( "list( \"F\" MinDriverSpacing )" ) reverse( l_IO_Topology_new )))
		); end if	

	); end foreach	
	l_IO_Topology = l_IO_Topology_new
	l_IO_Topology_save = l_IO_Topology_new


	; =================================================================================================
	; ADVANCED SPACING TOPOLOGY -- MAX POWER PADS 
	; =================================================================================================
	inPort = infile( SignalListFile )
	TotalLengthAvaliable = 2 * ( ChipHeight_um - ( CORPAD_extraOffsetY + CORPAD_extraOffsetX + (2 * CORPAD_prWidth ))) +
			       2 * ( ChipWidth_um - ( CORPAD_extraOffsetY + CORPAD_extraOffsetX + (2 * CORPAD_prWidth ))) -
			       4 * MinDriverSpacing
 
	if( MaxPower
		then
			when( inPort 

				while( fscanf( inPort "%s " signalName )
					if( 	signalName != "IOVDD" &&
						signalName != "IOVSS" &&
						signalName != "CoreVDD" &&
						signalName != "CoreVSS"
						then
							fscanf( inPort "%s\n" ioActualType)
							TotalLengthAvaliable = TotalLengthAvaliable - cadr( cdddr( evalstring( ioActualType ))) - MinDriverSpacing
							TotalNumberOfSignals = TotalNumberOfSignals + 1
						)

				); end while

				close( inPort )

			) ;end when

			TotalNumberOfPower = TotalLengthAvaliable / ( 35 + MinDriverSpacing ) 
			TargetRatio = TotalNumberOfPower / TotalNumberOfSignals

	); end if

	; =================================================================================================
	; File I/O Port Definitions
	inPort = infile( SignalListFile ) 
	GenCDL && ( outPortCDL  = outfile( CDLFileName "a" ) )
	GenCAST && ( outPortCAST = outfile( CASTFileName "a" ) ) 

	when( inPort 
		while( !EndOfFile

			; Decoding the Topology
			foreach( currentSymbolType l_IO_Topology

				; See if it is an advanced Topology Setting
				if( listp( currentSymbolType )
					then 
						Parameter = cadr( currentSymbolType )
				  		currentSymbolType = car( currentSymbolType )
				); end if

				; Clear Variables to start
				l_currentCellType = list( )
				l_t_instName = list( )
				addedWidth = 0 



				case( currentSymbolType
					( "S" 

						if( fscanf( inPort "%s " signalName )
							then
								case( signalName 
									( "IOVDD"

										IOVDDCounter = IOVDDCounter + 1
										TempCounter = IOVDDCounter
										currentPowerPadNumberString = strcat( "_" sprintf( nil "%d" TempCounter ))
										currentPowerPad = "IOVDD"
		
										PowerPadCounter = PowerPadCounter + 1
										l_currentCellType = list( "BONDIZ40" currentPowerPad )
										l_t_instName = list( strcat( strcat( "bondP_" currentPowerPad ) currentPowerPadNumberString ) 
												     strcat( strcat( "padP_"   currentPowerPad ) currentPowerPadNumberString ))
										addedWidth = cadr( cdddr( evalstring( currentPowerPad )))

									)

									( "IOVSS"

										IOVSSCounter = IOVSSCounter + 1
										TempCounter = IOVSSCounter
										currentPowerPadNumberString = strcat( "_" sprintf( nil "%d" TempCounter ))
										currentPowerPad = "IOVSS"
		
										PowerPadCounter = PowerPadCounter + 1
										l_currentCellType = list( "BONDIZ40" currentPowerPad )
										l_t_instName = list( strcat( strcat( "bondP_" currentPowerPad ) currentPowerPadNumberString ) 
												     strcat( strcat( "padP_"   currentPowerPad ) currentPowerPadNumberString ))
										addedWidth = cadr( cdddr( evalstring( currentPowerPad )))

									)

									( "CoreVDD"

										CoreVDDCounter = CoreVDDCounter + 1
										TempCounter = CoreVDDCounter
										currentPowerPadNumberString = strcat( "_" sprintf( nil "%d" TempCounter ))
										currentPowerPad = "CoreVDD"
		
										PowerPadCounter = PowerPadCounter + 1
										l_currentCellType = list( "BONDIZ40" currentPowerPad )
										l_t_instName = list( strcat( strcat( "bondP_" currentPowerPad ) currentPowerPadNumberString ) 
												     strcat( strcat( "padP_"   currentPowerPad ) currentPowerPadNumberString ))
										addedWidth = cadr( cdddr( evalstring( currentPowerPad )))

									)

									( "CoreVSS"

										CoreVSSCounter = CoreVSSCounter + 1
										TempCounter = CoreVSSCounter
										currentPowerPadNumberString = strcat( "_" sprintf( nil "%d" TempCounter ))
										currentPowerPad = "CoreVSS"
		
										PowerPadCounter = PowerPadCounter + 1
										l_currentCellType = list( "BONDIZ40" currentPowerPad )
										l_t_instName = list( strcat( strcat( "bondP_" currentPowerPad ) currentPowerPadNumberString ) 
												     strcat( strcat( "padP_"   currentPowerPad ) currentPowerPadNumberString ))
										addedWidth = cadr( cdddr( evalstring( currentPowerPad )))

									)

									( t 
										fscanf( inPort "%s\n" ioActualType )
										EndOfFile = nil
										FillFeeder = nil
										l_currentCellType = list( "BONDIZ40" ioActualType )
										l_t_instName = list( strcat( "bondS_" signalName )
												     strcat( "padS_" signalName ))
										addedWidth = cadr( cdddr( evalstring( ioActualType )))

										SignalPadCounter = SignalPadCounter + 1
									)
								); end case

							else
								EndOfFile = t
								FillFeeder = t
						); end if

					)

					( "P" 
						if( !EndOfFile
							then
                                                                currentPowerPad = car( PowerPadList )
                                                                PowerPadList = remove( currentPowerPad PowerPadList )
                                                                PowerPadList = reverse( cons( currentPowerPad reverse( PowerPadList )))

								case( currentPowerPad 
									( "IOVDD"

										IOVDDCounter = IOVDDCounter + 1
										TempCounter = IOVDDCounter
									)
									( "IOVSS"

										printf( "IOVSSCounter = %n\n" IOVSSCounter )
										IOVSSCounter = IOVSSCounter + 1
										TempCounter = IOVSSCounter
									)
									( "CoreVDD"

										CoreVDDCounter = CoreVDDCounter + 1
										TempCounter = CoreVDDCounter
									)
									( "CoreVSS"

										CoreVSSCounter = CoreVSSCounter + 1
										TempCounter = CoreVSSCounter
									)
								)

								currentPowerPadNumberString = strcat( "_" sprintf( nil "%d" TempCounter ))

								PowerPadCounter = PowerPadCounter + 1
								l_currentCellType = list( "BONDIZ40" currentPowerPad )
								l_t_instName = list( strcat( strcat( "bondP_" currentPowerPad ) currentPowerPadNumberString ) 
										     strcat( strcat( "padP_"   currentPowerPad ) currentPowerPadNumberString ))
								addedWidth = cadr( cdddr( evalstring( currentPowerPad )))

						); end if
					)

					( "F" 			
						if( !EndOfFile
							then
								 l_currentCellType = list()
								      l_t_instName = list()
									addedWidth = Parameter 
								neededFeederLength = Parameter

								; ====================================================================================
								; Put in the corresponding Feeder Cells
								; We have Feeder Cells in dimensions of 0.005, 0.01, 0.1, 1, 5, 10, 20 microns
								dividerNameList = list( "F20" "F10" "F5" "F1" "F0_1" "F0_01" "F0_005" )

								foreach( dividerName dividerNameList
 		
									divider = cadr( cdddr( evalstring( dividerName )))
									numAdded = floor( neededFeederLength / divider )
		
									neededFeederLength = neededFeederLength - ( numAdded * divider ) 
									for( i 1 numAdded
		
										FeederCellCounterString = sprintf( nil "%n" FeederCellCounter )
										l_currentCellType = cons( dividerName l_currentCellType )
										t_instName = strcat( "padFeeder_" FeederCellCounterString )
										FeederCellCounter = FeederCellCounter + 1
										l_t_instName = cons( t_instName l_t_instName )
		
									); end for
											
								); end foreach
								; ====================================================================================
						); end if
					)

				); end case


				; See if the pads need to turn the corner
				; See if a Corner Pad is necessary
				if( 	( currentOri == "R0"   && ( currentWidth + CORPAD_extraOffsetX + CORPAD_prWidth + addedWidth ) > ChipWidth_um ) ||
					( currentOri == "R90"  && ( currentHeight + CORPAD_extraOffsetX + CORPAD_prWidth + addedWidth ) > ChipHeight_um ) || 
					( currentOri == "R180" && ( currentWidth + CORPAD_extraOffsetX + CORPAD_prWidth + addedWidth ) > ChipWidth_um ) ||
					( currentOri == "R270" && EndOfFile == t && FillFeeder == t )

					then

						case( currentOri
							( "R0"
								nextOri = "R90"
								ChipSideLength_um = ChipWidth_um
								currentSideLength = currentWidth 
							)
							( "R90"
								nextOri = "R180"
								ChipSideLength_um = ChipHeight_um 
								currentSideLength = currentHeight 
							)
							( "R180"
								nextOri = "R270"
								ChipSideLength_um = ChipWidth_um 
								currentSideLength = currentWidth 
							)
							( "R270"
								nextOri = "R0"
								ChipSideLength_um = ChipHeight_um 
								currentSideLength = currentHeight 
							)
						); end case


						if( !EndOfFile 
							then

								neededFeederLength = MinDriverSpacing

								; ====================================================================================
								; Put in the corresponding Feeder Cells
								; We have Feeder Cells in dimensions of 0.005, 0.01, 0.1, 1, 5, 10, 20 microns
								dividerNameList = list( "F20" "F10" "F5" "F1" "F0_1" "F0_01" "F0_005" )

								foreach( dividerName dividerNameList
 		
									divider = cadr( cdddr( evalstring( dividerName )))
									numAdded = floor( neededFeederLength / divider )
		
									neededFeederLength = neededFeederLength - ( numAdded * divider ) 
									for( i 1 numAdded
		
										FeederCellCounterString = sprintf( nil "%n" FeederCellCounter )
										l_currentCellType = cons( dividerName l_currentCellType )
										t_instName = strcat( "padFeeder_" FeederCellCounterString )
										FeederCellCounter = FeederCellCounter + 1
										l_t_instName = cons( t_instName l_t_instName )
		
									); end for
											
								); end foreach
								; ====================================================================================

								; Then add a Feeder pad to the first of the list
;								l_currentCellType = cons( "F60" l_currentCellType )
;								l_t_instName = cons( strcat( "padF_" nextOri ) l_t_instName )

								; First add a corner pad to the first of the list before the just-added feeder pad
								l_currentCellType = cons( "CORPAD" l_currentCellType )
								l_t_instName = cons( strcat( "CORPAD_" nextOri ) l_t_instName )
						); end if

						neededFeederLength = max( ( ChipSideLength_um - ( currentSideLength + CORPAD_extraOffsetX + CORPAD_prWidth )) 0 )

						; ====================================================================================
						; Put in the corresponding Feeder Cells
						; We have Feeder Cells in dimensions of 0.005, 0.01, 0.1, 1, 5, 10, 20 microns
						dividerNameList = list( "F20" "F10" "F5" "F1" "F0_1" "F0_01" "F0_005" )

						foreach( dividerName dividerNameList
 
							divider = cadr( cdddr( evalstring( dividerName )))
							numAdded = floor( neededFeederLength / divider )

							neededFeederLength = neededFeederLength - ( numAdded * divider ) 
							for( i 1 numAdded

								FeederCellCounterString = sprintf( nil "%n" FeederCellCounter )
								l_currentCellType = cons( dividerName l_currentCellType )
								t_instName = strcat( "padFeeder_" FeederCellCounterString )
								FeederCellCounter = FeederCellCounter + 1
								l_t_instName = cons( t_instName l_t_instName )

							); end for
									
						); end foreach
						; ====================================================================================

						StopDrawing = nil



				) ;end if

				; Special Case for first Pad
				if( drawFirstCorner
					then


						neededFeederLength = MinDriverSpacing

						; ====================================================================================
						; Put in the corresponding Feeder Cells
						; We have Feeder Cells in dimensions of 0.005, 0.01, 0.1, 1, 5, 10, 20 microns
						dividerNameList = list( "F20" "F10" "F5" "F1" "F0_1" "F0_01" "F0_005" )

						foreach( dividerName dividerNameList
 		
							divider = cadr( cdddr( evalstring( dividerName )))
							numAdded = floor( neededFeederLength / divider )
		
							neededFeederLength = neededFeederLength - ( numAdded * divider ) 
							for( i 1 numAdded
		
								FeederCellCounterString = sprintf( nil "%n" FeederCellCounter )
								l_currentCellType = cons( dividerName l_currentCellType )
								t_instName = strcat( "padFeeder_" FeederCellCounterString )
								FeederCellCounter = FeederCellCounter + 1
								l_t_instName = cons( t_instName l_t_instName )
		
							); end for
											
						); end foreach
						; ====================================================================================


						; Then add a Feeder pad to the first of the list
						;l_currentCellType = cons( "F60" l_currentCellType )
						;l_t_instName = cons( strcat( "padF_" nextOri ) l_t_instName )

						; First add a corner pad to the first of the list before the just-added feeder pad
						l_currentCellType = cons( "CORPAD" l_currentCellType )
						l_t_instName = cons( strcat( "CORPAD_" nextOri ) l_t_instName )

						drawFirstCorner = nil
				); end if


				; See if you have way too many pads than the space you have on the chip
				if(  ( currentOri == "R270"  && (( currentHeight + CORPAD_extraOffsetX + CORPAD_prWidth + addedWidth ) - ChipHeight_um ) > Delta )
					|| StopDrawing
					then
						if( fscanf( inPort "%s %s\n" signalName ioActualType )
							then
								printf( "%sERROR!!! Too many pads!!! Not enough space\n" H )
								printf( "%sERROR!!! Try to change topology or Change Size of Chip\n" H )
						)
						l_currentCellType = list()

						StopDrawing = t

				); end if 



				; This is the actual loop that do the drawing and instantiations
				foreach( currentCellType l_currentCellType
					
					; Get the prBound info for the cell
					prWidth    = car( cdr( cdr( cdr( cdr( evalstring( currentCellType ))))))
					prHeight   = car( cdr( cdr( cdr( cdr( cdr( evalstring( currentCellType )))))))
					prX        = car( car( cdr( cdr( cdr( evalstring( currentCellType ))))))
					prY        = cadr( car( cdr( cdr( cdr( evalstring( currentCellType ))))))
					
					; See if it is the second corner pad or up (ignore the first one)
					if( currentCellType == "CORPAD" && currentOri != nextOri
						then
							case( currentOri
								( "R0" 
									x = x + CORPAD_prHeight
									currentWidth = CORPAD_extraOffsetX
								)
									 
								( "R90" 
									y = y + CORPAD_prWidth
									currentHeight = CORPAD_extraOffsetY
								)

								( "R180" 
									x = x - CORPAD_prHeight
									currentWidth = CORPAD_extraOffsetX
								)									 

							) ;end case
							currentOri = nextOri

						;	x = x + CORPAD_prHeight
					);end if


					; If it is a bond pad, it should be on the outer side
					if( currentCellType == "BONDIZ40"
						then
							case( currentOri
								( "R0"
									l_origin   = list( x-prX y-prHeight-prY )
								)
								( "R90"
									l_origin   = list( x-prY+prHeight y-prX )
								)
								( "R180"
									l_origin   = list( x+prX y+prHeight+prY )
								)
								( "R270"
									l_origin   = list( x+prY-prHeight y+prX )
								)

							); end case
						else
							l_origin   = list( x-prX y-prY )
					); end if

					d_cellView = geGetWindowCellView( )
					t_libName  = car( evalstring( currentCellType ))
					t_cellName = cadr( evalstring( currentCellType ))
					t_viewName = caddr( evalstring( currentCellType ))
					t_instName = car( l_t_instName )
					l_t_instName =  remove( t_instName l_t_instName )
					t_orient   = currentOri 

					printf( "%sPad \"%s\" created using type ( %s %s %s ) \n"
						H 
						t_instName 
						t_libName 
						t_cellName 
						t_viewName )
	
					; Put in Signal Pad
					d_inst = dbCreateInstByMasterName( 
						d_cellView 
						t_libName 
						t_cellName 
						t_viewName 
						t_instName 
						l_origin 
						t_orient 
						)

					;=================================================================================================
					; CDL OUTPUT
					;=================================================================================================
					; Output CDL for IOVDD and IOVSS only 

					if( ( currentCellType == "IOVDD" || currentCellType == "IOVSS" ) && GenCDL 
						then
							fprintf( outPortCDL 
								 "X%-20s %-10s %-10s %-10s %-10s %-20s\n"
								 t_instName 
								 CoreVDDNetName
								 CoreVSSNetName
								 IOVDDNetName
								 IOVSSNetName
								 t_cellName )

					); end if 
					;=================================================================================================


					; Add Connectivity and pins Info to the Driver Cells
					case( currentCellType

						( "CoreVDD" 
							VDDpinOffset = cadr( CoreVDDPin )

							; Moving the origin
							case( currentOri
								( "R0"
									l_origin_VDD = list( 	( car( l_origin ) + car( VDDpinOffset )) 
												( cadr( l_origin ) + cadr( VDDpinOffset )))
								)
								( "R90"
									l_origin_VDD = list( 	( car( l_origin ) - cadr( VDDpinOffset )) 
												( cadr( l_origin ) + car( VDDpinOffset )))
								)
								( "R180"
									l_origin_VDD = list( 	( car( l_origin ) - car( VDDpinOffset )) 
												( cadr( l_origin ) - cadr( VDDpinOffset )))
								)
								( "R270"
									l_origin_VDD = list( 	( car( l_origin ) + cadr( VDDpinOffset )) 
												( cadr( l_origin ) - car( VDDpinOffset )))
								)

							); end case

							; Draw the actual Pin
							d_pin = dbCreateInstByMasterName( 
								geGetWindowCellView() 
								"tsmc13lg"
								"m3_T" 
								"symbolic" 
;								strcat( "pinVDD_" signalName ) 
								strcat( "pinVDD_" currentPowerPadNumberString ) 
								l_origin_VDD 
								"R0" 
								)

							CoreVDDNetID || ( CoreVDDNetID = dbCreateNet( geGetWindowCellView() CoreVDDNetName ))

							; Create Connectivity
							dbCreatePin( CoreVDDNetID d_pin )
							dbCreateConnByName( CoreVDDNetID d_inst car( CoreVDDPin ))

							;===================================================================================
							; CDL OUTPUT
							;===================================================================================
							if( GenCDL
								then
									fprintf( outPortCDL 
										"X%-20s %-10s %-10s %-10s %-10s %-20s\n"
										t_instName 
										CoreVDDNetName
										CoreVSSNetName	 
										IOVDDNetName
										IOVSSNetName	 
										t_cellName )
							); end if
							;===================================================================================

						)

						( "CoreVSS" 
							VSSpinOffset = cadr( CoreVSSPin )

							; Moving the origin
							case( currentOri
								( "R0"
									l_origin_VSS = list( 	( car( l_origin ) + car( VSSpinOffset )) 
												( cadr( l_origin ) + cadr( VSSpinOffset )))
								)
								( "R90"
									l_origin_VSS = list( 	( car( l_origin ) - cadr( VSSpinOffset )) 
												( cadr( l_origin ) + car( VSSpinOffset )))
								)
								( "R180"
									l_origin_VSS = list( 	( car( l_origin ) - car( VSSpinOffset )) 
												( cadr( l_origin ) - cadr( VSSpinOffset )))
								)
								( "R270"
									l_origin_VSS = list( 	( car( l_origin ) + cadr( VSSpinOffset )) 
												( cadr( l_origin ) - car( VSSpinOffset )))
								)

							); end case


							; Draw the actual Pin
							d_pin = dbCreateInstByMasterName( 
								geGetWindowCellView() 
								"tsmc13lg"
								"m3_T" 
								"symbolic" 
;								strcat( "pinVSS_" signalName ) 
								strcat( "pinVSS_" currentPowerPadNumberString ) 
								l_origin_VSS 
								"R0" 
								)

							CoreVSSNetID || ( CoreVSSNetID = dbCreateNet( geGetWindowCellView() CoreVSSNetName ))

							; Create Connectivity
							dbCreatePin( CoreVSSNetID d_pin )
							dbCreateConnByName( CoreVSSNetID d_inst car(CoreVSSPin) )

							;===================================================================================
							; CDL OUTPUT
							;===================================================================================
							if( GenCDL
								then
									fprintf( outPortCDL 
										"X%-20s %-10s %-10s %-10s %-10s %-20s\n"
										t_instName 
										CoreVDDNetName
										CoreVSSNetName
										IOVDDNetName
										IOVSSNetName
										t_cellName )
							); end if
							;===================================================================================
						)

						( "DSO1" 
							DSO1_IPinOffset = cadr( DSO1_IPin )

							; Moving the origin
							case( currentOri
								( "R0"
									l_origin_DSO1_IPin = list( 	( car( l_origin ) + car( DSO1_IPinOffset )) 
													( cadr( l_origin ) + cadr( DSO1_IPinOffset )))
								)
								( "R90"
									l_origin_DSO1_IPin = list( 	( car( l_origin ) - cadr( DSO1_IPinOffset )) 
													( cadr( l_origin ) + car( DSO1_IPinOffset )))
								)
								( "R180"
									l_origin_DSO1_IPin = list( 	( car( l_origin ) - car( DSO1_IPinOffset )) 
													( cadr( l_origin ) - cadr( DSO1_IPinOffset )))
								)
								( "R270"
									l_origin_DSO1_IPin = list( 	( car( l_origin ) + cadr( DSO1_IPinOffset )) 
													( cadr( l_origin ) - car( DSO1_IPinOffset )))
								)

							); end case

							; Draw the actual Pin
							d_pin = dbCreateInstByMasterName( 
								geGetWindowCellView() 
								"tsmc13lg"
								"m3_T" 
								"symbolic" 
								strcat( "pinDSO1_" signalName ) 
								l_origin_DSO1_IPin 
								"R0" 
								)

							DSO1NetID = dbCreateNet( geGetWindowCellView() signalName )

							; Create Connectivity
							dbCreatePin( DSO1NetID d_pin )
							dbCreateConnByName( DSO1NetID d_inst car( DSO1_IPin ) )



							DSO1_PADPinOffset = cadr( DSO1_PADPin )

							; Moving the origin
							case( currentOri
								( "R0"
									l_origin_DSO1_PADPin = list( 	( car( l_origin ) + car( DSO1_PADPinOffset )) 
													( cadr( l_origin ) + cadr( DSO1_PADPinOffset )))
								)
								( "R90"
									l_origin_DSO1_PADPin = list( 	( car( l_origin ) - cadr( DSO1_PADPinOffset )) 
													( cadr( l_origin ) + car( DSO1_PADPinOffset )))
								)
								( "R180"
									l_origin_DSO1_PADPin = list( 	( car( l_origin ) - car( DSO1_PADPinOffset )) 
													( cadr( l_origin ) - cadr( DSO1_PADPinOffset )))
								)
								( "R270"
									l_origin_DSO1_PADPin = list( 	( car( l_origin ) + cadr( DSO1_PADPinOffset )) 
													( cadr( l_origin ) - car( DSO1_PADPinOffset )))
								)

							); end case

							; Draw the actual Pin
							d_pin = dbCreateInstByMasterName( 
								geGetWindowCellView() 
								"tsmc13lg"
								"m3_T" 
								"symbolic" 
								strcat( "pad_" signalName ) 
								l_origin_DSO1_PADPin 
								"R0" 
								)

							DSO1NetID = dbCreateNet( geGetWindowCellView() strcat( "pad_" signalName ))

							; Create Connectivity
							dbCreatePin( DSO1NetID d_pin )
							dbCreateConnByName( DSO1NetID d_inst car( DSO1_PADPin ) )

							;===================================================================================
							; CDL OUTPUT
							;===================================================================================
							if( GenCDL
								then
									fprintf( outPortCDL 
										"X%-20s %-10s %-10s %-10s %-10s %-20s %-20s %-20s\n"
										t_instName
										CoreVDDNetName
										CoreVSSNetName
										IOVDDNetName
										IOVSSNetName
										strcat( "pad_" signalName )
										signalName 
										t_cellName )
							); end if

							;===================================================================================
							; CAST OUTPUT
							;===================================================================================
							if( GenCAST
								then
									fprintf( outPortCAST 
										"%-10s %15s( %-15s, %-15s );\n"
										t_cellName
										t_instName
										strcat( "pad_" signalName )
										signalName )
							); end if
							;===================================================================================

						); end type DSO1

						( "DSI1" 
							DSI1_CPinOffset = cadr( DSI1_CPin )

							; Moving the origin
							case( currentOri
								( "R0"
									l_origin_DSI1_CPin = list( 	( car( l_origin ) + car( DSI1_CPinOffset )) 
													( cadr( l_origin ) + cadr( DSI1_CPinOffset )))
								)
								( "R90"
									l_origin_DSI1_CPin = list( 	( car( l_origin ) - cadr( DSI1_CPinOffset )) 
													( cadr( l_origin ) + car( DSI1_CPinOffset )))
								)
								( "R180"
									l_origin_DSI1_CPin = list( 	( car( l_origin ) - car( DSI1_CPinOffset )) 
													( cadr( l_origin ) - cadr( DSI1_CPinOffset )))
								)
								( "R270"
									l_origin_DSI1_CPin = list( 	( car( l_origin ) + cadr( DSI1_CPinOffset )) 
													( cadr( l_origin ) - car( DSI1_CPinOffset )))
								)

							); end case

							; Draw the actual Pin
							d_pin = dbCreateInstByMasterName( 
								geGetWindowCellView() 
								"tsmc13lg"
								"m3_T" 
								"symbolic" 
								strcat( "pinDSI1_" signalName ) 
								l_origin_DSI1_CPin 
								"R0" 
								)

							DSI1NetID = dbCreateNet( geGetWindowCellView() signalName )

							; Create Connectivity
							dbCreatePin( DSI1NetID d_pin )
							dbCreateConnByName( DSI1NetID d_inst car( DSI1_CPin ) )



							DSI1_PADPinOffset = cadr( DSI1_PADPin )

							; Moving the origin
							case( currentOri
								( "R0"
									l_origin_DSI1_PADPin = list( 	( car( l_origin ) + car( DSI1_PADPinOffset )) 
													( cadr( l_origin ) + cadr( DSI1_PADPinOffset )))
								)
								( "R90"
									l_origin_DSI1_PADPin = list( 	( car( l_origin ) - cadr( DSI1_PADPinOffset )) 
													( cadr( l_origin ) + car( DSI1_PADPinOffset )))
								)
								( "R180"
									l_origin_DSI1_PADPin = list( 	( car( l_origin ) - car( DSI1_PADPinOffset )) 
													( cadr( l_origin ) - cadr( DSI1_PADPinOffset )))
								)
								( "R270"
									l_origin_DSI1_PADPin = list( 	( car( l_origin ) + cadr( DSI1_PADPinOffset )) 
													( cadr( l_origin ) - car( DSI1_PADPinOffset )))
								)

							); end case

							; Draw the actual Pin
							d_pin = dbCreateInstByMasterName( 
								geGetWindowCellView() 
								"tsmc13lg"
								"m3_T" 
								"symbolic" 
								strcat( "pad_" signalName ) 
								l_origin_DSI1_PADPin 
								"R0" 
								)

							DSI1NetID = dbCreateNet( geGetWindowCellView() strcat( "pad_" signalName ))

							; Create Connectivity
							dbCreatePin( DSI1NetID d_pin )
							dbCreateConnByName( DSI1NetID d_inst car( DSI1_PADPin ) )

							;===================================================================================
							; CDL OUTPUT
							;===================================================================================
							if( GenCDL
								then
									fprintf( outPortCDL 
										"X%-20s %-10s %-10s %-10s %-10s %-20s %-20s %-20s\n"
										t_instName
										CoreVDDNetName
										CoreVSSNetName
										IOVDDNetName
										IOVSSNetName
										signalName 
										strcat( "pad_" signalName )
										t_cellName )
							); end if

							;===================================================================================
							; CAST OUTPUT
							;===================================================================================
							if( GenCAST
								then
									fprintf( outPortCAST 
										"%-10s %15s( %-15s, %-15s );\n"
										t_cellName
										t_instName
										signalName
										strcat( "pad_" signalName ))
							); end if
							;===================================================================================
						); end type DSI1
















						( "AS1" 
							AS1_CPinOffset = cadr( AS1_CPin )

							; Moving the origin
							case( currentOri
								( "R0"
									l_origin_AS1_CPin = list( 	( car( l_origin ) + car( AS1_CPinOffset )) 
													( cadr( l_origin ) + cadr( AS1_CPinOffset )))
								)
								( "R90"
									l_origin_AS1_CPin = list( 	( car( l_origin ) - cadr( AS1_CPinOffset )) 
													( cadr( l_origin ) + car( AS1_CPinOffset )))
								)
								( "R180"
									l_origin_AS1_CPin = list( 	( car( l_origin ) - car( AS1_CPinOffset )) 
													( cadr( l_origin ) - cadr( AS1_CPinOffset )))
								)
								( "R270"
									l_origin_AS1_CPin = list( 	( car( l_origin ) + cadr( AS1_CPinOffset )) 
													( cadr( l_origin ) - car( AS1_CPinOffset )))
								)

							); end case

							; Draw the actual Pin
							d_pin = dbCreateInstByMasterName( 
								geGetWindowCellView() 
								"tsmc13lg"
								"m3_T" 
								"symbolic" 
								strcat( "pinAS1_" signalName ) 
								l_origin_AS1_CPin 
								"R0" 
								)

							AS1NetID = dbCreateNet( geGetWindowCellView() signalName )

							; Create Connectivity
							dbCreatePin( AS1NetID d_pin )
							dbCreateConnByName( AS1NetID d_inst car( AS1_CPin ) )



							AS1_PADPinOffset = cadr( AS1_PADPin )

							; Moving the origin
							case( currentOri
								( "R0"
									l_origin_AS1_PADPin = list( 	( car( l_origin ) + car( AS1_PADPinOffset )) 
													( cadr( l_origin ) + cadr( AS1_PADPinOffset )))
								)
								( "R90"
									l_origin_AS1_PADPin = list( 	( car( l_origin ) - cadr( AS1_PADPinOffset )) 
													( cadr( l_origin ) + car( AS1_PADPinOffset )))
								)
								( "R180"
									l_origin_AS1_PADPin = list( 	( car( l_origin ) - car( AS1_PADPinOffset )) 
													( cadr( l_origin ) - cadr( AS1_PADPinOffset )))
								)
								( "R270"
									l_origin_AS1_PADPin = list( 	( car( l_origin ) + cadr( AS1_PADPinOffset )) 
													( cadr( l_origin ) - car( AS1_PADPinOffset )))
								)

							); end case

							; Draw the actual Pin
							d_pin = dbCreateInstByMasterName( 
								geGetWindowCellView() 
								"tsmc13lg"
								"m3_T" 
								"symbolic" 
								strcat( "pad_" signalName ) 
								l_origin_AS1_PADPin 
								"R0" 
								)

							AS1NetID = dbCreateNet( geGetWindowCellView() strcat( "pad_" signalName ))

							; Create Connectivity
							dbCreatePin( AS1NetID d_pin )
							dbCreateConnByName( AS1NetID d_inst car( AS1_PADPin ) )





							AS1_OENPinOffset = cadr( AS1_OENPin )

							; Moving the origin
							case( currentOri
								( "R0"
									l_origin_AS1_OENPin = list( 	( car( l_origin ) + car( AS1_OENPinOffset )) 
													( cadr( l_origin ) + cadr( AS1_OENPinOffset )))
								)
								( "R90"
									l_origin_AS1_OENPin = list( 	( car( l_origin ) - cadr( AS1_OENPinOffset )) 
													( cadr( l_origin ) + car( AS1_OENPinOffset )))
								)
								( "R180"
									l_origin_AS1_OENPin = list( 	( car( l_origin ) - car( AS1_OENPinOffset )) 
													( cadr( l_origin ) - cadr( AS1_OENPinOffset )))
								)
								( "R270"
									l_origin_AS1_OENPin = list( 	( car( l_origin ) + cadr( AS1_OENPinOffset )) 
													( cadr( l_origin ) - car( AS1_OENPinOffset )))
								)

							); end case

							; Draw the actual Pin
							d_pin = dbCreateInstByMasterName( 
								geGetWindowCellView() 
								"tsmc13lg"
								"m3_T" 
								"symbolic" 
								strcat( "oen_" signalName ) 
								l_origin_AS1_OENPin 
								"R0" 
								)

							AS1NetID = dbCreateNet( geGetWindowCellView() strcat( "oen_" signalName ))

							; Create Connectivity
							dbCreatePin( AS1NetID d_pin )
							dbCreateConnByName( AS1NetID d_inst car( AS1_OENPin ) )

							;===================================================================================
							; CDL OUTPUT
							;===================================================================================
							if( GenCDL
								then
									fprintf( outPortCDL 
										"X%-15s %-7s %-7s %-7s %-7s %-15s %-15s %-15s %-15s\n"
										t_instName
										CoreVDDNetName
										CoreVSSNetName
										IOVDDNetName
										IOVSSNetName
										strcat( "pad_" signalName )
										signalName 
										strcat( "oen_" signalName )
										t_cellName )
							); end if

							;===================================================================================
							; CAST OUTPUT
							;===================================================================================
							if( GenCAST
								then
									fprintf( outPortCAST 
										"%-10s %15s( %-15s, %-15s, %-15s );\n"
										t_cellName
										t_instName
										strcat( "pad_" signalName )
										signalName
										strcat( "oen_" signalName ))
							); end if
							;===================================================================================
						); end type AS1

					); end case

					;=================================================================================================

					; Increment the pointer only if it is not doing a bonding pad
					if( currentCellType != "BONDIZ40"
						then
							case( currentOri
								( "R0"
									currentWidth = currentWidth + prWidth
									x = x + prWidth
								)
								( "R90"
									currentHeight = currentHeight + prWidth
									y = y + prWidth
								)
								( "R180"
									currentWidth = currentWidth + prWidth
									x = x - prWidth
								)
								( "R270"
									currentHeight = currentHeight + prWidth
									y = y - prWidth
								)
							); end case
					); end if

				) ;end foreach


			if( MaxPower && !EndOfFile
				then

					PowerPadCounterTemp = PowerPadCounter + 1
					SignalPadCounterTemp  = SignalPadCounter + 1
					CurrentRatio = PowerPadCounterTemp / SignalPadCounterTemp



					l_IO_Topology = l_IO_Topology_save
					ExitWhile = nil

					printf( "Target Ratio is %f\n" TargetRatio )

					while( ( TargetRatio - CurrentRatio ) > 0 && !ExitWhile

							PowerPadCounterTemp = PowerPadCounterTemp + 1
							CurrentRatio = float( PowerPadCounterTemp ) / float( SignalPadCounterTemp )
							printf( "Current Ratio is %3n / %-3n = %f\n" PowerPadCounterTemp SignalPadCounterTemp CurrentRatio )

							if( ( TargetRatio - CurrentRatio ) > 0
								then

									l_IO_Topology = reverse( cons( "P" reverse( l_IO_Topology )))
									l_IO_Topology = reverse( cons( evalstring( "list( \"F\" MinDriverSpacing )") reverse( l_IO_Topology )))
									printf( "Added 1 Power Pad \n")


								else 
									ExitWhile = t	
							)					

					);end while
					println( l_IO_Topology )


			); end if



			FillFeeder = nil

			); end foreach

		); end while

		close( inPort )
		if( GenCDL
			then 
				close( outPortCDL )
		)

		if( GenCAST 
			then
				close( outPortCAST )
		)

	); end when

	;===================================================================================
	; CDL OUTPUT
	;===================================================================================
	; Print out .ENDS part of the CDL file
	if( GenCDL
		then
			outPortCDL = outfile( CDLFileName "a" ) 
			fprintf( outPortCDL ".ENDS PADFRAME\n" )
			close( outPortCDL )

	); end if

	;===================================================================================
	; CAST OUTPUT
	;===================================================================================
	if( GenCAST
		then
			outPortCAST = outfile( CASTFileName "a" ) 
			fprintf( outPortCAST "}\n" )
			close( outPortCAST )
	); end if
	;===================================================================================

	); end let

); end procedure




; =============================================================================================
; TR_DrawIO_GenCDL_CB
; =============================================================================================
; This is a small CallBack Function used to get the filename for the desired cdl output file

procedure( TR_DrawIO_GenCDL_CB( theForm )

	let( (	GenCDL 
		TR_DrawIO_CDLFileName
		ReturnValue )

	GenCDL = theForm->TR_DrawIO_GenCDL->GenCDL->value
	ReturnValue = t

	if( GenCDL
		then
			TR_DrawIO_CDLFileName = hiCreateStringField( 
		        	?name 'TR_DrawIO_CDLFileName
 		       		?prompt "Enter CDL Filename" 
  		      		?defValue "./PADFRAME.cdl" 
  		      		?editable t )

			hiAddField( theForm TR_DrawIO_CDLFileName )
		else
			hiDeleteField( theForm `TR_DrawIO_CDLFileName )

	); end if

	ReturnValue

	); end let

); end procedure

; =============================================================================================
; TR_DrawIO_GenCAST_CB
; =============================================================================================
; This is a small CallBack Function used to get the filename for the desired cast output file

procedure( TR_DrawIO_GenCAST_CB( theForm )

	let( (	GenCAST 
		TR_DrawIO_CASTFileName
		ReturnValue )

	GenCAST = theForm->TR_DrawIO_GenCAST->GenCAST->value
	ReturnValue = t

	if( GenCAST
		then
			TR_DrawIO_CASTFileName = hiCreateStringField( 
		        	?name 'TR_DrawIO_CASTFileName
 		       		?prompt "Enter CAST Filename" 
  		      		?defValue "./PADFRAME.cast" 
  		      		?editable t )

			hiAddField( theForm TR_DrawIO_CASTFileName )
		else
			hiDeleteField( theForm `TR_DrawIO_CASTFileName )

	); end if

	ReturnValue

	); end let

); end procedure

; =============================================================================================
; TR_DrawIO_TopologyAdvancedSpacing_CB_MaxPower
; =============================================================================================
; This is a small CallBack Function used to enable/disable the topology field

procedure( TR_DrawIO_TopologyAdvancedSpacing_CB( theForm )

	let( (	
		ReturnValue )

	ReturnValue = t

	case( theForm->TR_DrawIO_TopologyAdvancedSpacing->value

		("None"

			theForm->TR_DrawIO_Topology->enabled = t

		)
		("Max Feeder"

			theForm->TR_DrawIO_Topology->enabled = nil

		)
		("Max Power"

			theForm->TR_DrawIO_Topology->enabled = nil

		)

	); end case

	ReturnValue

	); end let

); end procedure

