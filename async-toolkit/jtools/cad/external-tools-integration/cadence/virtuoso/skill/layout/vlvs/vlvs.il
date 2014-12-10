; Support All Orientations
; R0 R90 R180 R270 MY MX MXR90 MYR90
;
; Symbolic Pins (objType == "inst") have been filtered out
; The script won't do anything 
;
; Limitation
; 1. NOT recognize SUBSTRATE pins (eg NWELL Pin) -> won't virtually join them
; 2. Gates MUST have well plugs to begin with 
;
;
; Useful Skill
; ------------
; car( caddr( car( geGetSelectedSet())->instTerms)->term->pins)->fig->objType
;
;
; Global Variables
; ----------------
; 1. TR_AssuraLVS_VLVS_BlackBoxFile
; 2. TR_AssuraLVS_VLVS_ReplayFile
; 3. TR_AssuraLVS_NetlistPath
; 4. TR_AssuraLVS_DeleteAllTempFiles
; 5. TR_AssuraLVS_VirtualJoinedPinList


TR_AssuraLVS_cellName = geGetWindowCellView()->cellName
TR_AssuraLVS_LVSParameterList = list( 
	"runName"
	"runDir"
	"technology"
;	"extract"
;	"compare"
;	"bind"
;	"rsfInclude"
	"layLibrary"
	"layCell"
	"layView"
	"schCellName"
	"schNetlists"
	"blackBoxCell"
;	"runBlackBox"
;	"runVirtual"
	)


TR_AssuraLVS_VLVS_BlackBoxFile    = "./VLVS_BlackBoxFile.txt"
TR_AssuraLVS_VLVS_ReplayFile	  = "./VLVS_ReplayFile.txt"
TR_AssuraLVS_NetlistPath	  = ( ConfigFileGetValue TheCDSConfigTable "CDL_DIR" )
(when TR_AssuraLVS_NetlistPath==""
  TR_AssuraLVS_NetlistPath	  = "./netlist"
)
TR_AssuraLVS_DeleteAllTempFiles	  = nil
TR_AssuraLVS_VirtualJoinedPinList = nil

procedure( DrawPinsLabels( @key 
				( Pin   list( "drawing" t )) 
				( Label list( "drawing" t ))
				( runPartialVirtual  nil )
				( DebugOn nil  )
			)

let( ( 	l_d_Selected 
	x_inst 		y_inst 
	xlow_instTerm 	ylow_instTerm
	xhigh_instTerm 	yhigh_instTerm 
	xlow_instTerm_new 	ylow_instTerm_new
	xhigh_instTerm_new 	yhigh_instTerm_new 
	d_Pin		DrawPin		PinLayer	PinPurpose 
	d_Label 	DrawLabel	LabelLayer	LabelPurpose
	d_Net		l_Net
	bBox_instTerm	xy_instTerm
	l_BlackBoxCells l_BlackBoxCells_temp Previous_BlackBoxCells
	isContinue
	lpp lpp_List lpp_Selectable lpp_Visible
	pin_Selectable inst_Selectable
	)


	; =============================================
	; Set up the LSW Window to the correct settings
	; =============================================
	pin_Selectable = leIsPinSelectable( )
	inst_Selectable = leIsInstSelectable( )

	leSetPinSelectable( t )
	leSetInstSelectable( t )
	lpp_List = leGetValidLayerList( techGetTechFile( geGetWindowCellView()) )
	lpp_List = foreach( mapcar lpp lpp_List

		lpp_Selectable = leIsLayerSelectable( lpp )
		lpp_Visible = leIsLayerVisible( lpp )

		leSetLayerSelectable( lpp t )
		leSetLayerVisible( lpp t )

		list( lpp lpp_Selectable lpp_Visible )

	); end foreach







	isContinue = t

	; This is the return list containing the netNames that needed to be "joinable"
	l_BlackBoxCells = nil

	TR_AssuraLVS_VirtualJoinedPinList = nil

	DrawPin   = cadr( Pin )
	DrawLabel = cadr( Label )
	PinPurpose   = car( Pin )
	LabelPurpose = car( Label )

	geSelectAllFig( )
	l_d_Selected = geGetSelectedSet( )

	; Getting all the nets in the layout
	foreach( d_Selected l_d_Selected	
		foreach( d_instTerm d_Selected->instTerms
			if( !member( d_instTerm->net->name TR_AssuraLVS_VirtualJoinedPinList ) && d_instTerm->net->name != nil
				then
					TR_AssuraLVS_VirtualJoinedPinList = cons( d_instTerm->net->name TR_AssuraLVS_VirtualJoinedPinList )
			)			
		)
	); end foreach d_Selected l_d_Selected

	if( runPartialVirtual
		isContinue = TR_GenerateVirtualJoinedPin( )
	); end if

	if( isContinue
		then

	foreach( d_Selected l_d_Selected	
		
		if( d_Selected->objType == "inst" && d_Selected->viewName != "symbolic"
			then

				DebugOn && printf( "DrawPinsLabels --> Start cell \"%s\" \"%s\" \n" d_Selected->cellName d_Selected->name )

				l_BlackBoxCells = cons( d_Selected->cellName l_BlackBoxCells )

				x_inst = car( d_Selected->xy )
				y_inst = cadr( d_Selected->xy )



				; Go thru each instTerms (IO pins)
				foreach( d_instTerm d_Selected->instTerms

					DebugOn && printf( "DrawPinsLabels --> --> Start instTerms = %s -> %s \n" d_instTerm->name d_instTerm->net->name )
					
					; Process only if user wants to virtually join the pin
					if( member( d_instTerm->net->name TR_AssuraLVS_VirtualJoinedPinList )
						then

					foreach( d_instTermPin d_instTerm->term->pins

					; Filter out Symbolic Pins
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
							
                                        case( d_Selected->orient

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
					if( DrawPin
						then
							PinLayer = d_instTermPin->fig->layerName

							; First, draw the pin
							d_Pin = dbCreateRect( geGetWindowCellView( ) list( PinLayer PinPurpose ) bBox_instTerm )

							; Second, Fill up Connectivity
							d_Net = dbMakeNet( geGetWindowCellView() d_instTerm->net->name )
							dbCreatePin( d_Net d_Pin )
					); end if


					if( DrawLabel
						then
							LabelLayer = d_instTermPin->fig->layerName

							d_Label = dbCreateLabel( 
								geGetWindowCellView( )
								list( LabelLayer LabelPurpose )
								xy_instTerm
								sprintf( nil "%s:1" d_instTerm->net->name )
								"centerCenter"
								"R0"
								"stick"
								0.05
								)
					);end if

					); end if filter out inst type pins

					);foreach d_instTermPin

				); end if filter for virtual Pins

				); foreach instTerm

				DebugOn && printf( "DrawPinsLabels --> Finish Cell \"%s\" \"%s\" \n"  d_Selected->cellName d_Selected->name )

			); end if



			; Deal with IO pins on the top level
			; Jan 27 2003
			; Now deals with "path "polygon" and "rect"
			; =========================================
			if( 	member( d_Selected->objType list( "rect" "path" "polygon")) && 
				member( d_Selected->net->name TR_AssuraLVS_VirtualJoinedPinList )

				then
					if( member( d_Selected->objType list( "rect" ))
						then
							bBox_instTerm   = d_Selected->bBox
							xy_instTerm	= car( d_Selected->bBox )
						else
							bBox_instTerm   = list( 
										list(
											car( car( d_Selected->points ) ) - 0.01
											cadr( car( d_Selected->points ) ) - 0.01
											)
										list(
											car( car( d_Selected->points ) ) + 0.01
											cadr( car( d_Selected->points ) ) + 0.01
											)
										)
							xy_instTerm	= car( d_Selected->points )
					); end if


					if( DrawPin
						then
							PinLayer = d_Selected->layerName
		
							; First, draw the pin
							d_Pin = dbCreateRect( geGetWindowCellView( ) list( PinLayer PinPurpose ) bBox_instTerm )

							; Second, Fill up Connectivity
							d_Net = dbMakeNet( geGetWindowCellView() d_Selected->net->name )
							dbCreatePin( d_Net d_Pin )
					); end if


					if( DrawLabel
						then
							LabelLayer = d_Selected->layerName

							d_Label = dbCreateLabel( 
								geGetWindowCellView( )
								list( LabelLayer LabelPurpose )
								xy_instTerm
								sprintf( nil "%s:1" d_Selected->net->name )
								"centerCenter"
								"R0"
								"stick"
								0.05
								)
					);end if

			); end if

			
	); end foreach


	; =============================================
	; Restore the LSW Window to the correct settings
	; =============================================
	leSetPinSelectable( pin_Selectable )
	leSetInstSelectable( inst_Selectable )
	foreach( lpp lpp_List

		leSetLayerSelectable( car(lpp) cadr(lpp) )
		leSetLayerVisible( car(lpp) caddr(lpp) )

	); end foreach




	;=====================================================================================================
	; Removing Duplicate Entries in the BlackBox Cell List
	;=====================================================================================================

	l_BlackBoxCells_temp 		= sort( l_BlackBoxCells `alphalessp )
	l_BlackBoxCells 		= cons( car( l_BlackBoxCells_temp ) nil )
	Previous_BlackBoxCells_temp 	= car( l_BlackBoxCells_temp )

	foreach( BlackBoxCells_temp l_BlackBoxCells_temp
		
		if( BlackBoxCells_temp != Previous_BlackBoxCells_temp
			then
				l_BlackBoxCells = cons( BlackBoxCells_temp l_BlackBoxCells )	
		);end if

		Previous_BlackBoxCells_temp 	= BlackBoxCells_temp

	);end foreach

	println( l_BlackBoxCells )

	); end if isContinue

	;=====================================================================================================
	; Return Value
	;=====================================================================================================

	l_BlackBoxCells

); end let

);end procedure



; ============================================================================================================
; ============================================================================================================
; This is the procedure that run the Virtual LVS
; This will operate on the current cell you opened
; Requirements
; 1. Netlist located at ./netlist/[cellname].cdl
; 2. You have correct setup for "extract.rul" "compare.rul" "bind.rul" and "switch"
; 3. It always use /scratch/$USER_VLVS as the working directory


procedure( AssuraLVS_31( @key 	( runVirtual 		nil ) 
				( runBlackBox 		nil ) 
				( runPartialVirtual 	nil )
				( VLVSViewName 		"layout.VLVS" )
				( DebugOn		nil )
				)

	let( ( 	l_BlackBoxCells
		VLVS_ReplayFile 
		scratchdir
		isContinue)


	; Echo Function Call
	; ==================
	DebugOn && printf( "AssuraLVS_31 --> AssuraLVS( ?runVirtual %L ?runBlackBox %L ?runPartialVirtual %L ?VLVSViewName %L )\n"
				runVirtual
				runBlackBox
				runPartialVirtual
				VLVSViewName )


	isContinue	  = t
;	VLVSViewName 	  = "layout.VLVS"
        
        ; find the proper grid TMP dir if it is set
        scratchdir = getShellEnvVar( "TMP" )
        if( ! scratchdir then scratchdir="/scratch" )
	VLVS_runDir	  = sprintf( nil "%s/%s_VLVS/" scratchdir getShellEnvVar( "USER" ))
	Original_viewName = geGetWindowCellView()->viewName
 
	; Ripped off unused/unwanted stuff
	sh( sprintf( nil "rm -rf %s" VLVS_runDir ))

	; =================================================================================
	; Create the VLVS_Layout and run Virtual LVS
	; =================================================================================

	if( runVirtual
		then
			;Backup Current CellView

			geSaveAs( 	geGetCellViewWindow( geGetWindowCellView( ) )
					geGetWindowCellView( )->libName 
					geGetWindowCellView( )->cellName 
					VLVSViewName 
					)



			geOpen( ?window 	geGetCellViewWindow( geGetWindowCellView( ) )
				?lib 		geGetWindowCellView( )->libName 
				?cell 		geGetWindowCellView( )->cellName
				?view 		VLVSViewName
				?viewType 	"maskLayout"
				?mode 		"a" )


			l_BlackBoxCells = DrawPinsLabels( ?runPartialVirtual runPartialVirtual )

			geSave( )

	); end if runVirtual




	; =================================================================================
	; Create "lvs.<cellName>.state" File used for LVS
	; =================================================================================

	TR_AssuraLVS_cellName			= geGetWindowCellView( )->cellName
	TR_AssuraLVS_Variable_runName		= TR_AssuraLVS_cellName
	TR_AssuraLVS_Variable_runDir		= VLVS_runDir
	TR_AssuraLVS_Variable_technology	= "Assura_tsmc13lg"
	TR_AssuraLVS_Variable_layLibrary	= geGetWindowCellView( )->libName
	TR_AssuraLVS_Variable_layCell		= TR_AssuraLVS_cellName
	TR_AssuraLVS_Variable_layView		= geGetWindowCellView( )->viewName
	TR_AssuraLVS_Variable_schCellName	= TR_AssuraLVS_cellName
	TR_AssuraLVS_Variable_schNetlists	= sprintf( nil "%s/%s.cdl" TR_AssuraLVS_NetlistPath geGetWindowCellView()->cellName )
	TR_AssuraLVS_Variable_blackBoxCell	= TR_AssuraLVS_VLVS_BlackBoxFile

	TR_AssuraLVS_ProcGenerateLVSStateFile( )


	; =================================================================================
	; Deals with Blackboxing
	; =================================================================================

	if( l_BlackBoxCells && runBlackBox
		then
			isContinue = TR_GenerateBlackBoxFile( l_BlackBoxCells )
	)



        ; =================================================================================
        ; Create the VLVS_ReplayFile
        ; =================================================================================

        if( l_BlackBoxCells && isContinue
                then

		        VLVS_ReplayFile = TR_AssuraLVS_VLVS_ReplayFile

		        outPort = outfile( VLVS_ReplayFile ) 

		        fprintf( outPort "deInstallApp(getCurrentWindow() \"Assura\")\n" )

		        fprintf( outPort "vuiLVSRun()\n" )
			fprintf( outPort "hiiSetCurrentForm('vuiLVSForm)\n" )

			fprintf( outPort "_vuiStateCB( \"LVS\" 'load )\n" )
			fprintf( outPort "hiiSetCurrentForm('vuiLVSstateForm)\n" )
			fprintf( outPort "vuiLVSstateForm->stateList->value = '(\"%s\" )\n" geGetWindowCellView( )->cellName )
			fprintf( outPort "hiFormDone(vuiLVSstateForm)\n" )

			fprintf( outPort "hiiSetCurrentForm('vuiLVSForm)\n" )
			fprintf( outPort "hiFormDone(vuiLVSForm)\n" )

			close( outPort )

			hiReplayFile( TR_AssuraLVS_VLVS_ReplayFile )
	); end if

	; =================================================================================
	; Wait for VLVS Result
	; =================================================================================

	if( runVirtual && isContinue && l_BlackBoxCells
		then

			TR_AssuraLVS_DialogBox = hiDisplayAppDBox( 
				?name 		'TR_AssuraLVS_DialogBox 
				?dboxText 	"Does your Virtual LVS Pass?" 
				?dialogType 	hicQuestionDialog
				?dialogStyle 	'modal 
				?buttonLayout 	'YesNo 
				)


			if( TR_AssuraLVS_DialogBox
				then
					geOpen( ?window 	geGetCellViewWindow( geGetWindowCellView( ) )
						?lib 		geGetWindowCellView( )->libName 
						?cell 		geGetWindowCellView( )->cellName
						?view 		Original_viewName
						?viewType 	"maskLayout"
						?mode 		"a" )

					ddDeleteLocal( ddGetObj(	geGetWindowCellView( )->libName
									geGetWindowCellView( )->cellName 
									VLVSViewName )  )

				else
					println( "Do your own debugging" )

			);end if

	); end if runVirtual

	);end let

); end procedure

; CC221
; ====================================================================================================
; This procedure is used to generate the blackboxed file used for VLVS
;	Users are prompted to input "filename" and "cellnames" he/she wanna to blackboxed
; ====================================================================================================

procedure( TR_GenerateBlackBoxFile( l_BlackBoxCells @key ( DebugOn nil ) )


	let((	
		BlackBoxCells
		BlackBoxCells_String
		) 


	BlackBoxCells_String = ""

	foreach( BlackBoxCells l_BlackBoxCells

		BlackBoxCells_String = sprintf( nil "%s%s\n" BlackBoxCells_String BlackBoxCells )

	); end foreach

	TR_GenerateBlackBoxFile_TextWindowField = hiCreateMLTextField( 
				?name 		`TR_GenerateBlackBoxFile_TextWindowField
				?prompt 	""
				?defValue 	BlackBoxCells_String	 
				)

	TR_GenerateBlackBoxFile_LoadButtonField = hiCreateButton(
				?name		`TR_GenerateBlackBoxFile_LoadButtonField
				?buttonText	"--LOAD FILE--"
				?callback	"TR_GenerateBlackBoxFile_LoadButtonField_CB( hiGetCurrentForm() )"
				)

	TR_GenerateBlackBoxFile_SaveButtonField = hiCreateButton(
				?name		`TR_GenerateBlackBoxFile_SaveButtonField
				?buttonText	"--SAVE FILE--"
				?callback	"TR_GenerateBlackBoxFile_SaveButtonField_CB( hiGetCurrentForm() )"
				)

        TR_GenerateBlackBoxFile_Form = hiCreateAppForm( 
                ?name 		'TR_GenerateBlackBoxFile_Form
		?initialSize  	list( 600 1000 )
                ?fields  	list( 
					list(  TR_GenerateBlackBoxFile_TextWindowField 0:0     600:800 0 )
					list(  TR_GenerateBlackBoxFile_LoadButtonField 0:800   200:30  0 )
					list(  TR_GenerateBlackBoxFile_SaveButtonField 200:800 200:30  0 )
				)
		?formTitle	""
        )

	hiDisplayForm( TR_GenerateBlackBoxFile_Form )

	); end let

); end procedure



;=============
; LOAD
;=============

procedure( TR_GenerateBlackBoxFile_LoadButtonField_CB( theForm )

	; Local Variables
	let((	inPort
		BlackBoxCells_String 
		word
		)

		BlackBoxCells_String = ""
		
		; Global Variable
		; TR_AssuraLVS_VLVS_BlackBoxFile

		inPort = infile( TR_AssuraLVS_VLVS_BlackBoxFile ) 

		when( inPort 
			while( fscanf( inPort "%s\n" word ) 
				BlackBoxCells_String = sprintf( nil "%s%s\n" BlackBoxCells_String word )
				) 
		);end when

		close( inPort ) 

		theForm->TR_GenerateBlackBoxFile_TextWindowField->value = BlackBoxCells_String

	); end let

); end procedure


;=============
; SAVE
;=============

procedure( TR_GenerateBlackBoxFile_SaveButtonField_CB( theForm )

	; Local Variables
	let((	outPort
		BlackBoxCells_String 
		word
		)

		BlackBoxCells_String = theForm->TR_GenerateBlackBoxFile_TextWindowField->value
		
		; Global Variable
		; TR_AssuraLVS_VLVS_BlackBoxFile

		outPort = outfile( TR_AssuraLVS_VLVS_BlackBoxFile ) 

		fprintf( outPort "%s" BlackBoxCells_String )

		close( outPort ) 

	); end let

); end procedure




















; ====================================================================================================
; This procedure is used to generate all the virtual joined pins for VLVS
;	Users are prompted to check pins he/she wanna to virtually joined
; ====================================================================================================
procedure( TR_GenerateVirtualJoinedPin( @key ( DebugOn nil ) )


	let((	; l_Cells_ChoiceString
		; l_Cells_DefaultString
		; Counter
		; NumRows
		) 

	;Global 
	;TR_AssuraLVS_VirtualJoinedPinList	

	TR_AssuraLVS_VirtualJoinedPinList = sort( TR_AssuraLVS_VirtualJoinedPinList `alphalessp )

	TR_AssuraLVS_VirtualJoinedPinListString = ""

	foreach( Pin TR_AssuraLVS_VirtualJoinedPinList

		TR_AssuraLVS_VirtualJoinedPinListString = sprintf( nil, "%s%s\n" TR_AssuraLVS_VirtualJoinedPinListString Pin )

	); end foreach

	TR_GenerateVirtualJoinedPin_TextWindowField = hiCreateMLTextField( 
				?name 		`TR_GenerateVirtualJoinedPin_TextWindowField
				?prompt 	""
				?defValue 	TR_AssuraLVS_VirtualJoinedPinListString	 
				)

	TR_GenerateVirtualJoinedPin_TextWindowRevertButtonField = hiCreateButton(
				?name		`TR_GenerateVirtualJoinedPin_TextWindowRevertButtonField
				?buttonText	"Load File"
				?callback	"TR_GenerateVirtualJoinedPin_TextWindowRevertButtonField( hiGetCurrentForm() )"
				)	

	TR_GenerateVirtualJoinedPin_TextWindowUpdateButtonField = hiCreateButton(
				?name		`TR_GenerateVirtualJoinedPin_TextWindowUpdateButtonField
				?buttonText	"Save File"
				?callback	"TR_GenerateVirtualJoinedPin_TextWindowUpdateButtonField( hiGetCurrentForm() )"
				)

	TR_GenerateVirtualJoinedPin_RemoveNetStringField = hiCreateStringField(
				?name		`TR_GenerateVirtualJoinedPin_RemoveNetStringField
				?prompt		""
				?defValue	""
				)	

	TR_GenerateVirtualJoinedPin_RemoveNetButtonField = hiCreateButton(
				?name		`TR_GenerateVirtualJoinedPin_RemoveNetButtonField
				?buttonText	"Remove Net"
				?callback	"TR_GenerateVirtualJoinedPin_RemoveNetButtonField( hiGetCurrentForm() )"
				)	

	TR_GenerateVirtualJoinedPin_AddNetStringField = hiCreateStringField(
				?name		`TR_GenerateVirtualJoinedPin_AddNetStringField
				?prompt		""
				?defValue	""
				)	

	TR_GenerateVirtualJoinedPin_AddNetButtonField = hiCreateButton(
				?name		`TR_GenerateVirtualJoinedPin_AddNetButtonField
				?buttonText	"Add Net"
				?callback	"TR_GenerateVirtualJoinedPin_AddNetButtonField( hiGetCurrentForm() )"
				)	


        TR_GenerateVirtualJoinedPin_Form = hiCreateAppForm( 
                ?name 		'TR_GenerateVirtualJoinedPin_Form
		?initialSize  	list( 600 1000 )
                ?fields  	list( 
					list( TR_GenerateVirtualJoinedPin_TextWindowField         0:0     600:600 0 )

					list( TR_GenerateVirtualJoinedPin_TextWindowRevertButtonField   0:600     300:30 0 )
					list( TR_GenerateVirtualJoinedPin_TextWindowUpdateButtonField 300:600     300:30 0 )


					list( TR_GenerateVirtualJoinedPin_RemoveNetStringField    0:630   200:30  0 )
					list( TR_GenerateVirtualJoinedPin_RemoveNetButtonField  200:630   100:30  0 )
					list( TR_GenerateVirtualJoinedPin_AddNetStringField     300:630   200:30  0 )
					list( TR_GenerateVirtualJoinedPin_AddNetButtonField     500:630   100:30  0 )
				)
        )

	hiDisplayForm( TR_GenerateVirtualJoinedPin_Form )
	
	); end let

); end procedure


procedure( TR_GenerateVirtualJoinedPin_RemoveNetButtonField( theForm )


	TR_AssuraLVS_VirtualJoinedPinList = setof( 	x 
							TR_AssuraLVS_VirtualJoinedPinList 
							x != theForm->TR_GenerateVirtualJoinedPin_RemoveNetStringField->value
							)

	TR_AssuraLVS_VirtualJoinedPinListString = ""

	foreach( Pin TR_AssuraLVS_VirtualJoinedPinList

		TR_AssuraLVS_VirtualJoinedPinListString = sprintf( nil, "%s%s\n" TR_AssuraLVS_VirtualJoinedPinListString Pin )

	); end foreach

	hiDeleteField( theForm `TR_GenerateVirtualJoinedPin_TextWindowField )

	TR_GenerateVirtualJoinedPin_TextWindowField = hiCreateMLTextField( 
				?name 		`TR_GenerateVirtualJoinedPin_TextWindowField
				?prompt 	""
				?defValue 	TR_AssuraLVS_VirtualJoinedPinListString	 
				)

	hiAddField( theForm list( TR_GenerateVirtualJoinedPin_TextWindowField 0:0 600:600 0 ))

); end procedure


procedure( TR_GenerateVirtualJoinedPin_AddNetButtonField( theForm )

	if( !member( theForm->TR_GenerateVirtualJoinedPin_AddNetStringField->value TR_AssuraLVS_VirtualJoinedPinList )
		then

			TR_AssuraLVS_VirtualJoinedPinList = cons( 	theForm->TR_GenerateVirtualJoinedPin_AddNetStringField->value 
									TR_AssuraLVS_VirtualJoinedPinList 
									)	


	); end if

	TR_AssuraLVS_VirtualJoinedPinList = sort( TR_AssuraLVS_VirtualJoinedPinList `alphalessp )

	TR_AssuraLVS_VirtualJoinedPinListString = ""

	foreach( Pin TR_AssuraLVS_VirtualJoinedPinList

		TR_AssuraLVS_VirtualJoinedPinListString = sprintf( nil, "%s%s\n" TR_AssuraLVS_VirtualJoinedPinListString Pin )

	); end foreach

	hiDeleteField( theForm `TR_GenerateVirtualJoinedPin_TextWindowField )

	TR_GenerateVirtualJoinedPin_TextWindowField = hiCreateMLTextField( 
				?name 		`TR_GenerateVirtualJoinedPin_TextWindowField
				?prompt 	""
				?defValue 	TR_AssuraLVS_VirtualJoinedPinListString	 
				)

	hiAddField( theForm list( TR_GenerateVirtualJoinedPin_TextWindowField 0:0 600:600 0 ))

); end procedure


procedure( TR_GenerateVirtualJoinedPin_TextWindowRevertButtonField( theForm )

	TR_AssuraLVS_VirtualJoinedPinListString = ""

	foreach( Pin TR_AssuraLVS_VirtualJoinedPinList

		TR_AssuraLVS_VirtualJoinedPinListString = sprintf( nil, "%s%s\n" TR_AssuraLVS_VirtualJoinedPinListString Pin )

	); end foreach

	hiDeleteField( theForm `TR_GenerateVirtualJoinedPin_TextWindowField )

	TR_GenerateVirtualJoinedPin_TextWindowField = hiCreateMLTextField( 
				?name 		`TR_GenerateVirtualJoinedPin_TextWindowField
				?prompt 	""
				?defValue 	TR_AssuraLVS_VirtualJoinedPinListString	 
				)

	hiAddField( theForm list( TR_GenerateVirtualJoinedPin_TextWindowField 0:0 600:600 0 ))

); end procedure

procedure( TR_GenerateVirtualJoinedPin_TextWindowUpdateButtonField( theForm )

	TR_AssuraLVS_VirtualJoinedPinList = parseString( theForm->TR_GenerateVirtualJoinedPin_TextWindowField->value "\n" )

	TR_AssuraLVS_VirtualJoinedPinList = sort( TR_AssuraLVS_VirtualJoinedPinList `alphalessp )

	TR_AssuraLVS_VirtualJoinedPinListString = ""

	foreach( Pin TR_AssuraLVS_VirtualJoinedPinList

		TR_AssuraLVS_VirtualJoinedPinListString = sprintf( nil, "%s%s\n" TR_AssuraLVS_VirtualJoinedPinListString Pin )

	); end foreach

	hiDeleteField( theForm `TR_GenerateVirtualJoinedPin_TextWindowField )

	TR_GenerateVirtualJoinedPin_TextWindowField = hiCreateMLTextField( 
				?name 		`TR_GenerateVirtualJoinedPin_TextWindowField
				?prompt 	""
				?defValue 	TR_AssuraLVS_VirtualJoinedPinListString	 
				)

	hiAddField( theForm list( TR_GenerateVirtualJoinedPin_TextWindowField 0:0 600:600 0 ))

); end procedure


procedure( TR_GenerateVirtualJoinedPin_TextWindowUpdateButtonField( theForm )

	let((	outPort
		inPort
		VLVS_PinFile
		word
		)
;	drain()

	VLVS_PinFile = "VLVS_PinFile.txt"


	outPort = outfile( VLVS_PinFile )

	fprintf( outPort theForm->TR_GenerateVirtualJoinedPin_TextWindowField->value )

	close( outPort )


	TR_AssuraLVS_VirtualJoinedPinList = nil

	inPort = infile( VLVS_PinFile ) 

	when( inPort 
		while( fscanf( inPort "%s\n" word ) 
			TR_AssuraLVS_VirtualJoinedPinList = cons( word TR_AssuraLVS_VirtualJoinedPinList ) 
			) 
	);end when

	close( inPort ) 






	; DUMB stuff

	TR_AssuraLVS_VirtualJoinedPinList = sort( TR_AssuraLVS_VirtualJoinedPinList `alphalessp )

	TR_AssuraLVS_VirtualJoinedPinListString = ""

	foreach( Pin TR_AssuraLVS_VirtualJoinedPinList

		TR_AssuraLVS_VirtualJoinedPinListString = sprintf( nil, "%s%s\n" TR_AssuraLVS_VirtualJoinedPinListString Pin )

	); end foreach

	hiDeleteField( theForm `TR_GenerateVirtualJoinedPin_TextWindowField )

	TR_GenerateVirtualJoinedPin_TextWindowField = hiCreateMLTextField( 
				?name 		`TR_GenerateVirtualJoinedPin_TextWindowField
				?prompt 	""
				?defValue 	TR_AssuraLVS_VirtualJoinedPinListString	 
				)

	hiAddField( theForm list( TR_GenerateVirtualJoinedPin_TextWindowField 0:0 600:600 0 ))



	); end let

); end procedure



procedure( TR_GenerateVirtualJoinedPin_TextWindowRevertButtonField( theForm )

	let((	outPort
		inPort
		VLVS_PinFile
		word
		)

	VLVS_PinFile = "VLVS_PinFile.txt"

	TR_AssuraLVS_VirtualJoinedPinList = nil

;	drain()

	inPort = infile( VLVS_PinFile ) 

	when( inPort 
		while( fscanf( inPort "%s\n" word ) 
			TR_AssuraLVS_VirtualJoinedPinList = cons( word TR_AssuraLVS_VirtualJoinedPinList ) 
			) 
	);end when

	close( inPort ) 



	; DUMB stuff

	TR_AssuraLVS_VirtualJoinedPinList = sort( TR_AssuraLVS_VirtualJoinedPinList `alphalessp )

	TR_AssuraLVS_VirtualJoinedPinListString = ""

	foreach( Pin TR_AssuraLVS_VirtualJoinedPinList

		TR_AssuraLVS_VirtualJoinedPinListString = sprintf( nil, "%s%s\n" TR_AssuraLVS_VirtualJoinedPinListString Pin )

	); end foreach

	hiDeleteField( theForm `TR_GenerateVirtualJoinedPin_TextWindowField )

	TR_GenerateVirtualJoinedPin_TextWindowField = hiCreateMLTextField( 
				?name 		`TR_GenerateVirtualJoinedPin_TextWindowField
				?prompt 	""
				?defValue 	TR_AssuraLVS_VirtualJoinedPinListString	 
				)

	hiAddField( theForm list( TR_GenerateVirtualJoinedPin_TextWindowField 0:0 600:600 0 ))



	); end let

); end procedure



; ====================================================================================================
; This procedure is used to generate the "lvs.<cellName>.state" used by the LVS GUI 
; ====================================================================================================
procedure( TR_AssuraLVS_ProcGenerateLVSStateFile( @key 	( DebugOn 	nil ) 
							( AssuraVersion	3.1 )
							)

	let((	
		outPort
		inPort
		line
		LVSParameter
		)

	outPort	= outfile( sprintf( nil ".lvs.%s.state" TR_AssuraLVS_cellName ))
;	inPort	= infile( "/home/user/stevemc/cadence/skill/TR_AssuraLVS.lvs.state" )
        inPort = infile( sprintf( nil "%s/share/skill/layout/vlvs/vlvs.state.template" PackageGetPackageRoot() ) )

	when( inPort 
	  while( gets( line inPort )


		rexCompile( "\\$\\$" )

		if( rexExecute( line )
		  then
		  foreach( LVSParameter TR_AssuraLVS_LVSParameterList

			DebugOn && printf( "TR_AssuraLVS_ProcGenerateLVSStateFile --> Matching Variable \"$$%s\"\n" LVSParameter )
			rexCompile( sprintf( nil "\\$\\$%s" LVSParameter ))

			if( rexExecute( line )
			  then
			  DebugOn && printf( "TR_AssuraLVS_ProcGenerateLVSStateFile --> --> \"$$%s\" Matched\n" LVSParameter )
			  line = rexReplace( line evalstring( sprintf( nil "TR_AssuraLVS_Variable_%s" LVSParameter )) 0)
			else
			  DebugOn && printf( "TR_AssuraLVS_ProcGenerateLVSStateFile --> --> \"$$%s\" Failed Matching\n" LVSParameter )
			); end if

	  	  ); end foreach
		); end if

		fprintf( outPort "%s" line )


	  ); end while
	);end when

	close( inPort ) 
	close( outPort )

	); end let

); end procedure











; ====================================================================================================
; This procedure is the main setup screen
; ====================================================================================================

procedure( TR_AssuraLVS( )

	let( (	TR_GenerateBlackBoxFile_FilenameField

		)

        TR_AssuraLVS_NetlistField = hiCreateStringField( 
                ?name           'TR_AssuraLVS_NetlistField
                ?prompt         "Default Netlist Path"
                ?defValue       TR_AssuraLVS_NetlistPath
		?enabled	t
                )

        TR_GenerateBlackBoxFile_FilenameField = hiCreateStringField( 
                ?name           'TR_GenerateBlackBoxFile_FilenameField
                ?prompt         "Enter Filename for Blackboxing"
                ?defValue       TR_AssuraLVS_VLVS_BlackBoxFile
		?enabled	t		
                )

        TR_AssuraLVS_VLVSViewNameField = hiCreateStringField( 
                ?name           'TR_AssuraLVS_VLVSViewNameField
                ?prompt         "Enter ViewName for VLVS"
                ?defValue       "layout.VLVS"
                )
	TR_AssuraLVS_StartVLVS_31 = hiCreateButton( 
		?name           'TR_AssuraLVS_StartVLVS_31
		?buttonText     "Assura VLVS 3.1" 
		?callback       "AssuraLVS_31( 	?runVirtual 		t 
						?runBlackBox 		t 
						?runPartialVirtual 	t
						?VLVSViewName 		TR_AssuraLVS_VLVSViewNameField->value)"
	)

	TR_AssuraLVS_Separator1 = hiCreateSeparatorField( 
		?name 		`TR_AssuraLVS_Separator1
	)

        TR_AssuraLVS_Form = hiCreateAppForm( 
                ?name 		'TR_AssuraLVS_Form
		?initialSize  	list( 1000 600 )
                ?fields  	list( 
					list( TR_AssuraLVS_NetlistField 		  0:0  600:30 200 )
					list( TR_GenerateBlackBoxFile_FilenameField 	  0:30 600:30 200 )
					list( TR_AssuraLVS_VLVSViewNameField 	          0:60 600:30 200 )
					list( TR_AssuraLVS_Separator1			  0:94 600:0  0   )

					list( TR_AssuraLVS_StartVLVS_31 		  0:100 300:30 0   )

				)
		?formTitle	"Cadence Assura Fulcrum Interface"
        )

	hiDisplayForm( TR_AssuraLVS_Form )

	); end let

); end procedure

