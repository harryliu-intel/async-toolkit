; Global Variable Used:
; 1. TR_Floorplanner_CellGroup_%n (use to display result after analysis)
; 2. TR_Floorplanner_CellGroup_x TR_Floorplanner_CellGroup_y (Form Coordinates)

TR_Floorplanner_CellGroup_x = 0
TR_Floorplanner_CellGroup_y = 0
TR_Floorplanner_DatapathThreshold = 0.4
TR_Floorplanner_Counter_Data = 0
TR_Floorplanner_Counter_Ctrl = 0
TR_Floorplanner_DatapathHeight = 0
TR_Floorplanner_BitPitch = 0
TR_Floorplanner_NumBits = 0
TR_Floorplanner_WorkOnAll = t
TR_Floorplanner_ScanLine = nil

; Set Reference Point for the form to (x,y)=(0,0) from the left top corner
hiSetFormPosition( 0:0 )

; ====================================================================================================
; Main Function Dialog
; ====================================================================================================
procedure( TR_Floorplanner(  )

        let( (  
		TR_Floorplanner_LeafResolutionX TR_Floorplanner_LeafResolutionY
		TR_Floorplanner_MidResolutionX TR_Floorplanner_MidResolutionY
		TR_Floorplanner_AutoSaveField
		TR_Floorplanner_StartCompaction	
		TR_Floorplanner_WorkOnAllField
		TR_Floorplanner_ScanLineField
		TR_Floorplanner_DatapathThresholdField
		TR_Floorplanner_StartAnalysis
		TR_Floorplanner_Separator1 
		)

	TR_Floorplanner_CellGroup_x = 0
	TR_Floorplanner_CellGroup_y = 0

	TR_Floorplanner_LeafResolutionX = hiCreateFloatField( 
		?name 		'TR_Floorplanner_LeafResolutionX
		?prompt 	"Leaf Cell xSnap(um)" 
		?defValue	0.24
		)

	TR_Floorplanner_LeafResolutionY = hiCreateFloatField( 
		?name 		'TR_Floorplanner_LeafResolutionY
		?prompt 	"Leaf Cell ySnap(um)" 
		?defValue	2*PowerGridPitch
		?enabled	t
		)

	TR_Floorplanner_MidResolutionX = hiCreateFloatField( 
		?name 		'TR_Floorplanner_MidResolutionX
		?prompt 	"Mid Cell xSnap(um)" 
		?defValue	PowerGridPitch
		)

	TR_Floorplanner_MidResolutionY = hiCreateFloatField( 
		?name 		'TR_Floorplanner_MidResolutionY
		?prompt 	"Mid Cell ySnap(um)" 
		?defValue	2*PowerGridPitch
		?enabled	t
		)

	TR_Floorplanner_AutoSaveField = hiCreateToggleField( 
		?name 		'TR_Floorplanner_AutoSaveField
		?choices	list( list( `AutoSave "AutoSave" ))
		?prompt 	"" 
		?defValue	list( nil )
		?enabled	t
		)


	TR_Floorplanner_CompactionDepthField = hiCreateScaleField( 
		?name 		`TR_Floorplanner_CompactionDepthField
		?prompt 	"Compaction Depth (\"-1\" = Current Cell Only)" 
		?isContinuous 	t 
		?range 		list( -1 32 ) 
		?defValue 	-1 
		?enabled 	t 
		)

	TR_Floorplanner_StartCompactionField = hiCreateButton( 
		?name 		'TR_Floorplanner_StartCompactionField
		?buttonText 	"Compaction" 
		?callback 	"TR_Floorplanner_StartCompactionField_CB( hiGetCurrentForm() )"
		)

        TR_Floorplanner_WorkOnAllField = hiCreateRadioField( 
                ?name           'TR_Floorplanner_WorkOnAllField
                ?choices        list( "Selected Objects Only" "All Objects" ) 
                ?prompt         "Work on"
                ?defValue       "All Objects"
		?callback 	list( "TR_Floorplanner_WorkOnAllField_CB( hiGetCurrentForm() )" ) 
                )

        TR_Floorplanner_ScanLineField = hiCreateRadioField( 
                ?name           'TR_Floorplanner_ScanLineField
                ?choices        list( "Yes" "No" ) 
                ?prompt         "Use Scan Line"
                ?defValue       "No"
		?callback 	list( "TR_Floorplanner_ScanLineField_CB( hiGetCurrentForm() )" ) 
                )

	TR_Floorplanner_DatapathHeightField = hiCreateFloatField( 
		?name 		'TR_Floorplanner_DatapathHeightField
		?prompt 	"Datapath Height (um)" 
		?defValue	float( TR_Floorplanner_DatapathHeight )
		?callback 	"TR_Floorplanner_DatapathHeightField_CB( hiGetCurrentForm() )"
		)

	TR_Floorplanner_BitPitchField = hiCreateFloatField( 
		?name 		'TR_Floorplanner_BitPitchField
		?prompt 	"            Bit Pitch (um)" 
		?defValue	float( TR_Floorplanner_BitPitch )
		?callback 	"TR_Floorplanner_BitPitchField_CB( hiGetCurrentForm() )"
		)

	TR_Floorplanner_NumBitsField = hiCreateIntField( 
		?name 		'TR_Floorplanner_NumBitsField
		?prompt 	"          Number of Bits" 
		?defValue	TR_Floorplanner_NumBits
		?callback 	"TR_Floorplanner_NumBitsField_CB( hiGetCurrentForm() )"
		)

	TR_Floorplanner_DatapathThresholdField = hiCreateScaleField( 
		?name 		`TR_Floorplanner_DatapathThresholdField
		?prompt 	"Datapath Threshold (%)" 
		?callback 	"TR_Floorplanner_DatapathThresholdField_CB( hiGetCurrentForm() )" 
		?isContinuous 	t 
		?range 		list( 20 80 ) 
		?defValue 	40 
		?enabled 	t 
		)

	TR_Floorplanner_StartAnalysis = hiCreateButton( 
		?name 		'TR_Floorplanner_StartAnalysis
		?buttonText 	"Analysis" 
		?callback 	"TR_Floorplanner_StartAnalysis_CB( hiGetCurrentForm() )"
		)

        TR_Floorplanner_Separator1 = hiCreateSeparatorField( 
                ?name           `TR_Floorplanner_Separator1
        )

        TR_Floorplanner_Separator2 = hiCreateSeparatorField( 
                ?name           `TR_Floorplanner_Separator2
        )

        TR_Floorplanner_Form = hiCreateAppForm( 
                ?name 		'TR_Floorplanner_Form
		?initialSize  list( 1000 1500 )
                ?fields  	list(
					list( TR_Floorplanner_LeafResolutionX 0:0 180:40 130 )
					list( TR_Floorplanner_LeafResolutionY 200:0 180:40 130 )

					list( TR_Floorplanner_MidResolutionX 0:40 180:40 130 )
					list( TR_Floorplanner_MidResolutionY 200:40 180:40 130 )

					list( TR_Floorplanner_AutoSaveField 500:0 100:40 00 )

					list( TR_Floorplanner_CompactionDepthField 0:75 400:40 300 )
					list( TR_Floorplanner_StartCompactionField 420:88 180:30 180 )


					list( TR_Floorplanner_WorkOnAllField 0:120 400:40 100 )
					list( TR_Floorplanner_ScanLineField 400:120 200:40 100 )

					list( TR_Floorplanner_DatapathHeightField   0:160 200:40 140 )
					list( TR_Floorplanner_BitPitchField       200:160 200:40 140 )
					list( TR_Floorplanner_NumBitsField        400:160 200:40 140 )

					list( TR_Floorplanner_DatapathThresholdField 0:200 600:50 300 )

					list( TR_Floorplanner_StartAnalysis 0:250 600:30 600 ))

;		?buttonLayout	list( `OKCancel list( `Spider\ 3\.0\.2 "TR_Floorplanner_VersionHistoryField_CB( )" ) )
;		?buttonDisabled	list( `OK `Help `Spider\ 3\.0\.2 )
		?buttonLayout	list( `OKCancel list( `SpiderVersionHistory "TR_Floorplanner_VersionHistoryField_CB( )" ) )
		?buttonDisabled	list( `OK `Help )
                ?callback 	"TR_Floorplanner_CB( hiGetCurrentForm() )" 
        )

	TR_Floorplanner_CellGroup_y = TR_Floorplanner_CellGroup_y + 280

        hiDisplayForm( TR_Floorplanner_Form )

        ); end let

); end procedure


; ====================================================================================================
; TR_Floorplanner_VersionLogField CallBack Function
; ====================================================================================================
procedure( TR_Floorplanner_VersionHistoryField_CB( )

	let(( 	TR_Floorplanner_VersionHistoryTextField
		TR_Floorplanner_VersionHistoryTextForm
		HistoryText
		)

	HistoryText =
	"\n Spider 3.0.5
	 \n -- Released May-29-2003
	 \n -- Spider will now automatically filter out 
	 \n    non-instance cells
	 \n
	 \n Spider 3.0.4
	 \n -- Released Jan-21-2003
	 \n -- Added options to auto-align cells 
	 \n    after floorplanning
	 \n
	 \n Spider 3.0.3
	 \n -- Released Jan-21-2003
	 \n -- Fixed Small bug in AnalysisLayout.il
	 \n    so that it will not match \"S1\" with \"S12\"
	 \n -- Cells are auto-aligned after Floorplanning
	 \n
	 \n Spider 3.0.2
	 \n -- Released Jan-20-2003
	 \n -- All arrays increase indicies 
	 \n    going upward (+y) or right (+x) 
	 \n -- Added Version History Support"


	TR_Floorplanner_VersionHistoryTextField = hiCreateMLTextField( 
			?name 		`TR_Floorplanner_VersionHistoryTextField
			?defValue 	HistoryText
			?editable	nil
			?enabled 	t
			)

        TR_Floorplanner_VersionHistoryTextForm = hiCreateAppForm( 
                ?name 		'TR_Floorplanner_VersionHistoryTextForm
		?initialSize  list( 500 500 )
                ?fields  	list(
					list( TR_Floorplanner_VersionHistoryTextField 0:0 500:400 100 )
					)
        )

	hiDisplayForm( TR_Floorplanner_VersionHistoryTextForm )

	t

	); end let

); end procedure



; ====================================================================================================
; TR_Floorplanner_DatapathThreshold CallBack Function
; ====================================================================================================
procedure( TR_Floorplanner_DatapathThresholdField_CB( theForm )

	TR_Floorplanner_DatapathThreshold = float( theForm->TR_Floorplanner_DatapathThresholdField->value ) / 100	

); end procedure





; ====================================================================================================
; TR_Floorplanner_StartCompaction CallBack Function
; ====================================================================================================
procedure( TR_Floorplanner_StartCompactionField_CB( theForm )

	if( theForm->TR_Floorplanner_CompactionDepthField->value >= 0
		then
			theForm->TR_Floorplanner_WorkOnAllField->value = "All Objects"
			TR_Floorplanner_WorkOnAll = t
	)

	CompactCellsResursive(  ?Depth		theForm->TR_Floorplanner_CompactionDepthField->value
				?WorkOnAll	TR_Floorplanner_WorkOnAll 
				?AlignToGrid 	list( 	theForm->TR_Floorplanner_LeafResolutionX->value:theForm->TR_Floorplanner_LeafResolutionY->value
							theForm->TR_Floorplanner_MidResolutionX->value:theForm->TR_Floorplanner_MidResolutionY->value )
				?AutoSave	theForm->TR_Floorplanner_AutoSaveField->AutoSave->value
				)

); end procedure



; ====================================================================================================
; TR_Floorplanner_WorkOnAll CallBack Function
; ====================================================================================================
procedure( TR_Floorplanner_WorkOnAllField_CB( theForm )

        if( theForm->TR_Floorplanner_WorkOnAllField->value == "All Objects"
                then
                        TR_Floorplanner_WorkOnAll = t
                else
                        TR_Floorplanner_WorkOnAll = nil
        )


); end procedure

; ====================================================================================================
; TR_Floorplanner_ScanLine CallBack Function
; ====================================================================================================
procedure( TR_Floorplanner_ScanLineField_CB( theForm )

	if( theForm->TR_Floorplanner_ScanLineField->value == "Yes"
		then
			TR_Floorplanner_ScanLine = t
		else
			TR_Floorplanner_ScanLine = nil
	)


); end procedure

; ====================================================================================================
; TR_Floorplanner_DatapathHeight CallBack Function
; ====================================================================================================
procedure( TR_Floorplanner_DatapathHeightField_CB( theForm )

	TR_Floorplanner_DatapathHeight = theForm->TR_Floorplanner_DatapathHeightField->value

); end procedure


; ====================================================================================================
; TR_Floorplanner_BitPitch CallBack Function
; ====================================================================================================
procedure( TR_Floorplanner_BitPitchField_CB( theForm )

	TR_Floorplanner_BitPitch = theForm->TR_Floorplanner_BitPitchField->value
	TR_Floorplanner_DatapathHeight = TR_Floorplanner_BitPitch * TR_Floorplanner_NumBits
	theForm->TR_Floorplanner_DatapathHeightField->value = TR_Floorplanner_DatapathHeight

); end procedure


; ====================================================================================================
; TR_Floorplanner_NumBits CallBack Function
; ====================================================================================================
procedure( TR_Floorplanner_NumBitsField_CB( theForm )

	TR_Floorplanner_NumBits = theForm->TR_Floorplanner_NumBitsField->value
	TR_Floorplanner_DatapathHeight = TR_Floorplanner_BitPitch * TR_Floorplanner_NumBits
	theForm->TR_Floorplanner_DatapathHeightField->value = TR_Floorplanner_DatapathHeight

); end procedure


; ====================================================================================================
; TR_Floorplanner_StartAnalysis CallBack Function
; ====================================================================================================
procedure( TR_Floorplanner_StartAnalysis_CB( theForm )

	let(	(
		;l_CellGroup
		l_TR_Floorplanner_CellGroup
		CleanPreviousRun
		Height_CellGroup
		)	

	CleanPreviousRun = t
	Height_CellGroup = 35

	; ====================================================================================================
	; Deleting Previous Fields
	; ====================================================================================================
	l_TR_Floorplanner_CellGroup = nil
	Counter = 1
	while( ( CleanPreviousRun && boundp( `l_CellGroup ) )

		if( 	evalstring( sprintf( nil "boundp( `TR_Floorplanner_CellGroup_%n )" Counter)) &&
			evalstring( sprintf( nil "TR_Floorplanner_CellGroup_%n->usedIn" Counter))
			then
				evalstring( sprintf( nil 
						"l_TR_Floorplanner_CellGroup = cons( `TR_Floorplanner_CellGroup_%n l_TR_Floorplanner_CellGroup )" Counter ))

				evalstring( sprintf( nil 
						"l_TR_Floorplanner_CellGroup = cons( `TR_Floorplanner_DisplayApply_CB_TypeField_%n l_TR_Floorplanner_CellGroup )" Counter ))

				evalstring( sprintf( nil 
						"l_TR_Floorplanner_CellGroup = cons( `TR_Floorplanner_DisplayApply_CB_OrderField_%n l_TR_Floorplanner_CellGroup )" Counter ))

				evalstring( sprintf( nil 
						"l_TR_Floorplanner_CellGroup = cons( `TR_Floorplanner_DisplayApply_CB_OrientationField_%n l_TR_Floorplanner_CellGroup )" Counter ))

				evalstring( sprintf( nil 
						"l_TR_Floorplanner_CellGroup = cons( `TR_Floorplanner_DisplayApply_CB_OrderHashingField_%n l_TR_Floorplanner_CellGroup )" Counter ))

				TR_Floorplanner_CellGroup_y = TR_Floorplanner_CellGroup_y - Height_CellGroup
		)

		if( Counter == length( l_CellGroup)
			then

				CleanPreviousRun = nil

		); end if
		Counter = Counter + 1


	); end while

	hiDeleteFields( theForm l_TR_Floorplanner_CellGroup )

	l_CellGroup = AnalysisLayout( 	?WorkOnAll 		TR_Floorplanner_WorkOnAll 
					?DatapathThreshold	TR_Floorplanner_DatapathThreshold 
					?ScanLine		TR_Floorplanner_ScanLine
					?DatapathHeight		TR_Floorplanner_DatapathHeight
					)		

	theForm->TR_Floorplanner_DatapathHeightField->value = TR_Floorplanner_DatapathHeight


	; ====================================================================================================
	; Create Display Options Field
	; ====================================================================================================
	; 	draw only if the variable exists OR it's not already on the GUI

	if( !boundp( `TR_Floorplanner_DisplayOptions ) || !TR_Floorplanner_DisplayOptions->usedIn
		then

		        TR_Floorplanner_DisplayOptions = hiCreateToggleField( 
		                ?name           'TR_Floorplanner_DisplayOptions
		                ?choices        list( 	list( `All  "All" )
							list( `None "None")
							list( `D3   "3-D" )
							list( `D2   "2-D" ) 
							list( `D1   "1-D" )
							list( `D0   "0-D" )
							list( `Data "Data")
							list( `Ctrl "Ctrl"))
		                ?prompt         "Display Options"
				?callback 	list( 	"TR_Floorplanner_DisplayOptions_CB_All( hiGetCurrentForm() )"
							"TR_Floorplanner_DisplayOptions_CB_None( hiGetCurrentForm() )"
							"TR_Floorplanner_DisplayOptions_CB_Else( hiGetCurrentForm() )"
							"TR_Floorplanner_DisplayOptions_CB_Else( hiGetCurrentForm() )"
							"TR_Floorplanner_DisplayOptions_CB_Else( hiGetCurrentForm() )"
							"TR_Floorplanner_DisplayOptions_CB_Else( hiGetCurrentForm() )"
							"TR_Floorplanner_DisplayOptions_CB_Else( hiGetCurrentForm() )"
							"TR_Floorplanner_DisplayOptions_CB_Else( hiGetCurrentForm() )"
							)
		                ?defValue       list( nil t nil nil nil nil nil nil )
        		        )


			TR_Floorplanner_DisplayApply = hiCreateButton( 
				?name 		'TR_Floorplanner_DisplayApply
				?buttonText 	"Refresh Display" 
				?callback 	"TR_Floorplanner_DisplayApply_CB( hiGetCurrentForm() )"
				)

			TR_Floorplanner_StartFloorplan = hiCreateButton( 
				?name 		'TR_Floorplanner_StartFloorplan
				?buttonText 	"Floorplan without Auto-alignment" 
				?callback 	"TR_Floorplanner_StartFloorplan_CB( hiGetCurrentForm() ?AutoAlign nil )"
				)

			TR_Floorplanner_StartFloorplanWithAlign = hiCreateButton( 
				?name 		'TR_Floorplanner_StartFloorplanWithAlign
				?buttonText 	"Floorplan with Auto-alignment" 
				?callback 	"TR_Floorplanner_StartFloorplan_CB( hiGetCurrentForm() ?AutoAlign t )"
				)


			hiAddFields( theForm 	list( 	list( 	TR_Floorplanner_DisplayOptions 
								TR_Floorplanner_CellGroup_x:TR_Floorplanner_CellGroup_y 
								500:30 
								120 )
							list(   TR_Floorplanner_DisplayApply
								500:TR_Floorplanner_CellGroup_y
								100:30 
								0 )
							list(   TR_Floorplanner_StartFloorplan
								TR_Floorplanner_CellGroup_x:TR_Floorplanner_CellGroup_y+30
								300:30 
								0 )
							list(   TR_Floorplanner_StartFloorplanWithAlign
								TR_Floorplanner_CellGroup_x+300:TR_Floorplanner_CellGroup_y+30
								300:30 
								0 )
							))


			TR_Floorplanner_CellGroup_y = TR_Floorplanner_CellGroup_y + 60
	)



	); end let

); end procedure




; ====================================================================================================
; Callback Function
; ====================================================================================================
procedure( TR_Floorplanner_DisplayOptions_CB_All( theForm )

	if( theForm->TR_Floorplanner_DisplayOptions->All->value
		then
			theForm->TR_Floorplanner_DisplayOptions->D3->value = t	
			theForm->TR_Floorplanner_DisplayOptions->D2->value = t	
			theForm->TR_Floorplanner_DisplayOptions->D1->value = t	
			theForm->TR_Floorplanner_DisplayOptions->D0->value = t
			theForm->TR_Floorplanner_DisplayOptions->Data->value = t	
			theForm->TR_Floorplanner_DisplayOptions->Ctrl->value = t
			theForm->TR_Floorplanner_DisplayOptions->None->value = nil
	)	

); end procedure


; ====================================================================================================
; Callback Function
; ====================================================================================================
procedure( TR_Floorplanner_DisplayOptions_CB_None( theForm )

	if( theForm->TR_Floorplanner_DisplayOptions->None->value
		then
			theForm->TR_Floorplanner_DisplayOptions->D3->value = nil	
			theForm->TR_Floorplanner_DisplayOptions->D2->value = nil	
			theForm->TR_Floorplanner_DisplayOptions->D1->value = nil	
			theForm->TR_Floorplanner_DisplayOptions->D0->value = nil
			theForm->TR_Floorplanner_DisplayOptions->Data->value = nil	
			theForm->TR_Floorplanner_DisplayOptions->Ctrl->value = nil
			theForm->TR_Floorplanner_DisplayOptions->All->value = nil
	)	

); end procedure



; ====================================================================================================
; Callback Function
; ====================================================================================================
procedure( TR_Floorplanner_DisplayOptions_CB_Else( theForm )


	if( (	theForm->TR_Floorplanner_DisplayOptions->D3->value &&
	    	theForm->TR_Floorplanner_DisplayOptions->D2->value &&	
	    	theForm->TR_Floorplanner_DisplayOptions->D1->value &&
		theForm->TR_Floorplanner_DisplayOptions->D0->value ) ||
	    (	theForm->TR_Floorplanner_DisplayOptions->Data->value &&	
	    	theForm->TR_Floorplanner_DisplayOptions->Ctrl->value )


		then

			theForm->TR_Floorplanner_DisplayOptions->All->value  = t	
			theForm->TR_Floorplanner_DisplayOptions->None->value = nil
		else
			theForm->TR_Floorplanner_DisplayOptions->All->value = nil
	)	

	if( (	theForm->TR_Floorplanner_DisplayOptions->D3->value ||
	    	theForm->TR_Floorplanner_DisplayOptions->D2->value ||	
	    	theForm->TR_Floorplanner_DisplayOptions->D1->value ||
		theForm->TR_Floorplanner_DisplayOptions->D0->value ||
	    	theForm->TR_Floorplanner_DisplayOptions->Data->value ||	
	    	theForm->TR_Floorplanner_DisplayOptions->Ctrl->value )

		then

			theForm->TR_Floorplanner_DisplayOptions->None->value = nil
		else
			theForm->TR_Floorplanner_DisplayOptions->None->value = t
			theForm->TR_Floorplanner_DisplayOptions->All->value  = nil	
	)	

); end procedure


; ====================================================================================================
; Callback Function
; ====================================================================================================
procedure( TR_Floorplanner_DisplayApply_CB( theForm )	

	let(	(
		;l_CellGroup
		l_TR_Floorplanner_CellGroup
		CleanPreviousRun
		Height_CellGroup
		)	

	CleanPreviousRun = t
	Height_CellGroup = 35


	; ====================================================================================================
	; Deleting Previous Fields
	; ====================================================================================================
	l_TR_Floorplanner_CellGroup = nil
	Counter = 1
	while( CleanPreviousRun

		if( 	evalstring( sprintf( nil "boundp( `TR_Floorplanner_CellGroup_%n )" Counter)) &&
			evalstring( sprintf( nil "TR_Floorplanner_CellGroup_%n->usedIn" Counter))
			then
				evalstring( sprintf( nil 
						"l_TR_Floorplanner_CellGroup = cons( `TR_Floorplanner_CellGroup_%n l_TR_Floorplanner_CellGroup )" Counter ))

				evalstring( sprintf( nil 
						"l_TR_Floorplanner_CellGroup = cons( `TR_Floorplanner_DisplayApply_CB_TypeField_%n l_TR_Floorplanner_CellGroup )" Counter ))

				evalstring( sprintf( nil 
						"l_TR_Floorplanner_CellGroup = cons( `TR_Floorplanner_DisplayApply_CB_OrderField_%n l_TR_Floorplanner_CellGroup )" Counter ))

				evalstring( sprintf( nil 
						"l_TR_Floorplanner_CellGroup = cons( `TR_Floorplanner_DisplayApply_CB_OrientationField_%n l_TR_Floorplanner_CellGroup )" Counter ))

				evalstring( sprintf( nil 
						"l_TR_Floorplanner_CellGroup = cons( `TR_Floorplanner_DisplayApply_CB_OrderHashingField_%n l_TR_Floorplanner_CellGroup )" Counter ))

				TR_Floorplanner_CellGroup_y = TR_Floorplanner_CellGroup_y - Height_CellGroup
		)

		if( Counter == length( l_CellGroup)
			then

				CleanPreviousRun = nil

		); end if
		Counter = Counter + 1


	); end while

	hiDeleteFields( theForm l_TR_Floorplanner_CellGroup )


	; ====================================================================================================
	; Add Current Fields
	; ====================================================================================================
	l_TR_Floorplanner_CellGroup = nil
	for( i 0 length( l_CellGroup )-1

		if( 	( caar( nth( i l_CellGroup )) == 3 && theForm->TR_Floorplanner_DisplayOptions->D3->value ) ||
 			( caar( nth( i l_CellGroup )) == 2 && theForm->TR_Floorplanner_DisplayOptions->D2->value ) ||
 			( caar( nth( i l_CellGroup )) == 1 && theForm->TR_Floorplanner_DisplayOptions->D1->value ) ||
 			( caar( nth( i l_CellGroup )) == 0 && theForm->TR_Floorplanner_DisplayOptions->D0->value ) ||
 			( caadr( nth( i l_CellGroup )) == "Data" && theForm->TR_Floorplanner_DisplayOptions->Data->value ) || 
 			( caadr( nth( i l_CellGroup )) == "Ctrl" && theForm->TR_Floorplanner_DisplayOptions->Ctrl->value )

		then


		; =============================================================================================
		; Cell Group Field
	        evalstring( sprintf( nil "TR_Floorplanner_CellGroup_%n = 
					hiCreateStringField( 
						?name		'TR_Floorplanner_CellGroup_%n 
						?prompt 	\"%n\" 
						?defValue 	cadddr( nth( %n l_CellGroup ))
						?editable	nil)"
					i+1
					i+1
					i+1
					i)
                		)

		evalstring( sprintf( nil 
				"l_TR_Floorplanner_CellGroup = cons( 
						list( 	TR_Floorplanner_CellGroup_%n 
							TR_Floorplanner_CellGroup_x:TR_Floorplanner_CellGroup_y 
							150:Height_CellGroup 
							30 )
						l_TR_Floorplanner_CellGroup )" i+1 ))


		; =============================================================================================
		; CellType Field
	        evalstring( sprintf( nil "TR_Floorplanner_DisplayApply_CB_TypeField_%n = 
					hiCreateRadioField( 
						?name		'TR_Floorplanner_DisplayApply_CB_TypeField_%n
						?choices	list( \"Data\" \"Ctrl\" ) 
						?defValue 	caadr( nth( %n l_CellGroup ))
						?callback	list( \"TR_Floorplanner_DisplayApply_CB_TypeField_CB( hiGetCurrentForm() %n )\" ))"
					i+1
					i+1
					i
					i)
                		)


		evalstring( sprintf( nil 
				"l_TR_Floorplanner_CellGroup = cons( 
						list( 	TR_Floorplanner_DisplayApply_CB_TypeField_%n 
							150:TR_Floorplanner_CellGroup_y 
							120:Height_CellGroup 
							0 )
						l_TR_Floorplanner_CellGroup )" i+1 ))



		; =============================================================================================
		; CellOrdering Field

	        evalstring( sprintf( nil "TR_Floorplanner_DisplayApply_CB_OrderField_%n = 
					hiCreateScaleField( 
						?name		'TR_Floorplanner_DisplayApply_CB_OrderField_%n
						?isContinuous 	t 
						?range 		list( 1 evalstring( strcat( \"TR_Floorplanner_Counter_\" caadr( nth( %n l_CellGroup ))))) 
						?defValue 	cadadr( nth( %n l_CellGroup ))
						?callback	\"TR_Floorplanner_DisplayApply_CB_OrderField_CB( hiGetCurrentForm() %n )\" )"
					i+1
					i+1
					i
					i
					i)
                		)

		evalstring( sprintf( nil 
				"l_TR_Floorplanner_CellGroup = cons( 
						list( 	TR_Floorplanner_DisplayApply_CB_OrderField_%n 
							270:TR_Floorplanner_CellGroup_y 
							130:Height_CellGroup 
							0 )
						l_TR_Floorplanner_CellGroup )" i+1 ))


		; =============================================================================================
		; CellOrientation Field

		case( evalstring( sprintf( nil "caar( nth( %n l_CellGroup ))" i))

			( 0
				FieldChoices = list( "NA" )
				FieldEnabled = nil
			)
			
			( 1
				FieldChoices = list( "V" "H" )
				FieldEnabled = t
			)
			
			( 2
				FieldChoices = list( "HV" "VH" )
				FieldEnabled = t
			)
		)	
			


	        evalstring( sprintf( nil "TR_Floorplanner_DisplayApply_CB_OrientationField_%n = 
					hiCreateRadioField( 
						?name		'TR_Floorplanner_DisplayApply_CB_OrientationField_%n
						?choices	FieldChoices
						?defValue 	cadar( nth( %n l_CellGroup ))
						?callback	list( \"TR_Floorplanner_DisplayApply_CB_OrientationField_CB( hiGetCurrentForm() %n )\" )
						?enabled	FieldEnabled )"
					i+1
					i+1
					i
					i)
                		)

		evalstring( sprintf( nil 
				"l_TR_Floorplanner_CellGroup = cons( 
						list( 	TR_Floorplanner_DisplayApply_CB_OrientationField_%n 
							400:TR_Floorplanner_CellGroup_y 
							100:Height_CellGroup 
							0 )
						l_TR_Floorplanner_CellGroup )" i+1 ))


		; =============================================================================================
		; Order Hashing Field

	        evalstring( sprintf( nil "TR_Floorplanner_DisplayApply_CB_OrderHashingField_%n = 
					hiCreateListField( 
						?name		'TR_Floorplanner_DisplayApply_CB_OrderHashingField_%n 
						?callback	\"TR_Floorplanner_DisplayApply_CB_OrderHashingField_CB( hiGetCurrentForm() %n )\"
						?defValue 	caddar( nth( %n l_CellGroup ))
						?editable	t)"
					i+1
					i+1
					i
					i)
                		)

		evalstring( sprintf( nil 
				"l_TR_Floorplanner_CellGroup = cons( 
						list( 	TR_Floorplanner_DisplayApply_CB_OrderHashingField_%n 
							500:TR_Floorplanner_CellGroup_y 
							100:Height_CellGroup 
							0 )
						l_TR_Floorplanner_CellGroup )" i+1 ))


		TR_Floorplanner_CellGroup_y = TR_Floorplanner_CellGroup_y + Height_CellGroup

		); end if

	); end foreach


	hiAddFields( theForm l_TR_Floorplanner_CellGroup )

	; ====================================================================================================

	); end let

)


; ====================================================================================================
; Callback Function
; ====================================================================================================
procedure( TR_Floorplanner_DisplayApply_CB_TypeField_CB( theForm index )

	let( (	CellGroup_Old
		CellGroup_New
		Type_Old
		Type_New
		)

	Type_Old = caadr( nth( index l_CellGroup ) )
	Order_Old = cadadr( nth( index l_CellGroup ) )

	evalstring( sprintf( nil "Type_New = theForm->TR_Floorplanner_DisplayApply_CB_TypeField_%n->value" index+1 ))

	case( Type_Old 

		( "Data"
			for( i Order_Old TR_Floorplanner_Counter_Data
				evalstring( sprintf( nil "theForm->TR_Floorplanner_DisplayApply_CB_OrderField_%n->value = i" index+1 ))
			)

			TR_Floorplanner_Counter_Data = TR_Floorplanner_Counter_Data - 1 
			TR_Floorplanner_Counter_Ctrl = TR_Floorplanner_Counter_Ctrl + 1
			CellGroup_New = subst( list( "Ctrl" TR_Floorplanner_Counter_Ctrl ) list( Type_Old TR_Floorplanner_Counter_Data+1 ) nth(index l_CellGroup))

			CellGroup_Old = nth( index l_CellGroup )
		)

		( "Ctrl"
			for( i Order_Old TR_Floorplanner_Counter_Ctrl
				evalstring( sprintf( nil "theForm->TR_Floorplanner_DisplayApply_CB_OrderField_%n->value = i" index+1 ))
			)

			TR_Floorplanner_Counter_Data = TR_Floorplanner_Counter_Data + 1
			TR_Floorplanner_Counter_Ctrl = TR_Floorplanner_Counter_Ctrl - 1
			CellGroup_New = subst( list( "Data" TR_Floorplanner_Counter_Data ) list( Type_Old TR_Floorplanner_Counter_Ctrl+1 ) nth(index l_CellGroup))
			CellGroup_Old = nth( index l_CellGroup )
		)

	); end case

	l_CellGroup = subst( CellGroup_New CellGroup_Old l_CellGroup )	

	; Refresh the display 
	TR_Floorplanner_DisplayApply_CB( theForm )

	; Return Value
	l_CellGroup

	); end let

)

; ====================================================================================================
; Callback Function
; ====================================================================================================
procedure( TR_Floorplanner_DisplayApply_CB_OrderField_CB( theForm index )

	let( (	CellGroup_Old
		CellGroup_New
		Order_Old
		Order_New

		CellGroup_Old2
		CellGroup_New2
		Order_Old2
		Order_New2

		)

	CellGroup_Old = nth( index l_CellGroup )
	Order_Old = cadr( CellGroup_Old )	

	evalstring( sprintf( nil "Order_New = list( caadr( CellGroup_Old ) theForm->TR_Floorplanner_DisplayApply_CB_OrderField_%n->value)" index+1 ))
	CellGroup_New = subst( Order_New Order_Old nth(index l_CellGroup))

	l_CellGroup = subst( CellGroup_New CellGroup_Old l_CellGroup )	


	for( i 0 length(l_CellGroup)-1 

		if( cadr( nth( i l_CellGroup )) == Order_New && i != index

			then
				

				CellGroup_Old2 = nth( i l_CellGroup )
				Order_Old2 = cadr( CellGroup_Old2 )

				Order_New2 = Order_Old
				CellGroup_New2 = subst( Order_New2 Order_Old2 nth(i l_CellGroup))

				l_CellGroup = subst( CellGroup_New2 CellGroup_Old2 l_CellGroup )

		)

	)

	; Can't do a Refresh on the display 
	; since this is a recursive callback 

	); end let

)


; ====================================================================================================
; Callback Function
; ====================================================================================================
procedure( TR_Floorplanner_DisplayApply_CB_OrientationField_CB( theForm index )

	let( (	CellGroup_Old
		CellGroup_New
		Orientation_Old
		Orientation_New
		)

	CellGroup_Old = nth( index l_CellGroup )
	Orientation_Old = car( CellGroup_Old )	

	evalstring( sprintf( nil "Orientation_New = list( 	caar( CellGroup_Old ) 
								theForm->TR_Floorplanner_DisplayApply_CB_OrientationField_%n->value 
								caddar( CellGroup_Old ))" index+1 ))
	CellGroup_New = subst( Orientation_New Orientation_Old nth(index l_CellGroup))

	l_CellGroup = subst( CellGroup_New CellGroup_Old l_CellGroup )	

	); end let

);end procedure


; ====================================================================================================
; Callback Function
; ====================================================================================================
procedure( TR_Floorplanner_DisplayApply_CB_OrderHashingField_CB( theForm index )

	let( (	CellGroup_Old
		CellGroup_New
		OrderHashing_Old
		OrderHashing_New
		)

	CellGroup_Old = nth( index l_CellGroup )
	OrderHashing_Old = car( CellGroup_Old )	

	evalstring( sprintf( nil "OrderHashing_New = list( 	caar( CellGroup_Old ) 
								cadar( CellGroup_Old )
								theForm->TR_Floorplanner_DisplayApply_CB_OrderHashingField_%n->value)" index+1 ))
	CellGroup_New = subst( OrderHashing_New OrderHashing_Old nth(index l_CellGroup))

	l_CellGroup = subst( CellGroup_New CellGroup_Old l_CellGroup )	

	); end let

);end procedure


; ====================================================================================================
; Callback Function
; ====================================================================================================
procedure( TR_Floorplanner_StartFloorplan_CB( theForm @key (DebugOn nil ) (AutoAlign nil))	

	let( ( 	Data_x Data_y Data_Width
		Ctrl_x Ctrl_y Ctrl_Width
		BitPitch
		) 

	Data_x = 0 
	Data_y = 0

	Ctrl_x = 0 
	Ctrl_y = 0
	
	geDeselectAllFig()

	l_CellGroup = TR_Floorplanner_Sort_l_CellGroup( l_CellGroup )

	foreach( CellGroup l_CellGroup

		DebugOn && printf( "TR_Floorplanner_StartFloorplan_CB --> Start Selecting Cells for CellGroup %s \n" cadddr( CellGroup ))

		foreach( Cell car( cddddr( CellGroup ))

			geSelectFig( Cell ) 

		); end foreach

		DebugOn && printf( "TR_Floorplanner_StartFloorplan_CB --> Forking to Procedure \"FloorplanCells\"\n")

		case( caadr( CellGroup )
		
			( "Data"

				if( TR_Floorplanner_NumBits == 0
					then
						BitPitch = 0
					else
						BitPitch = TR_Floorplanner_BitPitch
				)

				Data_Width = FloorplanCells( 	Data_x Data_y 	
								?Dimension 	caar( CellGroup ) 
								?Type 		caadr( CellGroup )
								?Orientation	cadar( CellGroup )
								?HashList	eval( caddar( CellGroup ))
								?BitPitch	BitPitch
								?DatapathHeight TR_Floorplanner_DatapathHeight
								?NumBitsPerCell 0
								)
				Data_x = Data_x + Data_Width
			)				

			( "Ctrl"
				Ctrl_Width = FloorplanCells( 	Ctrl_x Ctrl_y 
								?Dimension 	caar( CellGroup ) 
								?Type 		caadr( CellGroup )
								?Orientation	cadar( CellGroup )
								?HashList	eval( caddar( CellGroup ))
								?BitPitch	0
								?DatapathHeight 0
								?NumBitsPerCell 0
								)
				Ctrl_x = Ctrl_x + Ctrl_Width
			)				


		); end case

		geDeselectAllFig()

	);end foreach

	; Refresh the display 
	TR_Floorplanner_DisplayApply_CB( theForm )


	; Align everything to the corresponding power grid
	if( AutoAlign
		then
			CompactCellsResursive(  ?Depth		-1
						?WorkOnAll	TR_Floorplanner_WorkOnAll 
						?AlignToGrid 	list( 	theForm->TR_Floorplanner_LeafResolutionX->value:theForm->TR_Floorplanner_LeafResolutionY->value
									theForm->TR_Floorplanner_MidResolutionX->value:theForm->TR_Floorplanner_MidResolutionY->value )
						?AutoSave	nil
						)
	); end if

	);end let

); end procedure


; ====================================================================================================
; Sorting Function
; ====================================================================================================
procedure( TR_Floorplanner_Sort_l_CellGroup( l_CellGroup_Input )

	let( (	l_CellGroup_returnValue 
		l_CellGroup_Data
		l_CellGroup_Ctrl )
	
	l_CellGroup_returnValue = nil
	l_CellGroup_Data = nil
	l_CellGroup_Ctrl = nil


	foreach( CellGroup_Input l_CellGroup_Input

		if( caadr( CellGroup_Input ) == "Data"
			then
				l_CellGroup_Data = cons( list( cadadr( CellGroup_Input ) CellGroup_Input ) l_CellGroup_Data)
			else
				l_CellGroup_Ctrl = cons( list( cadadr( CellGroup_Input ) CellGroup_Input ) l_CellGroup_Ctrl)

		)
					
	); end foreach
	

	l_CellGroup_Data = sortcar( l_CellGroup_Data `greaterp ) 
	l_CellGroup_Ctrl = sortcar( l_CellGroup_Ctrl `greaterp )

	foreach( CellGroup_Ctrl l_CellGroup_Ctrl

		l_CellGroup_returnValue = cons( cadr( CellGroup_Ctrl ) l_CellGroup_returnValue )

	)

	foreach( CellGroup_Data l_CellGroup_Data

		l_CellGroup_returnValue = cons( cadr( CellGroup_Data ) l_CellGroup_returnValue )

	)

	l_CellGroup_returnValue

	); end let

); end procedure


; ====================================================================================================
; Sorting Function ------------CURRENTLY FORBIDDEN-----------------
; ====================================================================================================
procedure( TR_Floorplanner_Sort_l_Cell( l_Cell_Input l_Hash @key (Dimension 0 ) ( Orientation "NA" ))


	let( (	l_Cell_returnValue 
		l_Cell_Temp )
	
	l_Cell_returnValue = nil
	l_Cell_Temp = nil


	case( l_Hash
		( nil
			; Do Nothing
		)

		( t
		)


	); end case



	); end let

); end procedure


;==============================================================================
; DUMMY CALLBACK FUNCTION
;==============================================================================
procedure( TR_Floorplanner_CB( theForm )

	let((	returnValue )

		printf( "TR_Floorplanner_CB --> Thankx for using Spider 2.0\n" )

	); end let

); end procedure
