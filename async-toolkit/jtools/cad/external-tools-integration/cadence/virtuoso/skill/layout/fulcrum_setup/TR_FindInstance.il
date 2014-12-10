; This will report all 
; 1. cellName
; 2. instName
; 3. viewName
; on all selected instance ONLY
; CheckName( ?viewName `( nil "crap" ) ?cellName `( nil "lib.buffer.half.MBUF_1of2.0_L1"))
;
; WildCard Usage "*" matches 1+ occurance of any character if "*" appears as the first character
;		else matches 0+ occurance
;
; WildCard Usage "+" matches 0+ occurance of any number and the "-" charater
;
; Support Special Character List = "[" "]" "-" "." ","


procedure( FindInstance( @key
			( cellName list( t nil ) )
			( viewName list( t nil ) )
			( instName list( t nil ) )
			( WorkOnAll t )
			) 

	let( ( 	returnValue
		CellMatch 
		Counter
		l_d_Selected)

	returnValue  = t
	CellMatch    = t
	Counter      = 0

	if( WorkOnAll
		then
			geSelectAllFig()
	)

	l_d_Selected = geGetSelectedSet()

	geDeselectAllFig()

	foreach( d_Selected l_d_Selected

		if( d_Selected->objType == "inst"
			then
				CellMatch = t
				; Compile Patterns for cellName
				if( !car( cellName ) && CellMatch  
					then
						rexCompile( ConvertWildCardString( cadr( cellName ) ) )
						CellMatch = rexExecute( d_Selected->cellName )
				); end if	

				; Compile Patterns for cellName
				if( !car( viewName ) && CellMatch  
					then
						rexCompile( ConvertWildCardString( cadr( viewName ) ) )
						CellMatch = rexExecute( d_Selected->viewName )
				); end if	

				; Compile Patterns for instName
				if( !car( instName ) && CellMatch  
					then
						rexCompile( ConvertWildCardString( cadr( instName ) ) )
						CellMatch = rexExecute( d_Selected->name )
				); end if	


				if( CellMatch
					then
						Counter = Counter + 1
						printf( "FindInstance --> %3n %-45s %-15s %-15s\n"
							Counter 
							d_Selected->cellName
							d_Selected->viewName
							d_Selected->name
							)
						geSelectFig( d_Selected )
				)
		)

	); end foreach

	if( Counter == 0
		then
			printf( "FindInstance --> No Match Cells\n" )
		else
			printf( "FindInstance --> Number Matched = %n\n" Counter )
	)		
		
	returnValue

	); end let

); end procedure


; WildCard String Converter
procedure( ConvertWildCardString( InputString @key ( FullString t ) )

	let( (	OutputString )

		if( FullString
			then
				InputString = strcat( "^" InputString )
				InputString = strcat( InputString "$" )
		)

		rexCompile( "\\." )
		InputString =  rexReplace( InputString "\\\\." 0 )
		
		rexCompile( "\\[" )
		InputString =  rexReplace( InputString "\\\\[" 0 )
		
		rexCompile( "\\]" )
		InputString =  rexReplace( InputString "\\\\]" 0 )
		
		rexCompile( "\\-" )
		InputString =  rexReplace( InputString "\\\\-" 0 )
		
		rexCompile( "\\," )
		InputString =  rexReplace( InputString "\\\\," 0 )
		
		rexCompile( "\\*" )
		InputString = rexReplace( InputString ".*" 0 )

		rexCompile( "\\+" )
		InputString = rexReplace( InputString "[0-9\\\\-]*" 0 )

		rexCompile( "^\\^\\.\\*" )
		InputString = rexReplace( InputString "^.+" 0 )

		OutputString = InputString

	); end let

); end procedure


; This is the GUI Portion
procedure( TR_FindInstance( )

        let( (  TR_FindInstance 
                TR_FindInstance_cellName
                TR_FindInstance_ySnap )

        TR_FindInstance_cellName = hiCreateStringField( 
                ?name		'TR_FindInstance_cellName 
                ?prompt		"Enter cellName (t = search all)" 
                ?defValue	"t" 
                ?editable	t )

        TR_FindInstance_viewName = hiCreateStringField( 
                ?name		'TR_FindInstance_viewName 
                ?prompt		"Enter viewName (t = search all)" 
                ?defValue	"t" 
                ?editable	t )

        TR_FindInstance_instName = hiCreateStringField( 
                ?name		'TR_FindInstance_instName 
                ?prompt		"Enter instName (t = search all)" 
                ?defValue	"t" 
                ?editable	t )


	TR_FindInstance_WorkOnAll = hiCreateRadioField( 
		?name 		'TR_FindInstance_WorkOnAll
		?choices 	list( "Selected Objects Only" "All Objects" ) 
		?prompt 	"Work on"
		?defValue 	"All Objects"
		)

        TR_FindInstance_Form = hiCreateAppForm( 
                ?name 'TR_FindInstance_Form
                ?fields  '( 	TR_FindInstance_cellName 
				TR_FindInstance_viewName 
				TR_FindInstance_instName 
				TR_FindInstance_WorkOnAll )
                ?callback "TR_FindInstance_CB( hiGetCurrentForm() )" 
        )


        hiDisplayForm( TR_FindInstance_Form )

        ); end let

); end procedure


procedure( TR_FindInstance_CB( theForm )

	if( theForm->TR_FindInstance_cellName->value == "t"
		then
		        FindInstance_cellName = list( t t )
		else
		        FindInstance_cellName = list( nil theForm->TR_FindInstance_cellName->value )
	)
			
	if( theForm->TR_FindInstance_viewName->value == "t"
		then
		        FindInstance_viewName = list( t t )
		else
		        FindInstance_viewName = list( nil theForm->TR_FindInstance_viewName->value )
	)
			
	if( theForm->TR_FindInstance_instName->value == "t"
		then
		        FindInstance_instName = list( t t )
		else
		        FindInstance_instName = list( nil theForm->TR_FindInstance_instName->value )
	)

	if( theForm->TR_FindInstance_WorkOnAll->value == "All Objects"
		then
			FindInstance_WorkOnAll = t
		else
			FindInstance_WorkOnAll = nil
	)

 	FindInstance( 	?cellName	FindInstance_cellName
			?viewName	FindInstance_viewName
			?instName	FindInstance_instName
			?WorkOnAll	FindInstance_WorkOnAll
			)				

); end procedure


