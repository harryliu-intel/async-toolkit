procedure( TR_ChangeInstNameProcMainWild( @key
					( l_d_Cell	geGetSelectedSet())
					( newInstName	"DumbDumb" )
					( patternList	list( "\\[0\\]\\[1\\]" "\\[0\\]\\[2\\]" ))
					)
	let((	d_Cell
		sourcePattern	sourceName
		targetPattern	targetName
		)

	d_Cell = nil
	sourcePattern = nth( 0 patternList )
	targetPattern = nth( 1 patternList )

	foreach( d_Cell l_d_Cell

		sourceName = d_Cell->name

		rexCompile( sourcePattern )
		targetName = rexReplace( sourceName targetPattern 0 )

		d_Cell->name = targetName

	); end foreach

	); end let

); end procedure

procedure( TR_ChangeInstNameProcMain( @key
					( d_Cell	car(geGetSelectedSet()))
					( newInstName	"DumbDumb" )
					)
	d_Cell->name = newInstName	

); end procedure


procedure( TR_ChangeInstNameProcGUI( )

        let( (  TR_ChangeInstNameProcGUI 
                TR_ChangeInstNameProcGUI_newInstNameField
		)

        TR_ChangeInstNameProcGUI_newInstNameField = hiCreateStringField( 
                ?name		'TR_ChangeInstNameProcGUI_newInstNameField 
                ?prompt		"Enter New Instance Name" 
                ?defValue	car( geGetSelectedSet())->name
                ?editable 	t )

        TR_ChangeInstNameProcGUI_Form = hiCreateAppForm( 
                ?name 		'TR_ChangeInstNameProcGUI_Form
                ?initialSize    list( 300 500 )
                ?fields  	list( 
					list( TR_ChangeInstNameProcGUI_newInstNameField 0:0   300:30 200 )
					)
                ?callback "TR_ChangeInstNameProcMain( 
				?d_Cell		car( geGetSelectedSet())
				?newInstName	TR_ChangeInstNameProcGUI_newInstNameField->value
				)" 
        )

        hiDisplayForm( TR_ChangeInstNameProcGUI_Form )

        ); end let

); end procedure
