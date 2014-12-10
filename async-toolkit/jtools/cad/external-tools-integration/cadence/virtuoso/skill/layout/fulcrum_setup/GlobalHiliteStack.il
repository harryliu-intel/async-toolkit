; Global Variable
;
; 1. Global_HitliteStack

GlobalHiliteStack = nil

; ============================================================================================
; This procedure is used to create a hilite rectange griup and do nothing
; ============================================================================================

procedure( GlobalHiliteStack_Pop( )

	if( geGetCurrentHilightSet( geGetWindowCellView())
		then
			GlobalHiliteStack = cons( geGetCurrentHilightSet( geGetWindowCellView()) GlobalHiliteStack)
	)
	gePopHilightStack( geGetWindowCellView( ))
)


; ============================================================================================
; This procedure is used to create a hilite rectange griup and do nothing
; ============================================================================================

procedure( GlobalHiliteStack_Push( )

	if( car( GlobalHiliteStack )
		then
			gePushHilightStack( car( GlobalHiliteStack ) )
	)
	GlobalHiliteStack = cdr( GlobalHiliteStack )
)

; ============================================================================================
; This procedure is used to delete a hilite group 
; ============================================================================================

procedure( GlobalHiliteStack_Delete( )

	gePopHilightStack( geGetWindowCellView( ))

)

; ============================================================================================
; This procedure is used to clear the entire stack
; ============================================================================================

procedure( GlobalHiliteStack_Clear( )

	let(( isFinish )

	; Local Variables
	; ===============
	isFinish = nil

	while( !isFinish
		gePopHilightStack( geGetWindowCellView( ))
		if( !geGetCurrentHilightSet( geGetWindowCellView( ))
			then
				isFinish = t
		); end if
	); end while		

	); end let

)

