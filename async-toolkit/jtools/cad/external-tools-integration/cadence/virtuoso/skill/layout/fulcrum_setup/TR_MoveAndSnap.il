; global variables for alignment
MoveAndSnap_xSnap=pitch_m3
MoveAndSnap_ySnap=pitch_m4

MoveAndSnapGate_xSnap=GatePitch
MoveAndSnapGate_ySnap=PowerGridPitch

; Check to See if the selected objects is out of window
procedure( TR_MoveAndSnap_ProcCheckWindow( @key
						( DebugOn t )
						)

	let((	currentWindow		currentWindowBox 
		currentSelectedSet	currentSelected		currentSelectedBox
		numViolations
		XL	XH	YL	YH
		XL0	XH0	YL0	YH0
		XL1	XH1	YL1	YH1
		lockedDirection
		)

	; Local Variables
	; ===============
	currentWindow	   = geGetCellViewWindow( geGetWindowCellView())
	currentSelectedSet = geGetSelectedSet( )
	numViolations 	   = 1
	lockedDirection    = nil

	XL = caar( car( currentSelectedSet )->bBox )
	YL = cadar( car( currentSelectedSet )->bBox )
	XH = caadr( car( currentSelectedSet )->bBox )
	YH = cadadr( car( currentSelectedSet )->bBox )

	foreach( currentSelected currentSelectedSet

		XL1 = caar( currentSelected->bBox )
		YL1 = cadar( currentSelected->bBox )
		XH1 = caadr( currentSelected->bBox )
		YH1 = cadadr( currentSelected->bBox )

		if( XL1 < XL
			then
				XL = XL1
		); end if

		if( XH1 > XH
			then
				XH = XH1
		); end if

		if( YL1 < YL
			then
				YL = YL1
		); end if

		if( YH1 > YH
			then
				YH = YH1
		); end if
	) 

	currentSelectedBox = list( XL:YL XH:YH )



	while( numViolations == 1

		numViolations = 0

		currentWindowBox   = geGetWindowBox( currentWindow )

		XL0 = caar( currentWindowBox )
		YL0 = cadar( currentWindowBox )
		XH0 = caadr( currentWindowBox )
		YH0 = cadadr( currentWindowBox )

		; If the selected object is out of bound on the top
		if( YH > YH0 
			then
				numViolations = numViolations + 1
				if( lockedDirection != nil && lockedDirection != "n"
					then
						numViolations = numViolations + 1
				); end if
				lockedDirection = "n"
		);end if

		; If the selected object is out of bound on the bottom
		if( YL < YL0 
			then
				numViolations = numViolations + 1
				if( lockedDirection != nil && lockedDirection != "s"
					then
						numViolations = numViolations + 1
				); end if
				lockedDirection = "s"
		);end if

		; If the selected object is out of bound on the left
		if( XL < XL0 
			then
				numViolations = numViolations + 1
				if( lockedDirection != nil && lockedDirection != "w"
					then
						numViolations = numViolations + 1
				); end if
				lockedDirection = "w"
		);end if

		; If the selected object is out of bound on the right
		if( XH > XH0 
			then
				numViolations = numViolations + 1
				if( lockedDirection != nil && lockedDirection != "e"
					then
						numViolations = numViolations + 1
				); end if
				lockedDirection = "e"
		);end if



		; Scroll ONLY if there is one Violation
		; =====================================
		if( numViolations == 1
			then
				geScroll( currentWindow lockedDirection nil )
				DebugOn && printf( "TR_MoveAndSnap_ProcCheckWindow --> Scroll %s: currentWindowBox = %L objBox = %L\n" 
						lockedDirection
						currentWindowBox 
						currentSelectedBox ) 
		); end if

				
	); end while

	); end let

); end procedure



; move one cell
(defun MoveAndSnap_MoveFig ( fig dx dy )
  (dbMoveFig fig nil (list (list dx dy) "R0"))
  )

; move selected cells by specified offset
(defun MoveAndSnap_MoveSelected ( dx dy )
  (foreach fig (geGetSelSet) (MoveAndSnap_MoveFig fig dx dy))
  )

; move selected cells to closest alignment grid in each direction
(defun MoveAndSnap (dir @key (overwrite_xSnap nil) (overwrite_ySnap nil) (overwrite_fig nil) (overwrite_xy nil))
 (let (x xSnap y ySnap l_Selected)

  (if overwrite_fig then
    l_Selected = ( list overwrite_fig)
  else
    l_Selected = (geGetSelectedSet)
  )
  (if !overwrite_xSnap then
    xSnap = (max MoveAndSnap_xSnap (GetMaxXAlignmentOfSet l_Selected))
  else
    xSnap = overwrite_xSnap
  )

  (if !overwrite_ySnap then
    ySnap = (max MoveAndSnap_ySnap (GetMaxYAlignmentOfSet l_Selected))
  else
    ySnap = overwrite_ySnap
  )

  (foreach fig l_Selected
    (if (IsInstanceAnchored fig) then
      (printf "Not moving anchored instance %s.\n" fig->name)
    else ;do the rest

    (if fig->objType == "inst" || fig->objType == "mosaic"  then
      (if overwrite_xy then
        xOffSet = car( overwrite_xy ) - (car fig -> xy )
        yOffSet = cadr( overwrite_xy ) - (car (cdr fig -> xy ))
      else
        xOffSet = 0
        yOffSet = 0
      )

      x = (car fig -> xy ) + xOffSet
      y = (car (cdr fig -> xy )) + yOffSet

      (cond
        ((equal dir "L") (MoveAndSnap_MoveFig fig (floor (x-0.005)/xSnap)*xSnap-x 0 ))
        ((equal dir "R") (MoveAndSnap_MoveFig fig (ceiling (x+0.005)/xSnap)*xSnap-x 0 ))
        ((equal dir "U") (MoveAndSnap_MoveFig fig 0 (ceiling (y+0.005)/ySnap)*ySnap-y ))
        ((equal dir "D") (MoveAndSnap_MoveFig fig 0 (floor (y-0.005)/ySnap)*ySnap-y ))

        ((equal dir "A")
          (if (abs (floor x/xSnap)*xSnap-x ) <= (abs (ceiling x/xSnap)*xSnap-x) then
            (MoveAndSnap_MoveFig fig (floor x/xSnap)*xSnap-x 0 )
          else
            (MoveAndSnap_MoveFig fig (ceiling x/xSnap)*xSnap-x 0 )
          )
          x = (car fig -> xy ) + xOffSet
          y = (car (cdr fig -> xy )) + yOffSet

          (if (abs (ceiling y/ySnap)*ySnap-y ) <= (abs (floor y/ySnap)*ySnap-y ) then
            (MoveAndSnap_MoveFig fig 0 (ceiling y/ySnap)*ySnap-y )
          else
            (MoveAndSnap_MoveFig fig 0 (floor y/ySnap)*ySnap-y )
          )
        )

        ((equal dir "AV")
          (if (abs (ceiling y/ySnap)*ySnap-y ) <= (abs (floor y/ySnap)*ySnap-y ) then
            (MoveAndSnap_MoveFig fig 0 (ceiling y/ySnap)*ySnap-y )
          else
            (MoveAndSnap_MoveFig fig 0 (floor y/ySnap)*ySnap-y )
          )
        )

        ((equal dir "AH")
          (if (abs (floor x/xSnap)*xSnap-x ) <= abs( (ceiling x/xSnap)*xSnap-x) then
            (MoveAndSnap_MoveFig fig (floor x/xSnap)*xSnap-x 0 )
          else
            (MoveAndSnap_MoveFig fig (ceiling x/xSnap)*xSnap-x 0 )
          )
        )

        ((equal dir "ARAV")
          (MoveAndSnap_MoveFig fig (ceiling x/xSnap)*xSnap-x 0 )
          x = (car fig -> xy ) + xOffSet
          y = (car (cdr fig -> xy )) + yOffSet
          (if (abs (ceiling y/ySnap)*ySnap-y ) <= (abs (floor y/ySnap)*ySnap-y ) then
            (MoveAndSnap_MoveFig fig 0 (ceiling y/ySnap)*ySnap-y )
          else
            (MoveAndSnap_MoveFig fig 0 (floor y/ySnap)*ySnap-y )
          )
        )

        ((equal dir "RUD") 
          (MoveAndSnap_MoveFig fig (ceiling (x+0.005)/xSnap)*xSnap-x 0 )
          x = (car fig -> xy ) + xOffSet
          y = (car (cdr fig -> xy )) + yOffSet

          (MoveAndSnap_MoveFig fig 0 (ceiling (y+0.005)/ySnap)*ySnap-y )
          x = (car fig -> xy ) + xOffSet
          y = (car (cdr fig -> xy )) + yOffSet

          (MoveAndSnap_MoveFig fig 0 (floor (y-0.005)/ySnap)*ySnap-y )
          x = (car fig -> xy ) + xOffSet
          y = (car (cdr fig -> xy )) + yOffSet
        )
      )
     )
    )
   )

  ;removing this procedure call for Bug 13562
  ;TR_MoveAndSnap_ProcCheckWindow( )
 ); end let
)

;Move Selected Gates/Stacks to the nearest poly pitch
(defun MoveAndSnapGate (dir @key (overwrite_xSnap nil) (overwrite_ySnap nil) (overwrite_fig nil) (overwrite_xy nil))
 (let (x xSnap y ySnap l_Selected)

  (if overwrite_fig then
    l_Selected = ( list overwrite_fig)
  else
    l_Selected = (geGetSelectedSet)
  )
  (if !overwrite_xSnap then
    xSnap = (max MoveAndSnapGate_xSnap (GetMaxXAlignmentOfSet l_Selected))
  else
    xSnap = overwrite_xSnap
  )

  (if !overwrite_ySnap then
    ySnap = (max MoveAndSnapGate_ySnap (GetMaxYAlignmentOfSet l_Selected))
  else
    ySnap = overwrite_ySnap
  )

  (foreach fig l_Selected
    (if (IsInstanceAnchored fig) then
      (printf "Not moving anchored instance %s.\n" fig->name)
    else ;do the rest

    (if fig->objType == "inst" || fig->objType == "mosaic"  then
      (if overwrite_xy then
        xOffSet = car( overwrite_xy ) - (car fig -> xy )
        yOffSet = cadr( overwrite_xy ) - (car (cdr fig -> xy ))
      else
        xOffSet = 0
        yOffSet = 0
      )

      x = (car fig -> xy ) + xOffSet
      y = (car (cdr fig -> xy )) + yOffSet

      (cond
        ((equal dir "L") (MoveAndSnap_MoveFig fig (floor (x-0.005)/xSnap)*xSnap-x 0 ))
        ((equal dir "R") (MoveAndSnap_MoveFig fig (ceiling (x+0.005)/xSnap)*xSnap-x 0 ))
        ((equal dir "U") (MoveAndSnap_MoveFig fig 0 (ceiling (y+0.005)/ySnap)*ySnap-y ))
        ((equal dir "D") (MoveAndSnap_MoveFig fig 0 (floor (y-0.005)/ySnap)*ySnap-y ))

        ((equal dir "A")
          (if (abs (floor x/xSnap)*xSnap-x ) <= (abs (ceiling x/xSnap)*xSnap-x) then
            (MoveAndSnap_MoveFig fig (floor x/xSnap)*xSnap-x 0 )
          else
            (MoveAndSnap_MoveFig fig (ceiling x/xSnap)*xSnap-x 0 )
          )
          x = (car fig -> xy ) + xOffSet
          y = (car (cdr fig -> xy )) + yOffSet

          (if (abs (ceiling y/ySnap)*ySnap-y ) <= (abs (floor y/ySnap)*ySnap-y ) then
            (MoveAndSnap_MoveFig fig 0 (ceiling y/ySnap)*ySnap-y )
          else
            (MoveAndSnap_MoveFig fig 0 (floor y/ySnap)*ySnap-y )
          )
        )

        ((equal dir "AV")
          (if (abs (ceiling y/ySnap)*ySnap-y ) <= (abs (floor y/ySnap)*ySnap-y ) then
            (MoveAndSnap_MoveFig fig 0 (ceiling y/ySnap)*ySnap-y )
          else
            (MoveAndSnap_MoveFig fig 0 (floor y/ySnap)*ySnap-y )
          )
        )

        ((equal dir "AH")
          (if (abs (floor x/xSnap)*xSnap-x ) <= abs( (ceiling x/xSnap)*xSnap-x) then
            (MoveAndSnap_MoveFig fig (floor x/xSnap)*xSnap-x 0 )
          else
            (MoveAndSnap_MoveFig fig (ceiling x/xSnap)*xSnap-x 0 )
          )
        )

        ((equal dir "ARAV")
          (MoveAndSnap_MoveFig fig (ceiling x/xSnap)*xSnap-x 0 )
          x = (car fig -> xy ) + xOffSet
          y = (car (cdr fig -> xy )) + yOffSet
          (if (abs (ceiling y/ySnap)*ySnap-y ) <= (abs (floor y/ySnap)*ySnap-y ) then
            (MoveAndSnap_MoveFig fig 0 (ceiling y/ySnap)*ySnap-y )
          else
            (MoveAndSnap_MoveFig fig 0 (floor y/ySnap)*ySnap-y )
          )
        )

        ((equal dir "RUD") 
          (MoveAndSnap_MoveFig fig (ceiling (x+0.005)/xSnap)*xSnap-x 0 )
          x = (car fig -> xy ) + xOffSet
          y = (car (cdr fig -> xy )) + yOffSet

          (MoveAndSnap_MoveFig fig 0 (ceiling (y+0.005)/ySnap)*ySnap-y )
          x = (car fig -> xy ) + xOffSet
          y = (car (cdr fig -> xy )) + yOffSet

          (MoveAndSnap_MoveFig fig 0 (floor (y-0.005)/ySnap)*ySnap-y )
          x = (car fig -> xy ) + xOffSet
          y = (car (cdr fig -> xy )) + yOffSet
        )
      )
     )
    )
   )

  ;removing this procedure call for Bug 13562
  ;TR_MoveAndSnap_ProcCheckWindow( )
 ); end let
)


procedure( InitLockMoveAndSnap()
  ToggleSnapLock=0
)
InitLockMoveAndSnap()

procedure( LockMoveAndSnap()
  if( mod(ToggleSnapLock 2)==0 then
    hiSetBindKey( "Layout" "<Key>Left"  "MoveAndSnap(\"L\")")
    hiSetBindKey( "Layout" "<Key>Right" "MoveAndSnap(\"R\")")
    hiSetBindKey( "Layout" "<Key>Up"    "MoveAndSnap(\"U\")")
    hiSetBindKey( "Layout" "<Key>Down"  "MoveAndSnap(\"D\")")
  else
    hiSetBindKey( "Layout" "<Key>Left"  "geScroll(nil \"w\" nil)")
    hiSetBindKey( "Layout" "<Key>Right" "geScroll(nil \"e\" nil)")
    hiSetBindKey( "Layout" "<Key>Up"    "geScroll(nil \"n\" nil)")
    hiSetBindKey( "Layout" "<Key>Down"  "geScroll(nil \"s\" nil)")
  )
  ToggleSnapLock = ToggleSnapLock+1
)



procedure( TR_MoveAndSnap( )

	let( ( 	TR_MoveAndSnap 
		TR_MoveAndSnap_xSnap
		TR_MoveAndSnap_ySnap )

	TR_MoveAndSnap_xSnap = hiCreateStringField( 
        	?name 'TR_MoveAndSnap_xSnap 
        	?prompt "Enter xSnap Value" 
        	?defValue sprintf( nil "%n" MoveAndSnap_xSnap ) 
        	?editable t )

	TR_MoveAndSnap_ySnap = hiCreateStringField( 
        	?name 'TR_MoveAndSnap_ySnap 
        	?prompt "Enter ySnap Value" 
        	?defValue sprintf( nil "%n" MoveAndSnap_ySnap ) 
        	?editable t )

	TR_MoveAndSnap_Form = hiCreateAppForm( 
        	?name 'TR_MoveAndSnap_Form
        	?fields  '( TR_MoveAndSnap_xSnap TR_MoveAndSnap_ySnap )
        	?callback "TR_MoveAndSnap_CB( hiGetCurrentForm() )" 
	)

	hiDisplayForm( TR_MoveAndSnap_Form )

	); end let

); end procedure


procedure( TR_MoveAndSnap_CB( theForm )

	MoveAndSnap_xSnap = evalstring( theForm->TR_MoveAndSnap_xSnap->value )
	MoveAndSnap_ySnap = evalstring( theForm->TR_MoveAndSnap_ySnap->value )

); end procedure


procedure( SuperMoveAndSnap( direction @key 
				( AlignToGrid list( 0.48:MoveAndSnap_ySnap MoveAndSnap_xSnap:MoveAndSnap_ySnap ))
				( DebugOn t )
				)

	let(( 	l_d_Cell
		IsMidLevelCell
		)

	l_d_Cell = setof( x geGetSelectedSet() x->objType=="inst" )

	foreach( CellUnderTest l_d_Cell

		IsMidLevelCell = setof( x 
					leSearchHierarchy(	CellUnderTest->master
					CellUnderTest->master->bBox
					0
					"inst"
					nil
					)
					( x->libName != "tsmc13lg" && x->libName != "gate" && x->libName != "stack" )
				)

		; Filter out SLACK
		if( IsMidLevelCell
			then
				rexCompile( "lib.buffer.slack" )
				IsMidLevelCell = !rexExecute( CellUnderTest->cellName )
		)


		; Filter out TOKEN
		if( IsMidLevelCell
			then
				rexCompile( "lib.buffer.token" )
				IsMidLevelCell = !rexExecute( CellUnderTest->cellName )
		)

		; Filter out CTREE
		if( IsMidLevelCell
			then
				rexCompile( "lib.util.ctree" )
				IsMidLevelCell = !rexExecute( CellUnderTest->cellName )
		)


		if( IsMidLevelCell 
			then
				DebugOn && printf( "SuperMoveAndSnap --> \"%s\" is a mid-level cell\n" CellUnderTest->name )
				MoveAndSnap( 	direction
						?overwrite_fig CellUnderTest
						?overwrite_xSnap caadr( AlignToGrid ) 
						?overwrite_ySnap cadadr( AlignToGrid ) 
						)
			else
				DebugOn && printf( "SuperMoveAndSnap --> \"%s\" is a leaf cell\n" CellUnderTest->name )
				MoveAndSnap( 	direction
						?overwrite_fig CellUnderTest
						?overwrite_xSnap caar( AlignToGrid ) 
						?overwrite_ySnap cadar( AlignToGrid ) 
						)
		)

	); end foreach


	); end let

); end procedure

