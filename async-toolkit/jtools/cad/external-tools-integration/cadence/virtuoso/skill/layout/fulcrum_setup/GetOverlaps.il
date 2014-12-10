; This Skill Code will select cells ("inst") whish is overlapping with the seleceted cells
; The code will automatically filter out unrelated object type (eg. "shape" "rect") and 
; will not consider them in the process
; 
; Example
; -------
; Consider a layout with only instances A, B, C and one rectangle D
; Suppose:   A overlaps with B, C overlaps with the rectangle D, both groups (AB and CD) 
;	     do not intersect
;
;	(1) If A,B,C,D are selected to run this skill code
;           ==> only A and B will result in selection (since the code pays no attention to "rectangles"
;
;       (2) If only rectangle D is selected
;           ==> nothing will be in the selection (for the same reason in (1))
;
;       (3) If only instance A is selected
;           ==> Instance B will be in the selection (not A as it is treated as the source)

procedure( GetOverlaps( )

	SelectedCellList = geGetSelSet( )

	geDeselectAllFig( geGetWindowCellView( ) )

        foreach( SelectedCell SelectedCellList 

		if( SelectedCell->objType == "inst"
		
		then		
			overlap_list = dbGetOverlaps( geGetWindowCellView( )  SelectedCell->bBox nil )

	       		foreach( overlap_object overlap_list 
			
				if( !( overlap_object == SelectedCell )
					then
						geSelectFig( overlap_object )
				) ;if

        		) ;foreach	
		) ; if

        ) ;foreach

) ;procedure
