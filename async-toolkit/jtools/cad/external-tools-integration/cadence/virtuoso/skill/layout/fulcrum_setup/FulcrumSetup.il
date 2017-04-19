; In-house Skill Files
; ====================
defun( FulcrumSetup ()

(when (hiGraphicMode)
printf("Loading FulcrumSetup bindkeys...\n")
hiSetBindKey( "Layout" "<Key>h"     "SetSnap_x4_8_y9_6_Key_h( )" )
hiSetBindKey( "Layout" "<Shift>h"   "AlignCells( PowerGridPitch 2*PowerGridPitch 0 0 )" )

hiSetBindKey( "Layout" "<Key>j"     "SetSnap_x0_48_y9_6_Key_j( )" )
hiSetBindKey( "Layout" "<Shift>j"   "AlignCells( 0.24 2*PowerGridPitch 0 0 )" )

hiSetBindKey( "Layout" "<Ctrl>o" "GetOverlaps( )" )

hiSetBindKey( "Layout"  "<Key>1" "SetViewLevels( 0 1 )" )
hiSetBindKey( "Layout"  "<Key>2" "SetViewLevels( 0 2 )" )
hiSetBindKey( "Layout"  "<Key>3" "SetViewLevels( 0 3 )" )
hiSetBindKey( "Layout"  "<Key>4" "SetViewLevels( 0 4 )" )
hiSetBindKey( "Layout"  "<Key>5" "SetViewLevels( 0 5 )" )
hiSetBindKey( "Layout"  "<Key>6" "SetViewLevels( 0 6 )" )
hiSetBindKey( "Layout"  "<Key>7" "SetViewLevels( 0 7 )" )
hiSetBindKey( "Layout"  "<Key>8" "SetViewLevels( 0 8 )" )
hiSetBindKey( "Layout"  "<Key>9" "SetViewLevels( 0 32 )" )

hiSetBindKey( "Layout"  "<Shift>1" "SetViewLevels( 1 1 )" )
hiSetBindKey( "Layout"  "<Shift>2" "SetViewLevels( 2 2 )" )
hiSetBindKey( "Layout"  "<Shift>3" "SetViewLevels( 3 3 )" )
hiSetBindKey( "Layout"  "<Shift>4" "SetViewLevels( 4 4 )" )
hiSetBindKey( "Layout"  "<Shift>5" "SetViewLevels( 5 5 )" )
hiSetBindKey( "Layout"  "<Shift>6" "SetViewLevels( 6 6 )" )
hiSetBindKey( "Layout"  "<Shift>7" "SetViewLevels( 7 7 )" )
hiSetBindKey( "Layout"  "<Shift>8" "SetViewLevels( 8 8 )" )

hiSetBindKey( "Layout"  "<Ctrl>1" "DeleteViewLevels()" )
hiSetBindKey( "Layout"  "<Ctrl>2" "DeleteViewLevels()" )
hiSetBindKey( "Layout"  "<Ctrl>3" "DeleteViewLevels()" )
hiSetBindKey( "Layout"  "<Ctrl>4" "DeleteViewLevels()" )
hiSetBindKey( "Layout"  "<Ctrl>5" "DeleteViewLevels()" )
hiSetBindKey( "Layout"  "<Ctrl>6" "DeleteViewLevels()" )
hiSetBindKey( "Layout"  "<Ctrl>7" "DeleteViewLevels()" )
hiSetBindKey( "Layout"  "<Ctrl>8" "DeleteViewLevels()" )

hiSetBindKey( "Layout"  "<Shift>w"   "ResizeWindow( \"half\" )" )
hiSetBindKey( "Layout"  "<Ctrl>i"   "IconifyWindow( )" )
hiSetBindKey( "Layout"  "<Shift>i" "DeiconifyWindow( )" )

hiSetBindKey( "Layout"  "<Shift>Left"  "MoveToSnap( \"L\" )" )
hiSetBindKey( "Layout"  "<Shift>Right" "MoveToSnap( \"R\" )" )
hiSetBindKey( "Layout"  "<Shift>Up"    "MoveToSnap( \"T\" )" )
hiSetBindKey( "Layout"  "<Shift>Down"  "MoveToSnap( \"B\" )" )

hiSetBindKey( "Layout"  "<Ctrl>Left"  "MoveToSnapAlignV( \"L\" )" )
hiSetBindKey( "Layout"  "<Ctrl>Right" "MoveToSnapAlignV( \"R\" )" )
hiSetBindKey( "Layout"  "<Ctrl>Up"    "MoveToSnapAlignV( \"T\" )" )
hiSetBindKey( "Layout"  "<Ctrl>Down"  "MoveToSnapAlignV( \"B\" )" )
hiSetBindKey( "Layout"  "Ctrl Shift <Key>Left"  "MoveToSnapAlignV( \"L\" )" )
hiSetBindKey( "Layout"  "Ctrl Shift <Key>Right" "MoveToSnapAlignV( \"R\" )" )
hiSetBindKey( "Layout"  "Ctrl Shift <Key>Up"    "MoveToSnapAlignV( \"T\" )" )
hiSetBindKey( "Layout"  "Ctrl Shift <Key>Down"  "MoveToSnapAlignV( \"B\" )" )

hiSetBindKey( "Layout"  "<Ctrl>s"  "SelectSameMaster( )" )

;hiSetBindKey( "Layout"  "<Key>v"  "AlignCellsV( 0 0 0 nil )" )
hiSetBindKey( "Layout"  "<Ctrl>v"  "hiDisplayForm( trAlignCellsVForm )" )

hiSetBindKey( "Layout"  "Ctrl Shift <Key>p"  "Floorplan( )" )

hiSetBindKey( "Layout"  "Alt Ctrl <Key>p"   "TR_FloorplanArray( )" )
hiSetBindKey( "Layout"  "Alt <Key>p"        "ArrayCells( )" )

hiSetBindKey( "Layout"  "<Key>n"  "ShowNameInstance( )" )
hiSetBindKey( "Layout"  "<Ctrl>n"  "ShowNameMaster( )" )

hiSetBindKey( "Layout" "<Key>F6" "hiDisplayForm( A )" )

hiSetBindKey( "Layout" "<Ctrl>u" "dbFlipInstanceUD( )" )
;hiSetBindKey( "Layout" "<Ctrl>l" "dbFlipInstanceLR( )" )

hiSetBindKey( "Layout" "<Shift>u" "DB_FlipGroup( \"UD\" )" )
hiSetBindKey( "Layout" "<Shift>l" "DB_FlipGroup( \"LR\" )" )

; SetSnapping
; ===========


; MoveAndSnap
; ===========
(hiSetBindKey "Layout"  "Alt<Key>Left"  "(MoveAndSnap \"L\" )" )
(hiSetBindKey "Layout"  "Alt<Key>Right" "(MoveAndSnap \"R\" )" )
(hiSetBindKey "Layout"  "Alt<Key>Up"    "(MoveAndSnap \"U\" )" )
(hiSetBindKey "Layout"  "Alt<Key>Down"  "(MoveAndSnap \"D\" )" )
(hiSetBindKey "Layout"  "Alt<Key>z"     "(MoveAndSnap \"L\" )" )
(hiSetBindKey "Layout"  "Alt<Key>c"     "(MoveAndSnap \"R\" )" )
(hiSetBindKey "Layout"  "Alt<Key>s"     "(MoveAndSnap \"U\" )" )
(hiSetBindKey "Layout"  "Alt<Key>x"     "(MoveAndSnap \"D\" )" )
(hiSetBindKey "Layout"  "<Key>F5"    "TR_MoveAndSnap( )" )

; vi-style cell movement bindings
(hiSetBindKey "Layout"  "Ctrl<Key>h" "(MoveAndSnap \"L\" )" )
(hiSetBindKey "Layout"  "Ctrl<Key>l" "(MoveAndSnap \"R\" )" )
(hiSetBindKey "Layout"  "Ctrl<Key>k" "(MoveAndSnap \"U\" )" )
(hiSetBindKey "Layout"  "Ctrl<Key>j" "(MoveAndSnap \"D\" )" )


hiSetBindKey( "Layout"  "Shift<Key>l"        "AddHiLiteLabels( )" )
;hiSetBindKey( "Layout"  "Ctrl <Key>l"        "ZoomToHiLiteLabels( )" )
hiSetBindKey( "Layout"  "Ctrl Shift <Key>l"   "TR_AddHiLiteLabels( )" )

hiSetBindKey( "Layout"  "<Key>F4"       "TR_FindInstance( )" )

hiSetBindKey( "Layout"  "Ctrl <Key>p"  "TR_Floorplanner( )" )


; CrawFish Module
; ===============

hiSetBindKey( "Layout"  "Ctrl <Key>q"       "TR_AssuraLVS( )" )

; olap
; ===============
hiSetBindKey( "Layout"  "Ctrl Shift<Key>o"       "FL_Overlap( )" )


; Enhancement
; ===========
hiSetBindKey( "Layout"  "Alt <Key>q"       "TR_ChangeInstNameProcGUI( )" )

)
)

FulcrumSetup()
