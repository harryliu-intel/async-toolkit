(* Roman Parise - WCMTest *)
MODULE WCMTest ;

IMPORT WCM ;
IMPORT WCMTcamBlock ;
IMPORT Tcam ;
IMPORT IO ;
IMPORT Fmt ;
IMPORT Word ;

(************************)
(** Visible Procedures **)
(************************)

PROCEDURE FullTest( ) =
VAR
	(* For TCAM block test *)
	myouthits : REF ARRAY OF BOOLEAN := NIL ;
	myhitindexvalid := FALSE ;
	myhitindex : CARDINAL := 0 ;
	(* For WCM group test *)
	all_blocks := NEW( REF ARRAY OF REF WCMTcamBlock.T , WCM.NUM_SLICES ) ;
	action_ram_banks := NEW( REF ARRAY OF REF ARRAY OF WCM.ActionRAMEntry , WCM.NUM_ACTION_RAMS ) ;
	aram_cfg : WCM.ActionRAMCfg ;
	wcm_group : REF WCM.T := NIL ;
	winner : WCM.ActionRAMEntry ;
	inhits : REF ARRAY OF BOOLEAN := NIL ;
	mykeys : REF WCMTcamBlock.Keys := NIL ;
	myprofile : REF WCM.WCMGroupProfile := NIL ;
BEGIN
	IO.Put( "WCM Group Test\n" ) ;
	IO.Put( "==============\n" ) ;
	IO.Put( "All errors will be printed to the terminal.\n" ) ;
	IO.Put( "If nothing appears, your test worked!\n" ) ;
	IO.Put( "*** Test 1 - TCAM Block Test ***\n" ) ;
	(* Build an array of TcamBlocks - all default except the 0th slice *)
	FOR block_index := FIRST( all_blocks^ ) TO LAST( all_blocks^ ) DO
		all_blocks[ block_index ] := MakeTcamBlock( ) ;
	END ;
	(* Check outhits, index valid, and hit index *)
	myouthits := WCMTcamBlock.GetOutHits( all_blocks[ FIRST( all_blocks^ ) ] ) ;
	myhitindexvalid := WCMTcamBlock.GetHitIndexValid( all_blocks[ FIRST( all_blocks^ ) ] ) ;
	TRY
		myhitindex := WCMTcamBlock.GetHitIndex( all_blocks[ FIRST( all_blocks^ ) ] ) ;
	EXCEPT
		| WCMTcamBlock.NoHitsException( strerr ) => IO.Put( strerr & "\n" ) ;
	END ;
	FOR myouthits_index := FIRST( myouthits^ ) TO LAST( myouthits^ ) DO
		IF ( myouthits_index = 0 AND myouthits[ myouthits_index ] # TRUE ) OR
		   ( myouthits_index # 0 AND myouthits[ myouthits_index ] # FALSE ) THEN
			IO.Put( "Incorrect hit result for entry " & Fmt.Int( myouthits_index ) & "\n" ) ;
		END ;
	END ;
	IF myhitindex # 0 THEN
		IO.Put( "Hit index is incorrect!\n" ) ;
	END ;
	IF myhitindexvalid # TRUE THEN
		IO.Put( "Hit index valid is incorrect!\n" ) ;
	END ;
	IO.Put( "*** Test 2 - WCM Winning Action ***\n" ) ;
	(* Except each action in the first RAM entry is simply the number of the action RAM bank.
	0 elsewhere. *)
	FOR bank_index := FIRST( action_ram_banks^ ) TO LAST( action_ram_banks^ ) DO
		action_ram_banks[ bank_index ] := NEW( REF ARRAY OF WCM.ActionRAMEntry , WCM.NUM_ENTRIES ) ;
		FOR entry_index := FIRST( action_ram_banks[ bank_index ]^ ) TO LAST( action_ram_banks[ bank_index ]^ ) DO
			FOR action_index := FIRST( action_ram_banks[ bank_index ][ entry_index ] ) TO LAST( action_ram_banks[ bank_index ][ entry_index ] ) DO
				IF entry_index = 0 THEN
					action_ram_banks[ bank_index ][ entry_index ][ action_index ] := bank_index ;
				ELSE
					action_ram_banks[ bank_index ][ entry_index ][ action_index ] := 0 ;
				END ;
			END ;
		END ;
	END ;
	(* Build ARAMCfg register - all blocks map to the 0th slice and are enabled *)
	FOR bank_index := FIRST( aram_cfg.Enable ) TO LAST( aram_cfg.Enable ) DO
		aram_cfg.Enable[ bank_index ] := TRUE ;
	END ;
	FOR bank_index := FIRST( aram_cfg.SliceIndex ) TO LAST( aram_cfg.SliceIndex ) DO
		aram_cfg.SliceIndex[ bank_index ] := 0 ;
	END ;
	(* Build a WCMGroup *)
	mykeys := MakeKeys( ) ;
	inhits := MakeInHits( ) ;
	myprofile := MakeProfile( ) ;
	wcm_group := WCM.MakeWCMGroup( all_blocks , action_ram_banks , aram_cfg , mykeys , inhits , myprofile ) ;
	(* The top one should generate the winning action *)
	TRY
		winner := WCM.GetWinningAction( wcm_group ) ;
		FOR action_index := FIRST( winner ) TO LAST( winner ) DO
			IF winner[ action_index ] # LAST( wcm_group^.TcamBlock^ ) THEN
				IO.Put( "Incorrect winning action!\n" ) ;
			END ;
		END ;
	EXCEPT
		| WCM.NoActionException( msg ) =>
			EVAL msg ;
			IF NUMBER( wcm_group^.ActionRAM^ ) # 0 THEN
				IO.Put( "No action generated!\n" ) ;
			END ;
	END ;
	(* Now, disable the top one *)
	(* The second one from the top should generate the winning action *)
	IF NUMBER( wcm_group^.ActionRAM^ ) # 0 THEN
		wcm_group^.ARAMCfg.Enable[ LAST( wcm_group^.ARAMCfg.Enable ) ] := FALSE ;
		TRY
			winner := WCM.GetWinningAction( wcm_group ) ;
			FOR action_index := FIRST( winner ) TO LAST( winner ) DO
				IF winner[ action_index ] # LAST( wcm_group^.TcamBlock^ ) - 1 THEN
					IO.Put( "Incorrect winning action!\n" ) ;
				END ;
			END ;
		EXCEPT
			| WCM.NoActionException( msg ) =>
				EVAL msg ;
				IF NUMBER( wcm_group^.ActionRAM^ ) # 1 THEN
					IO.Put( "No action generated!\n" ) ;
				END ;
		END ;
	END ;
END FullTest ;

(***********************)
(** Hidden Procedures **)
(***********************)

(* Create the default TCAM block for this test...
- Key8's are all 1's except the first, which is all 0's.
- Key16's are all 1's except the second, which is all 0s.
- Key32's are all 1's except the third, which is all 0's.
- StartCompare is true. So, it's the beginning of a cascade.
- SelectTop is all 0s.
- Select0 is 00...001.
- Select1 is whatever the length of all Key16s is.
- Select2 is the length of Key16 + length of Key8 + 2.
- Select3 is 00...001.
- All input hits are false except the second one is true.
- First TCAM entry is all 0s. The others are all 1s.
- All TCAM entries are valid.
- All chunks are enabled
*)
PROCEDURE MakeTcamBlock( ) : REF WCMTcamBlock.T =
VAR
	mykeys := NEW( REF WCMTcamBlock.Keys ) ;
	block_profile := NEW( REF WCMTcamBlock.Profile ) ;
	myprofile := NEW( REF WCM.WCMGroupProfile ) ;
	inhits : REF ARRAY OF BOOLEAN := NIL ;
	entries := NEW( REF ARRAY OF Tcam.Entry , WCMTcamBlock.NUM_ENTRIES ) ;
	log2entriesperchunk := 6 ;
	entriesperchunk := Word.LeftShift( 1 , log2entriesperchunk ) ;
	chunkmask := NEW( REF ARRAY OF BOOLEAN , WCMTcamBlock.NUM_ENTRIES DIV entriesperchunk ) ;
	block := NEW( REF WCMTcamBlock.T ) ;
BEGIN
	(* Keys record *)
	mykeys := MakeKeys( ) ;
	(* Profile record *)
	myprofile := MakeProfile( ) ;
	block_profile.StartCompare := NEW( REF BOOLEAN ) ;
	block_profile.StartCompare^ := TRUE ;
	block_profile.MuxSelects := myprofile.MuxSelects ;
	(* Inhits *)
	inhits := MakeInHits( ) ;
	(* Tcam entries *)
	FOR entry_index := FIRST( entries^ ) TO LAST( entries^ ) DO
		IF entry_index = FIRST( entries^ ) THEN
			entries[ entry_index ].Key := 16_0000000000 ;
			entries[ entry_index ].KeyInvert := 16_ffffffffff ;
		ELSE
			entries[ entry_index ].Key := 16_ffffffffff ;
			entries[ entry_index ].KeyInvert := 16_0000000000 ;
		END ;
		entries[ entry_index ].Valid := TRUE ;
	END ;
	FOR cm_index := FIRST( chunkmask^ ) TO LAST( chunkmask^ ) DO
		chunkmask[ cm_index ] := TRUE ;
	END ;
	(* Set values *)
	TRY
		block^.BlockTcamSlice := Tcam.MakeSlice( NUMBER( entries^ ) , log2entriesperchunk , chunkmask , entries ) ;
	EXCEPT
		| Tcam.InvalidTCAMChunkConfiguration( errstr ) => IO.Put( errstr & "\n" ) ;
		| Tcam.InvalidTCAMEntryConfiguration( errstr ) => IO.Put( errstr & "\n" ) ;
	END ;
	block^.BlockKeys := mykeys ;
	block^.BlockProfile := block_profile ;
	block^.BlockInHits := inhits ;
	RETURN block ;
END MakeTcamBlock ;

PROCEDURE MakeInHits( ) : REF ARRAY OF BOOLEAN =
VAR
	inhits := NEW( REF ARRAY OF BOOLEAN , WCMTcamBlock.NUM_ENTRIES ) ;
BEGIN
	FOR inhits_index := FIRST( inhits^ ) TO LAST( inhits^ ) DO
		IF inhits_index # FIRST( inhits^ ) + 1 THEN
			inhits[ inhits_index ] := FALSE ;
		ELSE
			inhits[ inhits_index ] := TRUE ;
		END ;
	END ;
	RETURN inhits ;
END MakeInHits ;

PROCEDURE MakeKeys( ) : REF WCMTcamBlock.Keys =
VAR
	mykeys := NEW( REF WCMTcamBlock.Keys ) ;
BEGIN
	mykeys.Key8 := NEW( REF ARRAY OF BITS 8 FOR [16_00..16_FF] , WCMTcamBlock.KEY8_LENGTH ) ;
	FOR key8index := FIRST( mykeys.Key8^ ) TO LAST( mykeys.Key8^ ) DO
		IF key8index # FIRST( mykeys.Key8^ ) THEN
			mykeys.Key8[ key8index ] := 16_FF ;
		ELSE
			mykeys.Key8[ key8index ] := 16_00 ;
		END ;
	END ;
	mykeys.Key16 := NEW( REF ARRAY OF BITS 16 FOR [16_00..16_FFFF] , WCMTcamBlock.KEY16_LENGTH ) ;
	FOR key16index := FIRST( mykeys.Key16^ ) TO LAST( mykeys.Key16^ ) DO
		IF key16index # FIRST( mykeys.Key16^ ) + 1 THEN
			mykeys.Key16[ key16index ] := 16_FFFF ;
		ELSE
			mykeys.Key16[ key16index ] := 16_0000 ;
		END ;
	END ;
	mykeys.Key32 := NEW( REF ARRAY OF BITS 32 FOR [16_00000000..16_FFFFFFFF] , WCMTcamBlock.KEY32_LENGTH ) ;
	FOR key32index := FIRST( mykeys.Key32^ ) TO LAST( mykeys.Key32^ ) DO
		IF key32index # FIRST( mykeys.Key16^ ) + 2 THEN
			mykeys.Key32[ key32index ] := 16_FFFFFFFF ;
		ELSE
			mykeys.Key32[ key32index ] := 16_00000000 ;
		END ;
	END ;
	RETURN mykeys ;
END MakeKeys ;

PROCEDURE MakeProfile( ) : REF WCM.WCMGroupProfile =
VAR
	myprofile := NEW( REF WCM.WCMGroupProfile ) ;
BEGIN
	myprofile.StartCompare := NEW( REF ARRAY OF REF BOOLEAN , WCM.NUM_SLICES ) ;
	FOR startcompare_index := FIRST( myprofile.StartCompare^ ) TO LAST( myprofile.StartCompare^ ) DO
		myprofile.StartCompare[ startcompare_index ] := NEW( REF BOOLEAN ) ;
		myprofile.StartCompare[ startcompare_index ]^ := TRUE ;
	END ;
	myprofile.MuxSelects := NEW( REF WCMTcamBlock.KeyMuxSelect ) ;
	myprofile.MuxSelects.SelectTop := 16_00 ;
	myprofile.MuxSelects.Select0 := 16_01 ;
	myprofile.MuxSelects.Select1 := WCMTcamBlock.KEY16_LENGTH ;
	myprofile.MuxSelects.Select2 := WCMTcamBlock.KEY16_LENGTH + WCMTcamBlock.KEY8_LENGTH + 2 ;
	myprofile.MuxSelects.Select3 := 1 ;
	RETURN myprofile ;
END MakeProfile ;

BEGIN END WCMTest.
