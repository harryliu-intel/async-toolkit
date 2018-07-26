(* Roman Parise - WCMTest *)
MODULE WCMTest ;

IMPORT WCMTcamBlock ;
IMPORT Tcam ;
IMPORT IO ;
IMPORT Fmt ;
IMPORT Word ;

PROCEDURE FullTest( ) =
VAR
	block := NEW( REF WCMTcamBlock.T ) ;
	mykeys : WCMTcamBlock.Keys ;
	myprofile : WCMTcamBlock.Profile ;
	inhits := NEW( REF ARRAY OF BOOLEAN , WCMTcamBlock.NUM_ENTRIES ) ;
	entries := NEW( REF ARRAY OF Tcam.Entry , WCMTcamBlock.NUM_ENTRIES ) ;
	myouthits : REF ARRAY OF BOOLEAN := NIL ;
	myhitindexvalid := FALSE ;
	myhitindex : CARDINAL := 0 ;
	log2entriesperchunk := 6 ;
	entriesperchunk := Word.LeftShift( 1 , log2entriesperchunk ) ;
	chunkmask := NEW( REF ARRAY OF BOOLEAN , WCMTcamBlock.NUM_ENTRIES DIV entriesperchunk ) ;
BEGIN
	IO.Put( "WCM Group Test\n" ) ;
	IO.Put( "==============\n" ) ;
	IO.Put( "All errors will be printed to the terminal.\n" ) ;
	IO.Put( "If nothing appears, your test worked!\n" ) ;
	IO.Put( "*** Test 1 - TCAM Block Test ***\n" ) ;
	(* Keys record *)
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
	(* Profile record *)
	myprofile.StartCompare := TRUE ;
	myprofile.SelectTop := 16_00 ;
	myprofile.Select0 := 16_01 ;
	myprofile.Select1 := WCMTcamBlock.KEY16_LENGTH ;
	myprofile.Select2 := WCMTcamBlock.KEY16_LENGTH + WCMTcamBlock.KEY8_LENGTH + 2 ;
	myprofile.Select3 := 1 ;
	(* Inhits record *)
	FOR inhits_index := FIRST( inhits^ ) TO LAST( inhits^ ) DO
		IF inhits_index # FIRST( inhits^ ) + 1 THEN
			inhits[ inhits_index ] := FALSE ;
		ELSE
			inhits[ inhits_index ] := TRUE ;
		END ;
	END ;
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
		block^.BlockTcamSlice := Tcam.MakeSlice( NUMBER( entries^ ) , 6 , chunkmask , entries ) ;
	EXCEPT
		| Tcam.InvalidTCAMChunkConfiguration( errstr ) => IO.Put( errstr & "\n" ) ;
		| Tcam.InvalidTCAMEntryConfiguration( errstr ) => IO.Put( errstr & "\n" ) ;
	END ;
	block^.BlockKeys := mykeys ;
	block^.BlockProfile := myprofile ;
	block^.BlockInHits := inhits ;
	(* Check outhits, index valid, and hit index *)
	myouthits := WCMTcamBlock.GetOutHits( block ) ;
	myhitindexvalid := WCMTcamBlock.GetHitIndexValid( block ) ;
	TRY
		myhitindex := WCMTcamBlock.GetHitIndex( block ) ;
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
END FullTest ;

BEGIN END WCMTest.
