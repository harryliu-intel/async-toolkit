(* Roman Parise - WCMTcamBlock *)
MODULE WCMTcamBlock ;

(*************)
(** Imports **)
(*************)
IMPORT Tcam ;
IMPORT Word ;

(************************)
(** Visible Procedures **)
(************************)

PROCEDURE GetOutHits( TcamBlock : REF T ) : REF ARRAY OF BOOLEAN =
VAR
	outhits := Tcam.LookupInTcamSlice( GetSearchKey( TcamBlock ) , TcamBlock.BlockTcamSlice ) ;
BEGIN
	<* ASSERT TcamBlock # NIL *>
	<* ASSERT TcamBlock.BlockInHits # NIL *>
	<* ASSERT TcamBlock.BlockProfile # NIL *>
	<* ASSERT outhits # NIL *>
	<* ASSERT NUMBER( TcamBlock.BlockInHits^ ) = NUMBER( outhits^ ) *>
	<* ASSERT FIRST( TcamBlock.BlockInHits^ ) = FIRST( outhits^ ) *>
	FOR outhits_index := FIRST( outhits^ ) TO LAST( outhits^ ) DO
		outhits[ outhits_index ] := outhits[ outhits_index ] AND ( TcamBlock.BlockProfile.StartCompare^ OR TcamBlock.BlockInHits[ outhits_index ] ) ;
	END ;
	RETURN outhits ;
END GetOutHits ;

PROCEDURE GetHitIndexValid( TcamBlock : REF T ) : BOOLEAN =
VAR
	outhits := GetOutHits( TcamBlock ) ;
BEGIN
	<* ASSERT TcamBlock # NIL *>
	<* ASSERT outhits # NIL *>
	FOR outhits_index := FIRST( outhits^ ) TO LAST( outhits^ ) DO
		IF outhits[ outhits_index ] = TRUE THEN
			RETURN TRUE ;
		END ;
	END ;
	RETURN FALSE ;
END GetHitIndexValid ;

PROCEDURE GetHitIndex( TcamBlock : REF T ) : CARDINAL RAISES { NoHitsException } =
VAR
	outhits := GetOutHits( TcamBlock ) ;
BEGIN
	<* ASSERT TcamBlock # NIL *>
	<* ASSERT outhits # NIL *>
	FOR outhits_index := LAST( outhits^ ) TO FIRST( outhits^ ) BY -1 DO
		IF outhits[ outhits_index ] = TRUE THEN
			RETURN outhits_index ;
		END ;
	END ;
	RAISE NoHitsException( "Cannot get TCAM hit index since no hits in TCAM." ) ; 
END GetHitIndex ;

(***********************)
(** Hidden Procedures **)
(***********************)

(* GetSearchKey
TcamBlock - a reference to the TCAM block of interest
Get the TCAM search key via key muxing. The mapper provides
this block with Key8, Key16, Key32. It also provides the block
with a profile, which tells us what the mux selects are.
This procedure uses that information to determine what
the search string is.
*)
PROCEDURE GetSearchKey( TcamBlock : REF T ) : Tcam.KeyString =
CONST
	bits_in_byte = 8 ;
	mux_size = KEY8_LENGTH + KEY16_LENGTH + KEY32_LENGTH ;
	mux_size_most_sig = KEY8_LENGTH ;
	num_muxes = 4 ;
VAR
	(* Mux inputs *)
	mux_in : ARRAY[0..num_muxes-1] OF ARRAY[0..mux_size-1] OF BITS bits_in_byte FOR [16_00..16_FF] ;
	mux_in_most_sig : ARRAY[0..mux_size_most_sig-1] OF BITS bits_in_byte FOR [16_00..16_FF] ;
	(* Mux input assignment counters *)
	mux_in_assigned := 0 ;
	(* Mux outputs *)
	mux_outs : ARRAY[0..(num_muxes+1)-1] OF BITS bits_in_byte FOR [16_00..16_FF] ;
	(* Tcam search key to return *)
	search_key : Tcam.KeyString := 16_0000000000 ;
BEGIN

	(* Configure mux inputs *)

	(* Muxes 0 - 3 *)
	(* Take one byte from each of the available keys *)
	<* ASSERT TcamBlock # NIL *>
	<* ASSERT TcamBlock.BlockKeys.Key32 # NIL *>
	<* ASSERT TcamBlock.BlockKeys.Key16 # NIL *>
	<* ASSERT TcamBlock.BlockKeys.Key8 # NIL *>
	<* ASSERT FIRST( TcamBlock.BlockKeys.Key32^ ) = FIRST( TcamBlock.BlockKeys.Key16^ ) *>
	<* ASSERT FIRST( TcamBlock.BlockKeys.Key32^ ) = FIRST( TcamBlock.BlockKeys.Key8^ ) *>
	<* ASSERT mux_size = NUMBER( TcamBlock.BlockKeys.Key32^ ) + NUMBER( TcamBlock.BlockKeys.Key16^ ) + NUMBER( TcamBlock.BlockKeys.Key8^ ) *>
	FOR mux_in_index := FIRST( mux_in ) TO LAST( mux_in ) DO
		FOR key16index := FIRST( TcamBlock.BlockKeys.Key16^ ) TO LAST( TcamBlock.BlockKeys.Key16^ ) DO
			mux_in[ mux_in_index ][ FIRST( mux_in[ mux_in_index ] ) + mux_in_assigned ] := Word.Extract( TcamBlock.BlockKeys.Key16[ key16index ] , ( bits_in_byte * mux_in_index ) MOD 16 , bits_in_byte ) ;
			INC( mux_in_assigned ) ;
		END ;
		FOR key8index := FIRST( TcamBlock.BlockKeys.Key8^ ) TO LAST( TcamBlock.BlockKeys.Key8^ ) DO
			mux_in[ mux_in_index ][ FIRST( mux_in[ mux_in_index ] ) + mux_in_assigned ] := TcamBlock.BlockKeys.Key8[ key8index ] ;
			INC( mux_in_assigned ) ;
		END ;
		FOR key32index := FIRST( TcamBlock.BlockKeys.Key32^ ) TO LAST( TcamBlock.BlockKeys.Key32^ ) DO
			mux_in[ mux_in_index ][ FIRST( mux_in[ mux_in_index ] ) + mux_in_assigned ] := Word.Extract( TcamBlock.BlockKeys.Key32[ key32index ] , bits_in_byte * mux_in_index , bits_in_byte ) ;
			INC( mux_in_assigned ) ;
		END ;
		mux_in_assigned := 0 ;
	END ;

	(* Most significant mux *)
	(* Only assign inputs from Key8 *)
	<* ASSERT mux_size_most_sig = NUMBER( TcamBlock.BlockKeys.Key8^ ) *>
	<* ASSERT FIRST( TcamBlock.BlockKeys.Key8^ ) = FIRST( mux_in_most_sig ) *>
	FOR key8index := FIRST( TcamBlock.BlockKeys.Key8^ ) TO LAST( TcamBlock.BlockKeys.Key8^ ) DO
		mux_in_most_sig[ key8index ] := TcamBlock.BlockKeys.Key8[ key8index ] ;
	END ;

	(* Get outputs using mux selects *)
	<* ASSERT TcamBlock.BlockProfile # NIL *>
	mux_outs[ FIRST( mux_outs ) ] := mux_in[ FIRST( mux_in ) ][ TcamBlock.BlockProfile.MuxSelects.Select0 ] ;
	mux_outs[ FIRST( mux_outs ) + 1 ] := mux_in[ FIRST( mux_in ) + 1 ][ TcamBlock.BlockProfile.MuxSelects.Select1 ] ;
	mux_outs[ FIRST( mux_outs ) + 2 ] := mux_in[ FIRST( mux_in ) + 2 ][ TcamBlock.BlockProfile.MuxSelects.Select2 ] ;
	mux_outs[ FIRST( mux_outs ) + 3 ] := mux_in[ FIRST( mux_in ) + 3 ][ TcamBlock.BlockProfile.MuxSelects.Select3 ] ;
	mux_outs[ LAST( mux_outs ) ] := mux_in_most_sig[ TcamBlock.BlockProfile.MuxSelects.SelectTop ] ;
	
	(* Construct the Tcam search key *)
	FOR byte_in_search_key_index := 0 TO num_muxes DO
		search_key := Word.Insert( search_key , mux_outs[ FIRST( mux_outs ) + byte_in_search_key_index ] , bits_in_byte * byte_in_search_key_index , bits_in_byte ) ;
	END ;
	RETURN search_key ;
END GetSearchKey ;

BEGIN END WCMTcamBlock.
