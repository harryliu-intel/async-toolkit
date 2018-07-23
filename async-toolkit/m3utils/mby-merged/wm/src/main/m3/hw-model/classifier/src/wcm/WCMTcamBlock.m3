MODULE WCMTcamBlock ;

(*************)
(** Imports **)
(*************)
IMPORT Tcam ;
IMPORT Word ;

(************************)
(** Visible Procedures **)
(************************)

PROCEDURE GetOutHits( WCMGroup : REF T ) : REF ARRAY OF BOOLEAN =
VAR
	outhits := Tcam.LookupInTcamSlice( GetSearchKey( WCMGroup ) , WCMGroup.BlockTcamSlice ) ;
BEGIN
	<* ASSERT WCMGroup # NIL *>
	<* ASSERT WCMGroup.BlockInHits # NIL *>
	<* ASSERT outhits # NIL *>
	<* ASSERT NUMBER( WCMGroup.BlockInHits^ ) = NUMBER( outhits^ ) *>
	<* ASSERT FIRST( WCMGroup.BlockInHits^ ) = FIRST( outhits^ ) *>
	FOR outhits_index := FIRST( outhits^ ) TO LAST( outhits^ ) DO
		outhits[ outhits_index ] := outhits[ outhits_index ] AND ( WCMGroup.BlockProfile.StartCompare OR WCMGroup.BlockInHits[ outhits_index ] ) ;
	END ;
	RETURN outhits ;
END GetOutHits ;

PROCEDURE GetHitIndexValid( WCMGroup : REF T ) : BOOLEAN =
VAR
	outhits := GetOutHits( WCMGroup ) ;
BEGIN
	<* ASSERT WCMGroup # NIL *>
	<* ASSERT outhits # NIL *>
	FOR outhits_index := FIRST( outhits^ ) TO LAST( outhits^ ) DO
		IF outhits[ outhits_index ] = TRUE THEN
			RETURN TRUE ;
		END ;
	END ;
	RETURN FALSE ;
END GetHitIndexValid ;

PROCEDURE GetHitIndex( WCMGroup : REF T ) : CARDINAL RAISES { NoHitsException } =
VAR
	outhits := GetOutHits( WCMGroup ) ;
BEGIN
	<* ASSERT WCMGroup # NIL *>
	<* ASSERT outhits # NIL *>
	FOR outhits_index := FIRST( outhits^ ) TO LAST( outhits^ ) DO
		IF outhits[ outhits_index ] = TRUE THEN
			RETURN outhits_index ;
		END ;
	END ;
	RAISE NoHitsException( "Cannot get TCAM hit index since no hits in TCAM." ) ; 
END GetHitIndex ;

(***********************)
(** Hidden Procedures **)
(***********************)

PROCEDURE GetSearchKey( WCMGroup : REF T ) : Tcam.KeyString =
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
	<* ASSERT WCMGroup # NIL *>
	<* ASSERT WCMGroup.BlockKeys.Key32 # NIL *>
	<* ASSERT WCMGroup.BlockKeys.Key16 # NIL *>
	<* ASSERT WCMGroup.BlockKeys.Key8 # NIL *>
	<* ASSERT FIRST( WCMGroup.BlockKeys.Key32^ ) = FIRST( WCMGroup.BlockKeys.Key16^ ) *>
	<* ASSERT FIRST( WCMGroup.BlockKeys.Key32^ ) = FIRST( WCMGroup.BlockKeys.Key8^ ) *>
	<* ASSERT mux_size = NUMBER( WCMGroup.BlockKeys.Key32^ ) + NUMBER( WCMGroup.BlockKeys.Key16^ ) + NUMBER( WCMGroup.BlockKeys.Key8^ ) *>
	FOR mux_in_index := FIRST( mux_in ) TO LAST( mux_in ) DO
		FOR key16index := FIRST( WCMGroup.BlockKeys.Key16^ ) TO LAST( WCMGroup.BlockKeys.Key16^ ) DO
			mux_in[ mux_in_index ][ FIRST( mux_in[ mux_in_index ] ) + mux_in_assigned ] := Word.Extract( WCMGroup.BlockKeys.Key16[ key16index ] , ( bits_in_byte * mux_in_index ) MOD 16 , bits_in_byte ) ;
			INC( mux_in_assigned ) ;
		END ;
		FOR key8index := FIRST( WCMGroup.BlockKeys.Key8^ ) TO LAST( WCMGroup.BlockKeys.Key8^ ) DO
			mux_in[ mux_in_index ][ FIRST( mux_in[ mux_in_index ] ) + mux_in_assigned ] := WCMGroup.BlockKeys.Key8[ key8index ] ;
			INC( mux_in_assigned ) ;
		END ;
		FOR key32index := FIRST( WCMGroup.BlockKeys.Key32^ ) TO LAST( WCMGroup.BlockKeys.Key32^ ) DO
			mux_in[ mux_in_index ][ FIRST( mux_in[ mux_in_index ] ) + mux_in_assigned ] := Word.Extract( WCMGroup.BlockKeys.Key32[ key32index ] , bits_in_byte * mux_in_index , bits_in_byte ) ;
			INC( mux_in_assigned ) ;
		END ;
		mux_in_assigned := 0 ;
	END ;

	(* Most significant mux *)
	(* Only assign inputs from Key8 *)
	<* ASSERT mux_size_most_sig = NUMBER( WCMGroup.BlockKeys.Key8^ ) *>
	<* ASSERT FIRST( WCMGroup.BlockKeys.Key8^ ) = FIRST( mux_in_most_sig ) *>
	FOR key8index := FIRST( WCMGroup.BlockKeys.Key8^ ) TO LAST( WCMGroup.BlockKeys.Key8^ ) DO
		mux_in_most_sig[ key8index ] := WCMGroup.BlockKeys.Key8[ key8index ] ;
	END ;

	(* Get outputs using mux selects *)
	mux_outs[ FIRST( mux_outs ) ] := mux_in[ FIRST( mux_in ) ][ WCMGroup.BlockProfile.Select0 ] ;
	mux_outs[ FIRST( mux_outs ) + 1 ] := mux_in[ FIRST( mux_in ) + 1 ][ WCMGroup.BlockProfile.Select1 ] ;
	mux_outs[ FIRST( mux_outs ) + 2 ] := mux_in[ FIRST( mux_in ) + 2 ][ WCMGroup.BlockProfile.Select2 ] ;
	mux_outs[ FIRST( mux_outs ) + 3 ] := mux_in[ FIRST( mux_in ) + 3 ][ WCMGroup.BlockProfile.Select3 ] ;
	mux_outs[ LAST( mux_outs ) ] := mux_in_most_sig[ WCMGroup.BlockProfile.SelectTop ] ;
	
	(* Construct the Tcam search key *)
	FOR byte_in_search_key_index := 0 TO num_muxes DO
		search_key := Word.Insert( search_key , mux_outs[ FIRST( mux_outs ) + byte_in_search_key_index ] , bits_in_byte * byte_in_search_key_index , bits_in_byte ) ;
	END ;
	RETURN search_key ;
END GetSearchKey ;

BEGIN END WCMTcamBlock.
