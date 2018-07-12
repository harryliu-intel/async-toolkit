(* Roman Parise *)
(* High-level model of a TCAM *)

MODULE Tcam;
FROM Word IMPORT Shift ;
FROM Word IMPORT And ;
FROM Word IMPORT Xor ;

PROCEDURE MakeSlice( NumEntries : [1..LAST(INTEGER)] ;
                     Log2EntriesPerChunk : [1..LAST(INTEGER)] := 1 ;
                     ChunkMask : REF ARRAY OF BOOLEAN := NIL ;
                     Entries : REF ARRAY OF Entry := NIL ) :
		     TcamSlice
		     RAISES { InvalidTCAMEntryConfiguration , InvalidTCAMChunkConfiguration } =
VAR
	slice_to_return : TcamSlice ;
	entries_per_chunk : [1..LAST(INTEGER)] := Shift( 1 , Log2EntriesPerChunk ) ;
	number_of_chunks : [1..LAST(INTEGER)] ;
BEGIN

	(* Assigning NumEntries *)
	slice_to_return.NumEntries := NumEntries ;

	(* Assigning Log2EntriesPerChunk *)
	IF entries_per_chunk > NumEntries THEN
		(* "Too many entries per chunk" *)
		RAISE InvalidTCAMChunkConfiguration ;
	ELSIF NumEntries MOD entries_per_chunk # 0 THEN
		(* "Non-integer number of chunks in TCAM slice" *)
		RAISE InvalidTCAMChunkConfiguration ;
	ELSE
		slice_to_return.Log2EntriesPerChunk := Log2EntriesPerChunk ;
	END ;
	
	(* Assigning ChunkMask *)
	number_of_chunks := NumEntries DIV entries_per_chunk ;

	IF ChunkMask = NIL THEN
		(* User didn't assign a chunk mask *)
		slice_to_return.ChunkMask := NEW( REF ARRAY OF BOOLEAN , number_of_chunks ) ;
		FOR ChunkMaskIndex := FIRST( ( slice_to_return.ChunkMask )^ ) TO LAST( ( slice_to_return.ChunkMask )^ ) DO
			slice_to_return.ChunkMask[ ChunkMaskIndex ] := TRUE ;
		END ;
	ELSIF NUMBER( ChunkMask^ ) = number_of_chunks THEN
		(* User assigned a chunk mask of valid length *)
		slice_to_return.ChunkMask := ChunkMask ;
	ELSE
		(* "User assigned a chunk mask of invalid length" *)
		RAISE InvalidTCAMChunkConfiguration ;
	END ;

	(* Assigning Entries *)
	IF Entries = NIL THEN
		(* User didn't provide the entries *)
		slice_to_return.Entries := NEW( REF ARRAY OF Entry , NumEntries ) ;
	ELSIF NUMBER( Entries^ ) # NumEntries THEN
		(* "User provided Entries array of invalid length" *)
		RAISE InvalidTCAMEntryConfiguration ;
	ELSE
		(* User provided Entries array of valid length *)
		slice_to_return.Entries := Entries ;
	END ;

	RETURN slice_to_return ;

END MakeSlice ;

PROCEDURE LookupInTcamSlice( search : KeyString ;
			     slice : TcamSlice ) :
		  	     REF ARRAY OF BOOLEAN =
VAR
	entries_per_chunk : [1..LAST(INTEGER)] := Shift( 1 , slice.Log2EntriesPerChunk ) ;
	array_to_return : REF ARRAY OF BOOLEAN ;
	tcam_chunk : INTEGER := 0 ;
	tcam_index : INTEGER := 0 ;
	key_check_mask : KeyString := 0 ;
BEGIN

	(* Initialize the array to no hits *)
	array_to_return := NEW( REF ARRAY OF BOOLEAN , slice.NumEntries ) ;
	FOR hit_clear_index := FIRST( array_to_return^ ) TO LAST( array_to_return^ ) DO
		array_to_return[ hit_clear_index ] := FALSE ;
	END ;

	(* Go through each TCAM entry... *)
	WHILE tcam_index < slice.NumEntries DO

		(* If the chunk mask for the entry's chunk is enabled... *)
		tcam_chunk := Shift( tcam_index , -1 * slice.Log2EntriesPerChunk ) ;

		IF slice.ChunkMask[ tcam_chunk ] = TRUE THEN

			(* The entry is actually valid... *)
			IF slice.Entries[ tcam_index ].Valid = TRUE THEN

				(* And the entry matches the search... *)
				key_check_mask := Xor( slice.Entries[ tcam_index ].Key , slice.Entries[ tcam_index ].KeyInvert ) ;

				IF ( And( slice.Entries[ tcam_index ].Key , slice.Entries[ tcam_index ].KeyInvert ) = 0 ) AND
				   ( And( slice.Entries[ tcam_index ].Key , key_check_mask ) = And( search , key_check_mask ) ) THEN

					(* Report a HIT! *)
					array_to_return[ tcam_index ] := TRUE ;

				END ;
			END ;
			INC( tcam_index ) ;
		ELSE
			INC( tcam_index , entries_per_chunk ) ;
		END ;

	END ;

	RETURN array_to_return ;

END LookupInTcamSlice ;

BEGIN END Tcam.
