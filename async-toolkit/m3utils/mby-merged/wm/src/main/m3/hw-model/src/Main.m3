(* Roman Parise - TCAM Lookup Test *)

MODULE Main ;
IMPORT IO ;
IMPORT Tcam ;
FROM Word IMPORT Shift ;
VAR
	my_tcam : Tcam.TcamSlice ;
	chunk_mask : REF ARRAY OF BOOLEAN := NIL ;
	log2_entries_per_chunk := 6 ;
	search : Tcam.KeyString := 16_ffffffffff ;
	my_tcam_entries : REF ARRAY OF Tcam.Entry := NIL ;
	my_tcam_entries_temp : REF ARRAY OF Tcam.Entry := NIL ;
	num_chunks := 0 ;
	num_entries : [1..LAST(INTEGER)] := 1024 ;
	result : REF ARRAY OF BOOLEAN := NIL ;
BEGIN

	IO.Put( "TCAM Lookup Test\n" ) ;
	IO.Put( "================\n" ) ;

	(* Make a TCAM and check if the exceptions work *)
	my_tcam_entries := NEW( REF ARRAY OF Tcam.Entry , num_entries ) ;
	FOR Index := FIRST( my_tcam_entries^ ) TO LAST( my_tcam_entries^ ) DO
		my_tcam_entries[ Index ].Key := 16_ffffffffff ;
		my_tcam_entries[ Index ].KeyInvert := 16_ffffffffff ;
		my_tcam_entries[ Index ].Valid := TRUE ;
	END ;
	my_tcam_entries[ 5 ].Key := 16_ffffffffff ;
	my_tcam_entries[ 5 ].KeyInvert := 16_0000000000 ;
	my_tcam_entries[ 5 ].Valid := TRUE ;

	IO.Put( "All errors will be printed to the console.\n" ) ;
	IO.Put( "If none appears, your TCAM works as intended. :)\n" ) ;

	(* Should throw exception when NumEntries is 2 but length of my_tcam_entries is 1024. *)
	TRY
		my_tcam := Tcam.MakeSlice( NumEntries := 2 , Entries := my_tcam_entries ) ;
		IO.Put( "Error: Allocted TCAM with invalid number of entries!\n" ) ;
	EXCEPT
		| Tcam.InvalidTCAMEntryConfiguration => IO.Put( "" ) ;
		| Tcam.InvalidTCAMChunkConfiguration => IO.Put( "Error: Wrong exception thrown A!\n" ) ;
	END ;
	
	TRY
		my_tcam := Tcam.MakeSlice( NumEntries := num_entries , Entries := my_tcam_entries ) ;
	EXCEPT
		| Tcam.InvalidTCAMEntryConfiguration => IO.Put( "Error: Should not throw exception A!\n" ) ;
		| Tcam.InvalidTCAMChunkConfiguration => IO.Put( "Error: Should not throw exception B!\n" ) ;
	END ;

	IO.Put( "*** Test 1 - Simple Lookup ***\n" ) ;
	result := Tcam.LookupInTcamSlice( search , my_tcam ) ;
	FOR Index := FIRST( result^ ) TO LAST( result^ ) DO
		IF NOT ( ( ( Index = 5 ) AND ( result[ Index ] = TRUE ) ) OR
		         ( ( Index # 5 ) AND ( result[ Index ] = FALSE ) ) ) THEN
			IO.Put( "Error! At Entry " ) ;
			IO.PutInt( Index ) ;
			IO.Put( "!\n" ) ;
			IO.Put( "Entry " ) ;
			IO.PutInt( Index ) ;
			IO.Put( " is a " ) ;
			IF result[ Index ] = TRUE THEN
				IO.Put( "hit!\n" ) ;
			ELSE
				IO.Put( "miss!\n" ) ;
			END ;
		END ;
	END ;

	(* Now testing with chunk_mask *)
	num_chunks := num_entries DIV Shift( 1 , log2_entries_per_chunk ) ;
	chunk_mask := NEW( REF ARRAY OF BOOLEAN , num_chunks ) ;
	FOR cm_index := FIRST( chunk_mask^ ) TO LAST( chunk_mask^ ) DO
		chunk_mask[ cm_index ] := FALSE ;
	END ;

	(* Should throw exception when Log2EntriesPerChunk is 12 but length of my_tcam_entries is 1024. *)
	TRY
		my_tcam := Tcam.MakeSlice( NumEntries := num_entries , Log2EntriesPerChunk := 12 , Entries := my_tcam_entries ) ;
		IO.Put( "Error: Allocted TCAM with more entries per chunk than total entries!\n" ) ;
	EXCEPT
		| Tcam.InvalidTCAMChunkConfiguration => IO.Put("") ;
		| Tcam.InvalidTCAMEntryConfiguration => IO.Put( "Error: Wrong exception thrown B!\n" ) ;
	END ;
	(* Should throw exception when Log2EntriesPerChunk is 6 but length of my_tcam_entries_temp is 1025. *)
	my_tcam_entries_temp := NEW( REF ARRAY OF Tcam.Entry , 1025 ) ;
	TRY
		my_tcam := Tcam.MakeSlice( NumEntries := num_entries , Log2EntriesPerChunk := 6 , Entries := my_tcam_entries_temp ) ;
		IO.Put( "Error: Allocted TCAM with non-integer number of chunks!\n" ) ;
	EXCEPT
		| Tcam.InvalidTCAMEntryConfiguration => IO.Put("") ;
		| Tcam.InvalidTCAMChunkConfiguration => IO.Put( "Error: Wrong exception thrown C!\n" ) ;
	END ;
	(* Should throw exception when Log2EntriesPerChunk is 7 but ChunkMask has 16 entries. *)
	TRY
		my_tcam := Tcam.MakeSlice( num_entries , 7 , chunk_mask , my_tcam_entries ) ;
		IO.Put( "Error: Allocted TCAM with mismatch between number of chunks and number of elements of ChunkMask!\n" ) ;
	EXCEPT
		| Tcam.InvalidTCAMChunkConfiguration => IO.Put( "" ) ;
		| Tcam.InvalidTCAMEntryConfiguration => IO.Put( "Error: Wrong exception thrown D!\n" ) ;
	END ;
	TRY
		my_tcam := Tcam.MakeSlice( num_entries , 6 , chunk_mask , my_tcam_entries ) ;
	EXCEPT
		| Tcam.InvalidTCAMEntryConfiguration => IO.Put( "Error: Should not throw exception C!\n" ) ;
		| Tcam.InvalidTCAMChunkConfiguration => IO.Put( "Error: Should not throw exception D!\n" ) ;
	END ;

	IO.Put( "*** Test 2 - Simple Lookup with All Chunks Disabled ***\n" ) ;
	result := Tcam.LookupInTcamSlice( search , my_tcam ) ;
	FOR Index := FIRST( result^ ) TO LAST( result^ ) DO
		IF result[ Index ] = TRUE THEN
			IO.Put( "Error! At Entry " ) ;
			IO.PutInt( Index ) ;
			IO.Put( "!\n" ) ;
			IO.Put( "Entry " ) ;
			IO.PutInt( Index ) ;
			IO.Put( " is a " ) ;
			IF result[ Index ] = TRUE THEN
				IO.Put( "hit!\n" ) ;
			ELSE
				IO.Put( "miss!\n" ) ;
			END ;
		END ;
	END ;

	(* Confirm default entries *)
	TRY
		my_tcam := Tcam.MakeSlice( 512 ) ;
	EXCEPT
		| Tcam.InvalidTCAMEntryConfiguration => IO.Put( "Error: Should not throw exception E!\n" ) ;
		| Tcam.InvalidTCAMChunkConfiguration => IO.Put( "Error: Should not throw exception F!\n" ) ;
	END ;

	FOR EntryIndex := FIRST( my_tcam.Entries^ ) TO LAST( my_tcam.Entries^ ) DO
		IF NOT( my_tcam.Entries[ EntryIndex ].Key = 16_0000000000 AND my_tcam.Entries[ EntryIndex ].KeyInvert = 16_0000000000 AND my_tcam.Entries[ EntryIndex ].Valid = FALSE ) THEN
			IO.Put( "Invalid default entry value!\n" ) ;
		END ;
	END ;
	
	(* Confirm default chunk mask *)
	FOR ChunkMaskIndex := FIRST( my_tcam.ChunkMask^ ) TO LAST( my_tcam.ChunkMask^ ) DO
		IF my_tcam.ChunkMask[ ChunkMaskIndex ] = FALSE THEN
			IO.Put( "Invalid default chunk mask value!\n" ) ;
		END ;
	END ;
	
END Main.
