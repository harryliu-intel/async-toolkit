(* Roman Parise *)
(* High-level model of a TCAM *)

INTERFACE Tcam ;

(*************)
(* Constants *)
(*************)

(* Number of entries per TCAM slice *)
CONST NUM_ENTRIES = 1024 ;

(*********)
(* Types *)
(*********)

(* Type of TCAM entries and search strings - 40 bits *)
TYPE KeyString = [0..16_ffffffffff] ;

(* One 40-bit TCAM entry *)
TYPE Entry = RECORD
	Key : KeyString := 16_0000000000 ;
	KeyInvert : KeyString := 16_0000000000 ;
	Valid : BOOLEAN := FALSE
END ;

(* One TCAM slice with 40-bit entries *)
TYPE TcamSlice = RECORD
	NumEntries : INTEGER := 0 ;
	Log2EntriesPerChunk : [1..LAST(INTEGER)] := 1 ;
	ChunkMask : REF ARRAY OF BOOLEAN := NIL ;
	Entries : REF ARRAY OF Entry := NIL ;
END ;

(**************)
(* Exceptions *)
(**************)

EXCEPTION InvalidTCAMEntryConfiguration ;
EXCEPTION InvalidTCAMChunkConfiguration ;

(**************)
(* Procedures *)
(**************)

(* Create a TCAM slice
	
	- NumEntries : number of entries in TCAM slice (mandatory)
	- Log2EntriesPerChunk : number of entries per chunk; optional and
			        only useful if you plan on using a chunk mask ;
				initialized to 1 if not provided
	- ChunkMask : reference to an array of BOOLEANs ; 'True' turns on the chunk,
		      'False' turns off the chunk. Must have same length as
		      NumEntries / ( 2 ^ ( Log2EntriesPerChunk ) ) ; optional and
		      initialized to all 'True' if not provided
	- Entries : reference to an array of Entry records ; allows the user
		    initialize the key, keyinvent, and valid bits in the TCAM entries ;
		    optional and initialized to the default value of Entry if not provided
	Returns TcamSlice record with the values described above.
	- Raises InvalidTCAMEntryConfiguration when the length of Entries is not equal
	to NumEntries
	- Raises InvalidTCAMChunkConfiguration when 2^(Log2EntriesPerChunk) exceeds
	the number of entries, 2^(Log2EntriesPerChunk) doesn't evenly divide NumEntries,
	or if the ChunkMask length is not equal to NumEntries / ( 2^(Log2EntriesPerChunk) )

*)
PROCEDURE MakeSlice( NumEntries : [1..LAST(INTEGER)] ;
                     Log2EntriesPerChunk : [1..LAST(INTEGER)] := 1 ;
                     ChunkMask : REF ARRAY OF BOOLEAN := NIL ;
                     Entries : REF ARRAY OF Entry := NIL ) :
		     TcamSlice
		     RAISES { InvalidTCAMEntryConfiguration , InvalidTCAMChunkConfiguration } ;
	
(* LookupInTcamSlice

   - returns array of NUM_ENTRIES bools; '1' if that entry is a match, '0' o.w.
   - chunk_mask :: 16-bit string; TCAM is divided into 16 chunks of 64 entries
	           each; a '1' enables the chunk, a '0' disables the chunk
   - search :: the 32-bit string you are using to search in the TCAM
   - my_tcam :: array of NUM_ENTRIES entries

HLP TCAM Match Convention

Key | KeyInvert |    Matches
------------------------------
 0  |     0     | Either 0 or 1 (X)
 0  |     1     |       0
 1  |     0     |       1
 1  |     1     | Neither 0 nor 1

*)

PROCEDURE LookupInTcamSlice( search : KeyString ;
			     slice : TcamSlice ) :
		  	     REF ARRAY OF BOOLEAN ;

END Tcam.
