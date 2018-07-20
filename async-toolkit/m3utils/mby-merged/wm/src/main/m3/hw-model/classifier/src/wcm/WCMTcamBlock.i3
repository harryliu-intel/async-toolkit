INTERFACE WCMTcamBlock ;

(***********)
(** Types **)
(***********)

(* Profile
Configures the TCAM slice *)
TYPE Profile = RECORD
	ChunkMask : BOOLEAN := FALSE ;
	StartCompare : BITS 1 FOR [0..1] := 0 ;
	SelectTop : BITS 6 FOR [16_00..16_3F] := 16_00 ;
	Select0 : BITS 7 FOR [16_00..16_7F] := 16_00 ;
	Select1 : BITS 7 FOR [16_00..16_7F] := 16_00 ;
	Select2 : BITS 7 FOR [16_00..16_7F] := 16_00 ;
	Select3 : BITS 7 FOR [16_00..16_7F] := 16_00 ;
END ;

(* Keys
Bit strings that can be used to construct the TCAM search key *)
TYPE Keys = RECORD
	Key8 : REF ARRAY OF BITS 8 FOR [16_00..16_FF] := NIL ;
	Key16 : REF ARRAY OF BITS 16 FOR [16_0000..16_FFFF] := NIL ;
	Key32 : REF ARRAY OF BITS 32 FOR [16_00000000..16_FFFFFFFF] := NIL ;
END ;

(***************)
(** Constants **)
(***************)

CONST KEY8_LENGTH = 64 ;
CONST KEY16_LENGTH = 32 ;
CONST KEY32_LENGTH = 16 ;
CONST NUM_ENTRIES = 1024 ;

(****************)
(** Exceptions **)
(****************)

EXCEPTION InvalidKeysError( TEXT ) ;
EXCEPTION InvalidInHitsError( TEXT ) ;

(*************)
(** Objects **)
(*************)

TYPE T = OBJECT
METHODS
	(** Accessor Methods **)
	GetKeys( ) : Keys ;
	GetProfile( ) : Profile ;
	GetInHits( ) : REF ARRAY OF BOOLEAN ;
	GetOutHits( ) : REF ARRAY OF BOOLEAN ;
	GetHitIndexValid( ) : BOOLEAN ;
	GetHitIndex( ) : REF ARRAY OF BOOLEAN ;
	(** Mutator Methods **)
	SetKeys( NewKeys : Keys )  RAISES { InvalidKeysError } ;
	SetProfile( NewProfile : Profile ) ;
	SetInHits( NewHits : REF ARRAY OF BOOLEAN ) RAISES { InvalidInHitsError } ;
END ;

END WCMTcamBlock.
