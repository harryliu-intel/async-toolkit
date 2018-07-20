INTERFACE WCMTcamBlock ;

(***********)
(** Types **)
(***********)

(* Profile
Configures the TCAM slice *)
TYPE Profile = RECORD
	ChunkMask : REF ARRAY OF BOOLEAN := NEW( REF ARRAY OF BOOLEAN , 16 , FALSE ) ;
	StartCompare : REF ARRAY OF BOOLEAN := NIL ;
	StartSet : REF ARRAY OF BOOLEAN := NIL ;
	SelectTop : BITS 6 FOR [16_00..16_3F] := 16_00 ;
	Select0 : BITS 7 FOR [16_00..16_7F] := 16_00 ;
	Select1 : BITS 7 FOR [16_00..16_7F] := 16_00 ;
	Select2 : BITS 7 FOR [16_00..16_7F] := 16_00 ;
	Select3 : BITS 7 FOR [16_00..16_7F] := 16_00 ;
END ;

(* Keys
Bit strings that can be used to construct the TCAM search key *)
TYPE Keys = RECORD
	Key8 : BITS 8 FOR [16_00..16_FF] := 16_00 ;
	Key16 : BITS 16 FOR [16_0000..16_FFFF] := 16_0000 ;
	Key32 : BITS 32 FOR [16_00000000..16_FFFFFFFF] := 16_00000000 ;
END ;

(*************)
(** Objects **)
(*************)

TYPE T = OBJECT
METHODS
	(** Accessor Methods **)
	GetKeys( ) : Keys ;
	GetProfile( ) : Profile ;
	GetInHits( ) : REF ARRAY OF BIT ;
	GetOutHits( ) : REF ARRAY OF BIT ;
	GetHitIndexValid( ) : BIT ;
	GetHitIndex( ) : REF ARRAY OF BIT ;
	(** Mutator Methods **)
	SetKeys( NewKeys : Keys ) ;
	SetProfile( NewProfile : Profile ) ;
	SetInHits( NewHits : BIT ) ;
END ;

END WCMTcamBlock.
