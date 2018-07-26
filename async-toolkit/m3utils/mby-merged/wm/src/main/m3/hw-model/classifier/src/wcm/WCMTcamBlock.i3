(* Roman Parise - WCMTcamBlock *)
INTERFACE WCMTcamBlock ;

(*************)
(** Imports **)
(*************)
IMPORT Tcam ;
IMPORT WCMConstants ;

(***************)
(** Constants **)
(***************)

CONST KEY8_LENGTH = WCMConstants.KEY8_LENGTH ;
CONST KEY16_LENGTH = WCMConstants.KEY16_LENGTH ;
CONST KEY32_LENGTH = WCMConstants.KEY32_LENGTH ;
CONST NUM_ENTRIES = WCMConstants.NUM_ENTRIES ;

(***********)
(** Types **)
(***********)

(* Profile
Configures the TCAM slice *)
TYPE Profile = RECORD
	StartCompare : BOOLEAN := FALSE ;
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

(* WCM TCAM Block *)
TYPE T = RECORD 
	BlockTcamSlice : Tcam.TcamSlice ;
	BlockKeys : Keys ;
	BlockProfile : Profile ;
	BlockInHits : REF ARRAY OF BOOLEAN := NIL ;
END ;

(****************)
(** Exceptions **)
(****************)

EXCEPTION NoHitsException( TEXT ) ;

(****************)
(** Procedures **)
(****************)

PROCEDURE GetOutHits( WCMGroup : REF T ) : REF ARRAY OF BOOLEAN ;
PROCEDURE GetHitIndexValid( WCMGroup : REF T ) : BOOLEAN ;
PROCEDURE GetHitIndex( WCMGroup : REF T ) : CARDINAL RAISES { NoHitsException } ;

END WCMTcamBlock.
