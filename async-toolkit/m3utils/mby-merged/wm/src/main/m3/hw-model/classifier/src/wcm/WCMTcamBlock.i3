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

(* Profile - provided by the mapper
Configures the TCAM slice
StartCompare - denotes whether slice is the beginning of a cascade
SelectTop through Select3 - mux selects *)
TYPE Profile = RECORD
	StartCompare : BOOLEAN := FALSE ;
	SelectTop : BITS 6 FOR [16_00..16_3F] := 16_00 ;
	Select0 : BITS 7 FOR [16_00..16_7F] := 16_00 ;
	Select1 : BITS 7 FOR [16_00..16_7F] := 16_00 ;
	Select2 : BITS 7 FOR [16_00..16_7F] := 16_00 ;
	Select3 : BITS 7 FOR [16_00..16_7F] := 16_00 ;
END ;

(* Keys - provided by the mapper
Bit strings that can be used to construct the TCAM search key
Key8 - list of 8-bit keys from which to select for construction of search key
Key16 - same idea, but for 16-bits
Key32 - same idea, but for 32-bits *)
TYPE Keys = RECORD
	Key8 : REF ARRAY OF BITS 8 FOR [16_00..16_FF] := NIL ;
	Key16 : REF ARRAY OF BITS 16 FOR [16_0000..16_FFFF] := NIL ;
	Key32 : REF ARRAY OF BITS 32 FOR [16_00000000..16_FFFFFFFF] := NIL ;
END ;

(* WCM TCAM Block
BlockTcamSlice - the TcamSlice for this particular block
BlockKeys - the set of bit strings from which we can construct the search key
BlockProfile - determines A) what keys we're selecting from BlockKeys to construct
	       the search key and B) whether this slice is the start of a cascade
*)
TYPE T = RECORD 
	BlockTcamSlice : Tcam.TcamSlice ;
	BlockKeys : REF Keys := NIL ;
	BlockProfile : REF Profile := NIL ;
	BlockInHits : REF ARRAY OF BOOLEAN := NIL ;
END ;

(****************)
(** Exceptions **)
(****************)

EXCEPTION NoHitsException( TEXT ) ;

(****************)
(** Procedures **)
(****************)

(* GetOutHits
TcamBlock - a reference to the TCAM block of interest
Returns a reference to an array of length NUM_ENTRIES.
The i-th element corresponds to whether there was a hit (TRUE)
or a miss (FALSE) at the i-th entry in the TCAM.
*)
PROCEDURE GetOutHits( TcamBlock : REF T ) : REF ARRAY OF BOOLEAN ;

(* GetHitIndexValid
TcamBlock - a reference to the TCAM block of interest
Return TRUE if the TCAM block had a hit in one of its entries
Return FALSE otherwise
*)
PROCEDURE GetHitIndexValid( TcamBlock : REF T ) : BOOLEAN ;

(* GetHitIndex
TcamBlock - a reference to the TCAM block of interest
Returns the index of the TCAM slice hit. If more than one
entry results in a hit, the highest numbered entry wins.
So, if entries 0 and 1 are hits, entry 1 is the winner
and GetHitIndex returns 1.
Raises NoHitsException if no hits occur.
*)
PROCEDURE GetHitIndex( TcamBlock : REF T ) : CARDINAL RAISES { NoHitsException } ;

END WCMTcamBlock.
