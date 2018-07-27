(* Roman Parise - WCM Group *)
INTERFACE WCM ;

(*************)
(** Imports **)
(*************)
IMPORT WCMTcamBlock ;
IMPORT WCMConstants ;

(***************)
(** Constants **)
(***************)
CONST NUM_ENTRIES = WCMConstants.NUM_ENTRIES ;
CONST NUM_ACTION_RAMS = 10 ;
CONST NUM_ACTIONS = 2 ;
CONST NUM_SLICES = 10 ;

(***********)
(** Types **)
(***********)

(* Action Type *)
TYPE Action = [16_00000000..16_ffffffff] ;

(* Action RAM Entry *)
TYPE ActionRAMEntry = ARRAY[0..NUM_ACTIONS-1] OF Action ;

(* Action RAM Cfg Register
- Enable :: determines whether the action RAM is powered on
	    and can generate actions
- SliceIndex :: determines which TCAM slice addresses the
		action RAM; whenever entry i in the slice
		is hit, the entry i in the action RAM is
		the generated action
*)
TYPE ActionRAMCfg = RECORD
	Enable : ARRAY[0..NUM_ACTION_RAMS-1] OF BOOLEAN ;
	SliceIndex : ARRAY[0..NUM_ACTION_RAMS-1] OF [0..NUM_SLICES-1] ;
END ;

(* WCMGroupProfile
- StartCompare :: array of references to booleans; each one is
		  the startcompare bit for the corresponding TCAM slice
- MuxSelects :: the mux select bits for the WCM group
*)
TYPE WCMGroupProfile = RECORD
	StartCompare : REF ARRAY OF REF BOOLEAN := NIL ;
	MuxSelects : REF WCMTcamBlock.KeyMuxSelect := NIL ;
END ;

(* WCMGroup
- TcamBlock :: array of TCAM blocks in the WCM group
- ActionRAM :: the action RAM banks
- ARAMCfg :: the configuration of the action RAMs
- MyKeys :: the keys to the TCAM blocks from the mapper
	    ( same for all TCAM slices )
- InHits :: the hits from the previous group
- GroupProfile :: the profile that defines parameters
		  such as the select bits to the key muxes
		  and the StartCompare string
*)
TYPE T = RECORD
	TcamBlock : REF ARRAY OF REF WCMTcamBlock.T ;
	ActionRAM : REF ARRAY OF REF ARRAY OF ActionRAMEntry ;
	ARAMCfg : ActionRAMCfg ;
	MyKeys : REF WCMTcamBlock.Keys := NIL ;
	InHits : REF ARRAY OF BOOLEAN := NIL ;
	GroupProfile : REF WCMGroupProfile := NIL ;
END ;

(****************)
(** Exceptions **)
(****************)

EXCEPTION NoActionException( TEXT ) ;

(****************)
(** Procedures **)
(****************)

(* GetWinningAction
- WCMGroup :: reference to the WCM group of interest
Returns the winning ActionRAMEntry. A TCAM lookup is
performed for different slices. The action RAMs check
for any hits in their respective TCAM slices. The
action RAMs then generate their hit indices. The highest-
numbered action RAM has the winning entry.
If no actions are generated in total, the NoActionException
is thrown.
*)
PROCEDURE GetWinningAction( WCMGroup : REF T ) : ActionRAMEntry RAISES { NoActionException } ;

(* GetWCMGroupOutHits
- WCMGroup :: reference to the WCM group of interest
Returns the hits from the last TCAM slice. These can then
be sent to the next group.
*)
PROCEDURE GetWCMGroupOutHits( WCMGroup : REF T ) : REF ARRAY OF BOOLEAN ;

(* MakeWCMGroup
Returns a WCM group with the specified fields.
MyKeys, InHits, and GroupProfile are not changed by the procedure.
They are assigned by value and not as references.
TcamBlock will most likely be changed since the keys and profiles
are going to be overwritten by the WCM group parameters. ActionRAM
is assigned as a reference and not by value in the current
implementation.
Do NOT create TcamBlocks without first calling MakeWCMGroup.
*)
PROCEDURE MakeWCMGroup( TcamBlock : REF ARRAY OF REF WCMTcamBlock.T ;
			ActionRAM : REF ARRAY OF REF ARRAY OF ActionRAMEntry ;
			ARAMCfg : ActionRAMCfg ;
			MyKeys : REF WCMTcamBlock.Keys ;
			InHits : REF ARRAY OF BOOLEAN ;
			GroupProfile : REF WCMGroupProfile ) : REF T ;

(* SetInHits
- WCMGroup :: reference to the WCM group of interest
- InHits :: inhits to the 0th TCAM slice in the WCM group
InHits is assigned by value and not by reference.
*)
PROCEDURE SetInHits( WCMGroup : REF T ; InHits : REF ARRAY OF BOOLEAN ) ;

(* SetKeys
- WCMGroup :: reference to the WCM group of interest
- NewKeys :: the keys to the TCAM slices
NewKeys is assigned by value and not by reference.
*)
PROCEDURE SetKeys( WCMGroup : REF T ; NewKeys : REF WCMTcamBlock.Keys ) ;

(* SetKeys
- WCMGroup :: reference to the WCM group of interest
- GroupProfile :: the profile to the TCAM slices
GroupProfile is assigned by value and not by reference.
*)
PROCEDURE SetProfile( WCMGroup : REF T ; GroupProfile : REF WCMGroupProfile ) ;

END WCM.

(***********)
(** TODOs **)
(***********)
(* READONLY types for const arguments? *)
(* More robust computation of outhits from one stage
being inhits to next stage. *)
(* Please ensure encapsulation of data structures, such
as T. Not quite sure what the best way to do this in M3 is. *)
