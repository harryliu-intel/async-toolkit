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

(* Action RAM Cfg Register *)
TYPE ActionRAMCfg = RECORD
	Enable : ARRAY[0..NUM_ACTION_RAMS-1] OF BOOLEAN ;
	SliceIndex : ARRAY[0..NUM_ACTION_RAMS-1] OF [0..NUM_SLICES-1] ;
END ;

(* WCMGroup *)
TYPE T = RECORD
	TcamBlock : REF ARRAY OF REF WCMTcamBlock.T ;
	ActionRAM : REF ARRAY OF REF ARRAY OF ActionRAMEntry ;
	ARAMCfg : ActionRAMCfg ;
	MyKeys : REF WCMTcamBlock.Keys := NIL ;
	InHits : REF ARRAY OF BOOLEAN := NIL ;
	GroupProfile : REF WCMTcamBlock.Profile := NIL ;
END ;

(****************)
(** Exceptions **)
(****************)

EXCEPTION NoActionException( TEXT ) ;

(****************)
(** Procedures **)
(****************)

PROCEDURE GetWinningAction( WCMGroup : REF T ) : ActionRAMEntry RAISES { NoActionException } ;

PROCEDURE GetWCMGroupOutHits( WCMGroup : REF T ) : REF ARRAY OF BOOLEAN ;

PROCEDURE MakeWCMGroup( TcamBlock : REF ARRAY OF REF WCMTcamBlock.T ;
			ActionRAM : REF ARRAY OF REF ARRAY OF ActionRAMEntry ;
			ARAMCfg : ActionRAMCfg ;
			MyKeys : REF WCMTcamBlock.Keys ;
			InHits : REF ARRAY OF BOOLEAN ;
			GroupProfile : REF WCMTcamBlock.Profile ) : REF T ;

PROCEDURE SetInHits( WCMGroup : REF T ; InHits : REF ARRAY OF BOOLEAN ) ;

PROCEDURE SetKeys( WCMGroup : REF T ; NewKeys : REF WCMTcamBlock.Keys ) ;

PROCEDURE SetProfile( WCMGroup : REF T ; GroupProfile : REF WCMTcamBlock.Profile ) ;

END WCM.
