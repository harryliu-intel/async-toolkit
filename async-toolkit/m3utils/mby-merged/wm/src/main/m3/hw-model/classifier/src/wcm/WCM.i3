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
TYPE ActionRAMEntry = RECORD
	Actions : ARRAY[0..NUM_ACTIONS-1] OF Action ;
END ;

(* Action RAM Cfg Register *)
TYPE ActionRAMCfg = RECORD
	Enable : ARRAY[0..NUM_ACTION_RAMS-1] OF BOOLEAN ;
	SliceIndex : ARRAY[0..NUM_ACTION_RAMS-1] OF [0..NUM_SLICES-1] ;
END ;

(* WCMGroup *)
TYPE T = RECORD
	TcamBlock : ARRAY[0..NUM_SLICES-1] OF REF WCMTcamBlock.T ;
	ActionRAM : ARRAY[0..NUM_ACTION_RAMS-1] OF REF ARRAY OF ActionRAMEntry ;
	ARAMCfg : ActionRAMCfg ;
END ;

(****************)
(** Exceptions **)
(****************)

EXCEPTION NoActionException( TEXT ) ;

(****************)
(** Procedures **)
(****************)

PROCEDURE GetWinningAction( WCMGroup : REF T ) : ActionRAMEntry RAISES { NoActionException } ;

END WCM.
