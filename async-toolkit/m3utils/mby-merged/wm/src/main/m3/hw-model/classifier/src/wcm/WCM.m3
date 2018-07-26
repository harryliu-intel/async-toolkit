MODULE WCM ;

(*************)
(** Imports **)
(*************)
IMPORT WCMTcamBlock ;

(************************)
(** Visible Procedures **)
(************************)

(* Go through each action RAM bank *)
(* The highest numbered action RAM bank with an action wins *)
PROCEDURE GetWinningAction( WCMGroup : REF T ) : ActionRAMEntry RAISES { NoActionException } =
VAR
	exc_msg : TEXT := "No Action RAM bank has a valid action." ;
	winning_entry : ActionRAMEntry ;
BEGIN
	<* ASSERT NUMBER( WCMGroup^.ActionRAM ) = NUM_ACTION_RAMS *>
	FOR bank_index := NUM_ACTION_RAMS - 1 TO 0 BY -1 DO
		TRY
			winning_entry := GetAction( WCMGroup , bank_index ) ;
			RETURN winning_entry ;
		EXCEPT
			| NoActionException( msg ) => EVAL msg ;
			| WCMTcamBlock.NoHitsException( msg ) => EVAL msg ;
		END ;
	END ;
	RAISE NoActionException( exc_msg ) ;
END GetWinningAction ;

(***********************)
(** Hidden Procedures **)
(***********************)

(* 
Two conditions must be met for an Action to be returned...
   - ActionRAM must be enabled
   - The ActionRAM's corresponding slice must have a valid hit index
If one of these conditions fails, NoActionException is raised. *)
PROCEDURE GetAction( WCMGroup : REF T ; ActionRAMIndex : [0..NUM_ACTION_RAMS-1] ) : ActionRAMEntry RAISES { NoActionException , WCMTcamBlock.NoHitsException } =
VAR
	exc_msg : TEXT := "The selected Action RAM bank does not have a valid action." ;
	slice_index := 0 ;
	aram_bank : REF ARRAY OF ActionRAMEntry := NIL ;
	tcam_block : REF WCMTcamBlock.T := NIL ;
BEGIN
	<* ASSERT NUMBER( WCMGroup.ARAMCfg.Enable ) = NUM_ACTION_RAMS *>
	(* Action RAM bank is disabled. *)
	IF WCMGroup.ARAMCfg.Enable[ FIRST( WCMGroup.ARAMCfg.Enable ) + ActionRAMIndex ] = FALSE THEN
		RAISE NoActionException( exc_msg ) ;
	END ;
	<* ASSERT NUMBER( WCMGroup.ARAMCfg.SliceIndex ) = NUM_ACTION_RAMS *>
	slice_index := WCMGroup.ARAMCfg.SliceIndex[ ActionRAMIndex ] ;
	<* ASSERT NUMBER( WCMGroup.TcamBlock ) = NUM_SLICES *>
	tcam_block := WCMGroup.TcamBlock[ FIRST( WCMGroup.TcamBlock ) + slice_index ] ;
	<* ASSERT tcam_block # NIL *>
	(* Corresponding TCAM slice has no hits. *)
	IF WCMTcamBlock.GetHitIndexValid( tcam_block ) = FALSE THEN
		RAISE NoActionException( exc_msg ) ;
	END ;
	<* ASSERT NUMBER( WCMGroup.ActionRAM ) = NUM_ACTION_RAMS *>
	aram_bank := WCMGroup.ActionRAM[ FIRST( WCMGroup.ActionRAM ) + ActionRAMIndex ] ;
	<* ASSERT aram_bank # NIL *>
	<* ASSERT NUMBER( aram_bank^ ) = NUM_ENTRIES *>
	(* To silence warnings. We already confirmed that the hit index is valid,
	so NoHitsException is not going to occur. *)
	TRY
		RETURN aram_bank[ FIRST( aram_bank^ ) + WCMTcamBlock.GetHitIndex( tcam_block ) ] ;
	EXCEPT
		| WCMTcamBlock.NoHitsException( msg ) => RAISE WCMTcamBlock.NoHitsException( msg ) ;
	END ;
END GetAction ;

BEGIN END WCM.
