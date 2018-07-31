(* Roman Parise - WCM Group *)
MODULE WCM ;

(*************)
(** Imports **)
(*************)
IMPORT WCMTcamBlock ;

(************************)
(** Visible Procedures **)
(************************)

PROCEDURE GetWinningAction( WCMGroup : REF T ) : ActionRAMEntry RAISES { NoActionException } =
VAR
	exc_msg : TEXT := "No Action RAM bank has a valid action." ;
	winning_entry : ActionRAMEntry ;
BEGIN
	<* ASSERT WCMGroup # NIL *>
	<* ASSERT NUMBER( WCMGroup^.ActionRAM^ ) = NUM_ACTION_RAMS *>
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

PROCEDURE GetWCMGroupOutHits( WCMGroup : REF T ) : REF ARRAY OF BOOLEAN =
BEGIN
	<* ASSERT WCMGroup # NIL *>
	<* ASSERT WCMGroup.TcamBlock # NIL *>
	(* TODO: Probably not necessary to actually compute the hits after
	calling UpdateAllTCAMBlocks *)
	RETURN WCMTcamBlock.GetOutHits( WCMGroup.TcamBlock[ LAST( WCMGroup.TcamBlock^ ) ] ) ;
END GetWCMGroupOutHits ;

PROCEDURE MakeWCMGroup( TcamBlock : REF ARRAY OF REF WCMTcamBlock.T ;
			ActionRAM : REF ARRAY OF REF ARRAY OF ActionRAMEntry ;
			ARAMCfg : ActionRAMCfg ;
			MyKeys : REF WCMTcamBlock.Keys ;
			InHits : REF ARRAY OF BOOLEAN ;
			GroupProfile : REF WCMTcamBlock.Profile ) : REF T =
VAR
	wcm_group := NEW( REF T ) ;
BEGIN
	<* ASSERT TcamBlock # NIL *>
	<* ASSERT NUMBER( TcamBlock^ ) = NUM_SLICES *>
	<* ASSERT ( InHits = NIL AND NUMBER( TcamBlock^ ) = 0 ) OR ( InHits # NIL AND NUMBER( TcamBlock^ ) > 0 ) *>
	<* ASSERT ActionRAM # NIL *>
	<* ASSERT NUMBER( ActionRAM^ ) = NUM_ACTION_RAMS *>
	wcm_group.TcamBlock := TcamBlock ;
	wcm_group.ActionRAM := ActionRAM ;
	wcm_group.ARAMCfg := ARAMCfg ;
	wcm_group.MyKeys := NEW( REF WCMTcamBlock.Keys ) ;
	(* Ensure each TCAM block has the correct keys *)
	FOR tcam_block_index := FIRST( wcm_group.TcamBlock^ ) TO LAST( wcm_group.TcamBlock^ ) DO
		<* ASSERT wcm_group.TcamBlock[ tcam_block_index ] # NIL *>
		wcm_group.TcamBlock[ tcam_block_index ].BlockKeys := MyKeys ;
	END ;
	SetKeys( wcm_group , MyKeys ) ;
	wcm_group.GroupProfile := NEW( REF WCMTcamBlock.Profile ) ;
	(* Ensure each TCAM block has the proper profile *)
	FOR tcam_block_index := FIRST( wcm_group.TcamBlock^ ) TO LAST( wcm_group.TcamBlock^ ) DO
		<* ASSERT wcm_group.TcamBlock[ tcam_block_index ] # NIL *>
		wcm_group.TcamBlock[ tcam_block_index ].BlockProfile := GroupProfile ;
	END ;
	SetProfile( wcm_group , GroupProfile ) ;
	(* Set the proper inhits array to the 0th slice *)
	wcm_group.InHits := NEW( REF ARRAY OF BOOLEAN , NUM_ENTRIES ) ;
	<* ASSERT NUMBER( wcm_group.InHits^ ) = NUMBER( InHits^ ) OR InHits = NIL *>
	SetInHits( wcm_group , InHits ) ;
	RETURN wcm_group ;
END MakeWCMGroup ;

PROCEDURE SetInHits( WCMGroup : REF T ; InHits : REF ARRAY OF BOOLEAN ) =
BEGIN
	<* ASSERT WCMGroup # NIL *>
	<* ASSERT ( InHits = NIL AND NUMBER( WCMGroup.TcamBlock^ ) = 0 ) OR ( InHits # NIL AND NUMBER( WCMGroup.TcamBlock^ ) > 0 ) *>
	FOR inhits_index := FIRST( WCMGroup.TcamBlock^ ) TO LAST( WCMGroup.TcamBlock^ ) DO
		WCMGroup.InHits[ inhits_index ] := InHits[ inhits_index ] ;
	END ;
	UpdateAllTCAMBlocks( WCMGroup ) ;
END SetInHits ;

PROCEDURE SetKeys( WCMGroup : REF T ; NewKeys : REF WCMTcamBlock.Keys ) =
BEGIN
	<* ASSERT WCMGroup # NIL *>
	<* ASSERT NewKeys # NIL *>
	WCMGroup.MyKeys^ := NewKeys^ ;
END SetKeys ;

PROCEDURE SetProfile( WCMGroup : REF T ; GroupProfile : REF WCMTcamBlock.Profile ) =
BEGIN
	<* ASSERT WCMGroup # NIL *>
	<* ASSERT GroupProfile # NIL *>
	WCMGroup.GroupProfile^ := GroupProfile^ ;
END SetProfile ;

(***********************)
(** Hidden Procedures **)
(***********************)

(* GetAction
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
	<* ASSERT WCMGroup # NIL *>
	<* ASSERT NUMBER( WCMGroup.ARAMCfg.Enable ) = NUM_ACTION_RAMS *>
	(* Action RAM bank is disabled. *)
	IF WCMGroup.ARAMCfg.Enable[ FIRST( WCMGroup.ARAMCfg.Enable ) + ActionRAMIndex ] = FALSE THEN
		RAISE NoActionException( exc_msg ) ;
	END ;
	<* ASSERT NUMBER( WCMGroup.ARAMCfg.SliceIndex ) = NUM_ACTION_RAMS *>
	slice_index := WCMGroup.ARAMCfg.SliceIndex[ ActionRAMIndex ] ;
	<* ASSERT NUMBER( WCMGroup.TcamBlock^ ) = NUM_SLICES *>
	tcam_block := WCMGroup.TcamBlock[ FIRST( WCMGroup.TcamBlock^ ) + slice_index ] ;
	<* ASSERT tcam_block # NIL *>
	UpdateAllTCAMBlocks( WCMGroup ) ;
	(* Corresponding TCAM slice has no hits. *)
	IF WCMTcamBlock.GetHitIndexValid( tcam_block ) = FALSE THEN
		RAISE NoActionException( exc_msg ) ;
	END ;
	<* ASSERT NUMBER( WCMGroup.ActionRAM^ ) = NUM_ACTION_RAMS *>
	aram_bank := WCMGroup.ActionRAM[ FIRST( WCMGroup.ActionRAM^ ) + ActionRAMIndex ] ;
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

(* This iterates through all the TCAM blocks and
performs a search. It then propagates all the outhits
to the later TCAM slices accordingly.
This needs to be called when the inhits array
is changed since the outhits of each TCAM slice
will not have updated. This does NOT need to be called
when changing Profile, Keys, etc. since the TCAM slices already
point to those structures. *)
PROCEDURE UpdateAllTCAMBlocks( WCMGroup : REF T ) =
VAR
	prev_out_hits : REF ARRAY OF BOOLEAN := NIL ;
BEGIN
	<* ASSERT WCMGroup # NIL *>
	(* Iterate through all TCAM slices *)
	<* ASSERT WCMGroup.TcamBlock # NIL *>
	FOR slice_index := FIRST( WCMGroup.TcamBlock^ ) TO LAST( WCMGroup.TcamBlock^ ) DO
		<* ASSERT WCMGroup.TcamBlock[ slice_index ] # NIL *>
		<* ASSERT WCMGroup.TcamBlock[ slice_index ].BlockInHits # NIL *>
		IF slice_index = FIRST( WCMGroup.TcamBlock^ ) THEN
			(* The first one can just be evaluated, but we make sure
			inhits is the same as the group's, just in case. *)
			<* ASSERT WCMGroup.InHits # NIL *>
			<* ASSERT NUMBER( WCMGroup.TcamBlock[ slice_index ].BlockInHits^ ) = NUMBER( WCMGroup.InHits^ ) *>
			WCMGroup.TcamBlock[ slice_index ].BlockInHits := WCMGroup.InHits ;
		ELSE
			(* Later slices change their inhits *)
			<* ASSERT prev_out_hits # NIL *>
			<* ASSERT NUMBER( WCMGroup.TcamBlock[ slice_index ].BlockInHits^ ) = NUMBER( prev_out_hits^ ) *>
			WCMGroup.TcamBlock[ slice_index ].BlockInHits := prev_out_hits ;
		END ;
		(* Always keep track of the most recent outhits *)
		prev_out_hits := WCMTcamBlock.GetOutHits( WCMGroup.TcamBlock[ slice_index ] ) ;
	END ;
END UpdateAllTCAMBlocks ;

BEGIN END WCM.
