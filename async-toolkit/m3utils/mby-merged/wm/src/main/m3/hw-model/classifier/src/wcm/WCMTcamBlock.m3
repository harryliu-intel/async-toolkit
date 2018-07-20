INTERFACE WCMTcamBlock ;

IMPORT Tcam ;

TYPE T = OBJECT
	BlockTcamSlice : Tcam.TcamSlice ;
	BlockKeys : Keys ;
	BlockProfile : Profile ;
	BlockInHits : REF ARRAY OF BOOLEAN := NIL ;
METHODS
	GetKeys( ) := MuxGetKeys ;
	GetProfile( ) := MuxGetProfile ;
	GetInHits( ) := MuxGetInHits ;
	GetOutHits( ) := MuxGetOutHits ;
	GetHitIndexValid( ) := MuxGetHitIndexValid ;
	GetHitIndex( ) := MuxGetHitIndex ;
	SetKeys( NewKeys : Keys ) := MuxSetKeys ;
	SetProfile( NewProfile : Profile ) := MuxSetProfile ;
	SetInHits( NewHits : REF ARRAY OF BOOLEAN ) := MuxSetInHits ;
END ;

(** Accessor Methods **)

PROCEDURE MuxGetKeys( self : T ) : Keys =
BEGIN
	RETURN self.BlockKeys ;
END MuxGetKeys ;

PROCEDURE MuxGetProfile( self : T ) : Profile =
BEGIN
	RETURN self.BlockProfile ;
END MuxGetProfile ;

PROCEDURE MuxGetInHits( self : T ) : REF ARRAY OF BOOLEAN =
BEGIN
	RETURN self.BlockInHits ;
END MuxGetInHits ;

PROCEDURE MuxGetOutHits( self : T ) : REF ARRAY OF BOOLEAN =
BEGIN
	RETURN NIL ;
END MuxGetOutHits ;

PROCEDURE MuxGetHitIndexValid( self : T ) : BOOLEAN =
BEGIN
	RETURN NIL ;
END MuxGetHitIndexValid ;

PROCEDURE MuxGetHitIndex( self : T ) : REF ARRAY OF BOOLEAN =
BEGIN
	RETURN NIL ;
END MuxGetHitIndex ;

(** Mutator Methods **)

PROCEDURE MuxSetKeys( self : T ; NewKeys : Keys ) RAISES { InvalidKeysError } =
BEGIN
	IF Keys.Key8 = NIL THEN
		RAISE InvalidKeysError( "Key8 is NIL!" ) ;
	END ;
	IF Keys.Key16 = NIL THEN
		RAISE InvalidKeysError( "Key16 is NIL!" ) ;
	END ;
	IF Keys.Key32 = NIL THEN
		RAISE InvalidKeysError( "Key32 is NIL!" ) ;
	END ;
	IF NUMBER( Keys.Key8^ ) # KEY8_LENGTH THEN
		RAISE InvalidKeysError( "Key8 length is not correct!" ) ;
	END ;
	IF NUMBER( Keys.Key16^ ) # KEY16_LENGTH THEN
		RAISE InvalidKeysError( "Key16 length is not correct!" ) ;
	END ;
	IF NUMBER( Keys.Key32^ ) # KEY32_LENGTH THEN
		RAISE InvalidKeysError( "Key32 length is not correct!" ) ;
	END ;
	self.BlockKeys := NewKeys ;
END MuxSetKeys ;

PROCEDURE MuxSetProfile( self : T ; NewProfile : Profile ) =
BEGIN
	self.BlockProfile := NewProfile ;
END MuxSetProfile ;

PROCEDURE MuxSetInHits( self : T ; NewHits : REF ARRAY OF BOOLEAN ) RAISES { InvalidInHitsError } =
BEGIN
	IF NewHits = NIL THEN
		RAISE InvalidInHitsError( "NewHits is NIL!" ) ;
	END ;
	IF NUMBER( NewHits^ ) # NUM_ENTRIES THEN
		RAISE InvalidInHitsError( "NewHits does not have correct number of elements!" ) ;
	END ;
	self.BlockInHits := NewHits ;
END MuxSetInHits ;

(* GetMuxArrays *)
(* Goes from least significant MUX to most significant MUX. *)
PROCEDURE GetMuxArrays( self : T ) : REF ARRAY OF REF ARRAY OF BITS 8 FOR [16_00..16_FF] =
	RETURN NIL ;
BEGIN
	
END GetMuxArrays ;

END WCMTcamBlock.
