MODULE ParseQueue ;

IMPORT CARDINALList ;
IMPORT IO ;
IMPORT Fmt ;

PROCEDURE Push( q : REF T ; el : CARDINAL := 0 ) =
BEGIN
	<* ASSERT q # NIL *>
	q^ := CARDINALList.Cons( el , q^ ) ;
END Push ;

PROCEDURE Pop( q : REF T ) =
BEGIN
	<* ASSERT q # NIL *>
	q^ := q^.tail ;
END Pop ;

PROCEDURE Inc( q : REF T ; inc : CARDINAL := 1 ) =
BEGIN
	<* ASSERT q # NIL *>
	INC( q^.head , inc ) ;
END Inc ;

PROCEDURE Peek( q : REF T ) : CARDINAL =
BEGIN
	<* ASSERT q # NIL *>
	<* ASSERT CARDINALList.Length( q^ ) > 0 *>
	RETURN q^.head ;
END Peek ;

PROCEDURE Depth( q : REF T ) : CARDINAL =
BEGIN
	<* ASSERT q # NIL *>
	RETURN CARDINALList.Length( q^ ) ;
END Depth ;

PROCEDURE Sum( q : REF T ) : CARDINAL =
VAR
	temp_list := NEW( REF T ) ;
	sum := 0 ;
BEGIN
	<* ASSERT q # NIL *>
	temp_list^ := q^ ;
	WHILE CARDINALList.Length( temp_list^ ) # 0 DO
		INC( sum , Peek( temp_list ) ) ;
		temp_list^ := temp_list^.tail ;
	END ;
	RETURN sum ;
END Sum ;

PROCEDURE Debug( q : REF T ) =
VAR
	temp_list : T ;
BEGIN
	<* ASSERT q # NIL *>
	temp_list := q^ ;
	IO.Put( "Debug queue top-to-bottom: " ) ;
	WHILE temp_list # NIL DO
		IO.Put( Fmt.Int( temp_list.head ) & " " ) ;
		temp_list := temp_list.tail ;
	END ;
	IO.Put( "\n" ) ;
END Debug ;

BEGIN END ParseQueue .
