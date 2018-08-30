MODULE ParseQueue ;

IMPORT CARDINALList ;

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

PROCEDURE Inc( q : REF T ; inc : CARDINAL := 0 ) =
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

BEGIN END ParseQueue .
