INTERFACE ParseQueue ;

IMPORT CARDINALList ;

TYPE T = CARDINALList.T ;

PROCEDURE Push( q : REF T ; el : CARDINAL := 0 ) ;

PROCEDURE Pop( q : REF T ) ;

PROCEDURE Inc( q : REF T ; inc : CARDINAL := 1 ) ;

PROCEDURE Peek( q : REF T ) : CARDINAL ;

PROCEDURE Depth( q : REF T ) : CARDINAL ;

END ParseQueue .
