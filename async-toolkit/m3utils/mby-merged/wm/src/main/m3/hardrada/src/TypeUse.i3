INTERFACE TypeUse ;

CONST Brand = "TypeUse" ;

TYPE T = RECORD
	TypeName : TEXT := "" ;
	(* Blank implies no pointer points to this *)
	Ptr : REF ARRAY OF TEXT := NIL ; 
	AlreadyAssigned : BOOLEAN := FALSE ;
END ;

END TypeUse .
