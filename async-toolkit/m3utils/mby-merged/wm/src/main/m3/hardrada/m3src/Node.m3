MODULE Node ;

(**********************)
(* Visible Procedures *)
(**********************)

PROCEDURE Equal( NodeA , NodeB : T ) : BOOLEAN =
BEGIN
	IF NodeA.val # NodeB.val THEN
		RETURN FALSE ;
	END ;
	IF NodeA.cat # NodeB.cat THEN
		RETURN FALSE ;
	END ;
	IF NodeA.children # NIL AND NodeB.children # NIL THEN
		IF NUMBER( NodeA.children^ ) # NUMBER( NodeB.children^ ) THEN
			RETURN FALSE ;
		END ;
		FOR child_index := FIRST( NodeA.children^ ) TO LAST( NodeA.children^ ) DO
			IF NodeA.children[ child_index ] # NIL AND NodeB.children[ child_index ] # NIL THEN
				IF NOT Equal( NodeA.children[ child_index ]^ , NodeB.children[ child_index ]^ ) THEN
					RETURN FALSE ;
				END ;
			ELSIF NodeA.children[ child_index ] # NodeB.children[ child_index ] THEN
				RETURN FALSE ;
			END ;
		END ;
		RETURN TRUE ;
	ELSIF NodeA.children # NodeB.children THEN
		RETURN FALSE ;
	ELSE
		RETURN TRUE ;
	END ;
END Equal ;

PROCEDURE DeepCopy( CurrentNode : REF T ) : REF T =
VAR
	NewNode := NEW( REF T ) ;
BEGIN
	NewNode^.val := CurrentNode^.val ;
	NewNode^.cat := CurrentNode^.cat ;
	IF CurrentNode^.children # NIL THEN
		NewNode^.children := NEW( REF ARRAY OF REF T , NUMBER( NewNode^.children^ ) ) ;
		FOR child_index := FIRST( NewNode^.children^ ) TO LAST( NewNode^.children^ ) DO
			NewNode^.children[ child_index ] := DeepCopy( CurrentNode^.children[ child_index ] ) ;
		END ;
	ELSE
		NewNode^.children := NIL ;
	END ;
	RETURN NewNode ;
END DeepCopy ;

BEGIN END Node .
