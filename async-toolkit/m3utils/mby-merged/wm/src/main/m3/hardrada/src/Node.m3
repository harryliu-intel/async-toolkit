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

PROCEDURE FindAllNonterms( root : REF T ; NontermVal : TEXT ) : REF ARRAY OF REF T =
VAR
	return_arr : REF ARRAY OF REF T := NIL ;
BEGIN
	<* ASSERT root # NIL *>
	return_arr := NEW( REF ARRAY OF REF T , 0 ) ;
	IF root^.cat = Category.NonTerminal THEN
		IF root^.val = NontermVal THEN
			AppendArr( return_arr , root ) ;
		END ;
		FOR child_index := FIRST( root^.children^ ) TO LAST( root^.children^ ) DO
			AppendArrToArr( return_arr , FindAllNonterms( root^.children[ child_index ] , NontermVal ) ) ;
		END ;
	END ;
	RETURN return_arr ;
END FindAllNonterms ;

(* TODO Can you use readonly for these? *)
PROCEDURE AppendArr( ArrToChange : REF ARRAY OF REF T ; NodeToAppend : REF T ) =
VAR
	single_element_arr : REF ARRAY OF REF T := NIL ;
BEGIN
	<* ASSERT ArrToChange # NIL *>
	<* ASSERT NodeToAppend # NIL *>
	single_element_arr := NEW( REF ARRAY OF REF T , 1 ) ;
	single_element_arr[ FIRST( single_element_arr^ ) ] := NodeToAppend ;
	AppendArrToArr( ArrToChange , single_element_arr ) ;
END AppendArr ;

PROCEDURE AppendArrToArr( ArrToChange : REF ARRAY OF REF T ; ArrToAppend : REF ARRAY OF REF T ) =
VAR
	ArrToReturn : REF ARRAY OF REF T := NIL ;
	temp_last := 0 ;
BEGIN
	<* ASSERT ArrToChange # NIL *>
	<* ASSERT ArrToAppend # NIL *>
	<* ASSERT FIRST( ArrToChange^ ) = FIRST( ArrToReturn^ ) *>
	ArrToReturn := NEW( REF ARRAY OF REF T , NUMBER( ArrToChange^ ) + NUMBER( ArrToAppend^ ) ) ;
	FOR arr_index := FIRST( ArrToChange^ ) TO LAST( ArrToChange^ ) DO
		ArrToReturn[ arr_index ] := ArrToChange[ arr_index ] ;
	END ;
	temp_last := LAST( ArrToReturn^ ) ;
	FOR arr_index := FIRST( ArrToAppend^ ) TO LAST( ArrToAppend^ ) DO
		ArrToReturn[ ( arr_index - FIRST( ArrToAppend^ ) ) + temp_last + 1 ] := ArrToAppend[ arr_index ] ;
	END ;
	ArrToChange := ArrToReturn ;
END AppendArrToArr ;

BEGIN END Node .
