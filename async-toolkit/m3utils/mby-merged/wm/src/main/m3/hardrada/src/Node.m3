MODULE Node ;

(**********************)
(* Visible Procedures *)
(**********************)

(* Node *)

PROCEDURE Equal( NodeA , NodeB : T ) : BOOLEAN =
BEGIN
	IF NodeA.val # NodeB.val THEN
		RETURN FALSE ;
	END ;
	IF NodeA.cat # NodeB.cat THEN
		RETURN FALSE ;
	END ;
	IF NOT EqualDList( NodeA.children , NodeB.children ) THEN
		RETURN FALSE ;
	END ;
	RETURN TRUE ;
END Equal ;

PROCEDURE DeepCopy( NewNode : REF T ; CurrentNode : REF T ) =
BEGIN
	<* ASSERT CurrentNode # NIL *>
	NewNode := NEW( REF T ) ;
	NewNode^.val := CurrentNode^.val ;
	NewNode^.cat := CurrentNode^.cat ;
	NewNode^.children := NEW( REF DList ) ;
	DeepCopyDList( NewNode^.children , CurrentNode^.children ) ;
END DeepCopy ;

PROCEDURE FindAllNonterms( newlist : REF DList ; root : REF T ; NontermVal : TEXT ) =
VAR
	current_child := NEW( REF DList ) ;
	templist := NEW( REF DList ) ;
BEGIN
	<* ASSERT root # NIL *>
	<* ASSERT newlist # NIL *>
	DefaultDList( newlist ) ;
	IF root^.cat = Category.NonTerminal THEN
		IF root^.val = NontermVal THEN
			AppendNode( newlist , root ) ;
		END ;
		(* TODO Make this a function... somehow *)
		GoToBeginning( current_child , root^.children ) ;
		LOOP
			FindAllNonterms( templist , current_child^.cur , NontermVal ) ;
			AppendDList( newlist , templist ) ;
			IF current_child^.next = NIL THEN
				EXIT ;
			ELSE
				current_child := current_child^.next ;
			END ;
		END ;
	END ;
END FindAllNonterms ;

(* DList *)

PROCEDURE IsEmpty( list : DList ) : BOOLEAN =
BEGIN
	RETURN ( list.cur = NIL AND list.prev = NIL AND list.next = NIL ) ;
END IsEmpty ;

PROCEDURE Length( list : DList ) : CARDINAL =
VAR
	count := 0 ;
BEGIN
	IF IsEmpty( list ) THEN
		RETURN 0 ;
	END ;
	WHILE list.prev # NIL DO
		list := list.prev^ ;
	END ;
	WHILE list.next # NIL DO
		INC( count ) ;
		list := list.next^ ;
	END ;
	RETURN count ;
END Length ;

PROCEDURE EqualDList( listA : REF DList ; listB : REF DList ) : BOOLEAN =
VAR
	currentListA : REF DList := NIL ;
	currentListB : REF DList := NIL ;
BEGIN
	IF NOT IsEmpty( listA^ ) AND NOT IsEmpty( listB^ ) THEN
		IF Length( listA^ ) = Length( listB^ ) THEN
			GoToBeginning( currentListA , listA ) ;
			GoToBeginning( currentListB , listB ) ;
			LOOP
				IF currentListA.cur # NIL AND currentListB.cur # NIL THEN
					IF NOT Equal( currentListA.cur^ , currentListB.cur^ ) THEN
						RETURN FALSE ;
					END ;
				ELSIF currentListA.cur # currentListB.cur THEN
					RETURN FALSE ;
				END ;
				IF currentListA.next = NIL THEN
					EXIT ;
				ELSE
					currentListA := currentListA^.next ;
					currentListB := currentListB^.next ;
				END ;
			END ;
			RETURN TRUE ;
		ELSE
			RETURN FALSE ;
		END ;
	ELSIF IsEmpty( listA^ ) AND IsEmpty( listB^ ) THEN
		RETURN TRUE ;
	ELSE
		RETURN FALSE ;
	END ;
END EqualDList ;

PROCEDURE GoToBeginning( begoflist : REF DList ; list : REF DList ) =
VAR
	curlist := list ;
BEGIN
	<* ASSERT begoflist # NIL *>
	WHILE curlist^.prev # NIL DO
		curlist := curlist^.prev ;
	END ;
	begoflist^.cur := curlist^.cur ;
	begoflist^.prev := curlist^.prev ;
	begoflist^.next := curlist^.next ;
END GoToBeginning ;

PROCEDURE GoToEnd( endoflist : REF DList ; list : REF DList ) =
VAR
	curlist := list ;
BEGIN
	<* ASSERT endoflist # NIL *>
	WHILE curlist^.next # NIL DO
		curlist := curlist^.next ;
	END ;
	endoflist^.cur := curlist^.cur ;
	endoflist^.prev := curlist^.prev ;
	endoflist^.next := curlist^.next ;
END GoToEnd ;

PROCEDURE DeepCopyDList( newlist : REF DList ; list : REF DList ) =
BEGIN
	<* ASSERT list # NIL *>
	<* ASSERT newlist # NIL *>
	DeepCopy( newlist^.cur , list^.cur ) ;
	IF list^.next # NIL THEN
		DeepCopyDList( newlist^.next , list^.next ) ;
	ELSE
		newlist^.next := NIL ;
	END ;
	IF list.prev # NIL THEN
		DeepCopyDList( newlist^.prev , list^.prev ) ;
	ELSE
		newlist^.prev := NIL ;
	END ;
END DeepCopyDList ;

(* TODO Can you use readonly for these? *)
PROCEDURE AppendNode( list : REF DList ; NodeToAppend : REF T ) =
VAR
	templistptr : REF DList := NEW( REF DList ) ;
BEGIN
	<* ASSERT list # NIL *>
	<* ASSERT NodeToAppend # NIL *>
	IF NOT IsEmpty( list^ ) THEN
		GoToEnd( templistptr , list ) ;
		templistptr^.next := NEW( REF DList , cur := NodeToAppend , prev := templistptr , next := NIL ) ;
	ELSE
		list^.cur := NodeToAppend ;
	END ;
END AppendNode ;

(* TODO listB can probably be readonly here. Do that more often *)
PROCEDURE AppendDList( listA : REF DList ; listB : REF DList ) =
VAR
	templistptrA : REF DList := NEW( REF DList ) ;
	templistptrB : REF DList := NEW( REF DList ) ;
	newtemplistptrB : REF DList := NEW( REF DList ) ;
BEGIN
	IF NOT IsEmpty( listA^ ) AND NOT IsEmpty( listB^ ) THEN
		GoToEnd( templistptrA , listA ) ;
		GoToBeginning( templistptrB , listB ) ;
		DeepCopyDList( newtemplistptrB , templistptrB ) ;
		templistptrA^.next := newtemplistptrB ;
		newtemplistptrB^.prev := templistptrA ;
	ELSIF IsEmpty( listA^ ) AND NOT IsEmpty( listB^ ) THEN
		DeepCopyDList( listA , listB ) ;
	END ;
END AppendDList ;

PROCEDURE DefaultDList( list : REF DList ) =
BEGIN
	list^.cur := NIL ;
	list^.prev := NIL ;
	list^.next := NIL ;
END DefaultDList ;

BEGIN END Node .
