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
	current_child : REF DList := NIL ;
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
		current_child := GoToBeginning( root^.children ) ;
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

PROCEDURE IsEmpty( list : REF DList ) : BOOLEAN =
BEGIN
	RETURN ( list^.cur = NIL AND list^.prev = NIL AND list^.next = NIL ) ;
END IsEmpty ;

PROCEDURE Length( list : REF DList ) : CARDINAL =
VAR
	count := 0 ;
	templist : REF DList := NIL ;
BEGIN
	IF IsEmpty( list ) THEN
		RETURN 0 ;
	END ;
	INC( count ) ;
	templist := GoToBeginning( list ) ;
	WHILE templist.next # NIL DO
		INC( count ) ;
		templist := templist^.next ;
	END ;
	RETURN count ;
END Length ;

PROCEDURE EqualDList( listA : REF DList ; listB : REF DList ) : BOOLEAN =
VAR
	currentListA : REF DList := NIL ;
	currentListB : REF DList := NIL ;
BEGIN
	IF NOT IsEmpty( listA ) AND NOT IsEmpty( listB ) THEN
		IF Length( listA ) = Length( listB ) THEN
			currentListA := GoToBeginning( listA ) ;
			currentListB := GoToBeginning( listB ) ;
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
	ELSIF IsEmpty( listA ) AND IsEmpty( listB ) THEN
		RETURN TRUE ;
	ELSE
		RETURN FALSE ;
	END ;
END EqualDList ;

(* Return pointer. Otherwise, you need to pass a REF REF to this
function and that can be tricky to acquire and maintain in higher
level code.*)
PROCEDURE GoToBeginning( list : REF DList ) : REF DList =
BEGIN
	<* ASSERT list # NIL *>
	WHILE list^.prev # NIL DO
		list := list^.prev ;
	END ;
	RETURN list ;
END GoToBeginning ;

(* Return pointer. Otherwise, you need to pass a REF REF to this
function and that can be tricky to acquire and maintain in higher
level code.*)
PROCEDURE GoToEnd( list : REF DList ) : REF DList =
BEGIN
	<* ASSERT list # NIL *>
	WHILE list^.next # NIL DO
		list := list^.next ;
	END ;
	RETURN list ;
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

PROCEDURE ShallowCopyDList( newlist : REF DList ; list : REF DList ) =
BEGIN
	<* ASSERT list # NIL *>
	<* ASSERT newlist # NIL *>
	newlist^.cur := list^.cur ;
	IF list^.next # NIL THEN
		ShallowCopyDList( newlist^.next , list^.next ) ;
	ELSE
		newlist^.next := NIL ;
	END ;
	IF list.prev # NIL THEN
		ShallowCopyDList( newlist^.prev , list^.prev ) ;
	ELSE
		newlist^.prev := NIL ;
	END ;
END ShallowCopyDList ;

(* TODO Can you use readonly for these? *)
PROCEDURE AppendNode( list : REF DList ; NodeToAppend : REF T ) =
VAR
	endoflist : REF DList := NIL ;
BEGIN
	<* ASSERT list # NIL *>
	<* ASSERT NodeToAppend # NIL *>
	endoflist := GoToEnd( list ) ;
	endoflist^.next := NEW( REF DList , cur := NodeToAppend , prev := endoflist , next := NIL ) ;
END AppendNode ;

(* TODO listB can probably be readonly here. Do that more often *)
PROCEDURE AppendDList( listA : REF DList ; listB : REF DList ) =
VAR
	templistptrA : REF DList := NIL ;
	templistptrB : REF DList := NIL ;
	newtemplistptrB : REF DList := NEW( REF DList ) ;
BEGIN
	IF NOT IsEmpty( listA ) AND NOT IsEmpty( listB ) THEN
		templistptrA := GoToEnd( listA ) ;
		templistptrB := GoToBeginning( listB ) ;
		DeepCopyDList( newtemplistptrB , templistptrB ) ;
		templistptrA^.next := newtemplistptrB ;
		newtemplistptrB^.prev := templistptrA ;
	ELSIF IsEmpty( listA ) AND NOT IsEmpty( listB ) THEN
		DeepCopyDList( listA , listB ) ;
	END ;
END AppendDList ;

PROCEDURE PrependNode( list : REF DList ; NodeToAppend : REF T ) =
VAR
	templistptr : REF DList := NIL ;
BEGIN
	<* ASSERT list # NIL *>
	<* ASSERT NodeToAppend # NIL *>
	IF NOT IsEmpty( list ) THEN
		templistptr := GoToBeginning( list ) ;
		templistptr^.prev := NEW( REF DList , cur := NodeToAppend , prev := NIL , next := templistptr ) ;
	ELSE
		list^.cur := NodeToAppend ;
	END ;
END PrependNode ;

PROCEDURE PrependDList( listA : REF DList ; listB : REF DList ) =
VAR
	templistptrA : REF DList := NIL ;
	templistptrB : REF DList := NEW( REF DList ) ;
	newtemplistptrB : REF DList := NEW( REF DList ) ;
BEGIN
	IF NOT IsEmpty( listA ) AND NOT IsEmpty( listB ) THEN
		templistptrA := GoToBeginning( listA ) ;
		templistptrB := GoToEnd( listB ) ;
		DeepCopyDList( newtemplistptrB , templistptrB ) ;
		templistptrA^.prev := newtemplistptrB ;
		newtemplistptrB^.next := templistptrA ;
	ELSIF IsEmpty( listA ) AND NOT IsEmpty( listB ) THEN
		DeepCopyDList( listA , listB ) ;
	END ;
END PrependDList ;

PROCEDURE DeleteFromList( list : REF DList ) =
BEGIN
	<* ASSERT list # NIL *>
	IF list^.prev # NIL THEN
		list^.prev^.next := list^.next ;
	END ;
	IF list^.next^.prev # NIL THEN
		list^.next^.prev := list^.prev ;
	END ;
END DeleteFromList ;

PROCEDURE DeleteList( list : REF DList ) =
VAR
	child : REF DList := NIL ;
	nextchild := NEW( REF DList ) ;
BEGIN
	(* TODO Should you document if something meaningful happens when you use the
	same list for both args? *)
	child := GoToBeginning( list ) ;
	LOOP
		nextchild := child^.next ;
		DeleteFromList( child ) ;
		child := nextchild ;
		IF child = NIL THEN
			EXIT ;
		END ;
	END ;
END DeleteList ;

PROCEDURE DefaultDList( list : REF DList ) =
BEGIN
	<* ASSERT list # NIL *>
	list^.cur := NIL ;
	list^.prev := NIL ;
	list^.next := NIL ;
END DefaultDList ;

BEGIN END Node .
