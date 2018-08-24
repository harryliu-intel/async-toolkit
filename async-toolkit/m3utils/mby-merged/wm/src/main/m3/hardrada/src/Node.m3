MODULE Node ;

(***********)
(* Imports *)
(***********)

IMPORT IO ;
IMPORT Fmt ;
IMPORT TextList ;

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
	<* ASSERT NewNode # NIL *>
	<* ASSERT CurrentNode # NIL *>
	NewNode^.val := CurrentNode^.val ;
	NewNode^.cat := CurrentNode^.cat ;
	NewNode^.children := NEW( REF DList ) ;
	IF CurrentNode^.children # NIL THEN
		IF NewNode^.children = NIL THEN
			NewNode^.children := NEW( REF DList ) ;
		END ;
		DeepCopyDList( NewNode^.children , CurrentNode^.children ) ;
	END ;
END DeepCopy ;

PROCEDURE FindAllNodesWithCategoryDeep( newlist : REF DList ; root : REF T ; cat : Category ) =
VAR
	current_child : REF DList := NIL ;
	templist := NEW( REF DList ) ;
BEGIN
	<* ASSERT root # NIL *>
	<* ASSERT newlist # NIL *>
	DefaultDList( newlist ) ;
	DefaultDList( templist ) ;
	IF root^.cat = cat THEN
		AppendNode( newlist , root ) ;
	END ;
	IF root^.cat = Category.NonTerminal THEN
		(* TODO Make this a function... somehow *)
		current_child := GoToBeginning( root^.children ) ;
		WHILE current_child # NIL DO
			FindAllNodesWithCategoryDeep( templist , current_child^.cur , cat ) ;
			AppendDListDeepWithShallowNodes( newlist , templist ) ;
			current_child := current_child^.next ;
		END ;
	END ;
END FindAllNodesWithCategoryDeep ;

PROCEDURE FindAllNodesWithCategory( newlist : REF DList ; root : REF T ; cat : Category ) =
VAR
	current_child : REF DList := NIL ;
	templist := NEW( REF DList ) ;
BEGIN
	<* ASSERT root # NIL *>
	<* ASSERT newlist # NIL *>
	DefaultDList( newlist ) ;
	DefaultDList( templist ) ;
	IF root^.cat = cat THEN
		AppendNode( newlist , root ) ;
	END ;
	IF root^.cat = Category.NonTerminal THEN
		(* TODO Make this a function... somehow *)
		current_child := GoToBeginning( root^.children ) ;
		WHILE current_child # NIL DO
			FindAllNodesWithCategory( templist , current_child^.cur , cat ) ;
			AppendDListDeep( newlist , templist ) ;
			current_child := current_child^.next ;
		END ;
	END ;
END FindAllNodesWithCategory ;

(* TODO Probably good to generalize this function so it can work for GetParent as
well. *)
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

(* TODO Assumes node only has one parent. Write the function later
to check if the tree is valid and assert this at the start. *)
PROCEDURE GetParent( root : REF T ; childnode : REF T ) : REF T RAISES { NoMatchException } =
VAR
	rootchild : REF DList := NIL ;
	tempparent : REF T := NIL ;
BEGIN
	<* ASSERT childnode # NIL *>
	IF root = NIL THEN
		RAISE NoMatchException ;
	ELSIF root = childnode THEN
		RETURN NIL ;
	ELSE
		rootchild := root^.children ;
		(* TODO Assert to make sure it's not empty and is NIL if it should be? *)
		WHILE rootchild # NIL DO
			TRY
				tempparent := GetParent( rootchild^.cur , childnode ) ;
				IF tempparent = NIL THEN
					RETURN root ;
				ELSE
					RETURN tempparent ;
				END ;
			EXCEPT
				| NoMatchException => rootchild := rootchild^.next ;
			END ;
		END ;
		RAISE NoMatchException ;
	END ;
END GetParent ;

PROCEDURE FollowPath( list : REF DList ; root : REF T ; path : TextList.T ) =
VAR
	return_end_of_path := NEW( REF DList ) ;
	recursive_return_end_of_path := NEW( REF DList ) ;
	recursive_templist := NEW( REF DList ) ;
	current_child : REF DList := NIL ;
BEGIN
	<* ASSERT TextList.Length( path ) > 0 *>
	<* ASSERT list # NIL *>
	<* ASSERT root # NIL *>
	<* ASSERT root^.cat = Category.NonTerminal *>
	DefaultDList( return_end_of_path ) ;
	DefaultDList( recursive_return_end_of_path ) ;
	DefaultDList( recursive_templist ) ;
	DefaultDList( list ) ;
	current_child := GoToBeginning( root^.children ) ;
	LOOP
		IF current_child^.cur^.val = path.head THEN
			AppendNode( return_end_of_path , current_child^.cur ) ;
		END ;
		IF current_child^.next = NIL THEN
			EXIT ;
		ELSE
			current_child := current_child^.next ;
		END ;
	END ;
	IF TextList.Length( path ) > 1 THEN
		path := path.tail ;
		current_child := GoToBeginning( return_end_of_path ) ;
		LOOP
			FollowPath( recursive_templist , current_child^.cur , path ) ;
			AppendDList( recursive_return_end_of_path , recursive_templist ) ;
			IF current_child^.next = NIL THEN
				EXIT ;
			ELSE
				current_child := current_child^.next ;
			END ;
		END ;
		recursive_return_end_of_path := GoToBeginning( recursive_return_end_of_path ) ;
		list^.cur := recursive_return_end_of_path^.cur ;
		list^.prev := recursive_return_end_of_path^.prev ;
		list^.next := recursive_return_end_of_path^.next ;
	ELSE
		return_end_of_path := GoToBeginning( return_end_of_path ) ;
		list^.cur := return_end_of_path^.cur ;
		list^.prev := return_end_of_path^.prev ;
		list^.next := return_end_of_path^.next ;
	END ;
END FollowPath ;

(* DList *)

PROCEDURE IsEmpty( list : REF DList ) : BOOLEAN =
BEGIN
	<* ASSERT list # NIL *>
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
	WHILE templist^.next # NIL DO
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

PROCEDURE ShallowCopyDList( newlist : REF DList ; list : REF DList ) =
VAR
	templist : REF DList := NIL ;
	myprev : REF DList := NIL ;
BEGIN
	<* ASSERT list # NIL *>
	<* ASSERT newlist # NIL *>
	templist := GoToBeginning( list ) ;
	myprev := NIL ;
	WHILE templist # NIL DO
		(* What is cur? *)
		newlist^.cur := templist^.cur ;
		(* What is prev? *)
		newlist^.prev := myprev ;
		(* What is next? *)
		IF templist^.next # NIL THEN
			newlist^.next := NEW( REF DList ) ;
			myprev := newlist ;
			newlist := newlist^.next ;
		ELSE
			newlist^.next := NIL ;
		END ;
		(* Next loop iteration *)
		templist := templist^.next ;
	END ;
END ShallowCopyDList ;

PROCEDURE DeepCopyDList( newlist : REF DList ; list : REF DList ) =
VAR
	templist : REF DList := NIL ;
	myprev : REF DList := NIL ;
BEGIN
	<* ASSERT list # NIL *>
	<* ASSERT newlist # NIL *>
	templist := GoToBeginning( list ) ;
	myprev := NIL ;
	WHILE templist # NIL DO
		(* What is cur? *)
		IF templist^.cur # NIL THEN
			IF newlist^.cur = NIL THEN
				newlist^.cur := NEW( REF T ) ;
			END ;
			DeepCopy( newlist^.cur , templist^.cur ) ;
		ELSE
			newlist^.cur := NIL ;
		END ;
		(* What is prev? *)
		newlist^.prev := myprev ;
		(* What is next? *)
		IF templist^.next # NIL THEN
			newlist^.next := NEW( REF DList ) ;
			myprev := newlist ;
			newlist := newlist^.next ;
		ELSE
			newlist^.next := NIL ;
		END ;
		(* Next loop iteration *)
		templist := templist^.next ;
	END ;
END DeepCopyDList ;

(* TODO Can you use readonly for these? *)
PROCEDURE AppendNode( list : REF DList ; NodeToAppend : REF T ) =
VAR
	endoflist : REF DList := NIL ;
BEGIN
	<* ASSERT list # NIL *>
	<* ASSERT NodeToAppend # NIL *>
	IF NOT IsEmpty( list ) THEN
		endoflist := GoToEnd( list ) ;
		endoflist^.next := NEW( REF DList , cur := NodeToAppend , prev := endoflist , next := NIL ) ;
	ELSE
		list^.cur := NodeToAppend ;
	END ;
END AppendNode ;

PROCEDURE AppendDList( listA : REF DList ; listB : REF DList ) =
VAR
	templistptrA : REF DList := NIL ;
	templistptrB : REF DList := NIL ;
BEGIN
	<* ASSERT listA # NIL *>
	<* ASSERT listB # NIL *>
	IF NOT IsEmpty( listA ) AND NOT IsEmpty( listB ) THEN
		templistptrA := GoToEnd( listA ) ;
		templistptrB := GoToBeginning( listB ) ;
		templistptrA^.next := templistptrB ;
		templistptrB^.prev := templistptrA ;
	ELSIF IsEmpty( listA ) AND NOT IsEmpty( listB ) THEN
		listA^.cur := listB^.cur ;
		listA^.next := listB^.next ;
		listA^.prev := listB^.prev ;
	END ;
END AppendDList ;

(* TODO listB can probably be readonly here. Do that more often *)
PROCEDURE AppendDListDeepWithShallowNodes( listA : REF DList ; listB : REF DList ) =
VAR
	templistptrA : REF DList := NIL ;
	templistptrB : REF DList := NIL ;
	newtemplistptrB : REF DList := NEW( REF DList ) ;
BEGIN
	<* ASSERT listA # NIL *>
	<* ASSERT listB # NIL *>
	IF NOT IsEmpty( listA ) AND NOT IsEmpty( listB ) THEN
		templistptrA := GoToEnd( listA ) ;
		templistptrB := GoToBeginning( listB ) ;
		ShallowCopyDList( newtemplistptrB , templistptrB ) ;
		(* DeepCopyDList( newtemplistptrB , templistptrB ) ; *)
		templistptrA^.next := newtemplistptrB ;
		newtemplistptrB^.prev := templistptrA ;
	ELSIF IsEmpty( listA ) AND NOT IsEmpty( listB ) THEN
		ShallowCopyDList( listA , listB ) ;
		(* DeepCopyDList( listA , listB ) ; *)
	END ;
END AppendDListDeepWithShallowNodes ;

(* TODO listB can probably be readonly here. Do that more often *)
PROCEDURE AppendDListDeep( listA : REF DList ; listB : REF DList ) =
VAR
	templistptrA : REF DList := NIL ;
	templistptrB : REF DList := NIL ;
	newtemplistptrB : REF DList := NEW( REF DList ) ;
BEGIN
	<* ASSERT listA # NIL *>
	<* ASSERT listB # NIL *>
	IF NOT IsEmpty( listA ) AND NOT IsEmpty( listB ) THEN
		templistptrA := GoToEnd( listA ) ;
		templistptrB := GoToBeginning( listB ) ;
		DeepCopyDList( newtemplistptrB , templistptrB ) ;
		templistptrA^.next := newtemplistptrB ;
		newtemplistptrB^.prev := templistptrA ;
	ELSIF IsEmpty( listA ) AND NOT IsEmpty( listB ) THEN
		DeepCopyDList( listA , listB ) ;
	END ;
END AppendDListDeep ;

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
	<* ASSERT listA # NIL *>
	<* ASSERT listB # NIL *>
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
	IF list^.next # NIL THEN
		list^.next^.prev := list^.prev ;
	END ;
	DefaultDList( list ) ;
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

PROCEDURE DebugList( list : REF DList ) =
VAR
	templist : REF DList := NIL ;
BEGIN
	<* ASSERT list # NIL *>
	IF IsEmpty( list ) THEN
		IO.Put( "( Empty )\n" ) ;
	ELSE
		templist := GoToBeginning( list ) ;
		WHILE templist^.next # NIL DO
			IO.Put( templist^.cur^.val ) ;
			IO.Put( "->" ) ;
			templist := templist^.next ;
		END ;
		IO.Put( templist^.cur^.val ) ;
		IO.Put( "\n" ) ;
	END ;
END DebugList ;

BEGIN END Node .
