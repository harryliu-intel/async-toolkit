INTERFACE Node ;

(*********)
(* Types *)
(*********)

TYPE Category = { NonTerminal , Constant , Identifier , NoCategory } ;

TYPE T = RECORD

	(* Value of the node. For instance, val := "+"
	for the binary operation "add". *)
	val : TEXT := "" ;

	(* The node's category... Look at Category TYPE
	definition for details. *)
	cat : Category := Category.NoCategory ;

	(* An array of the node's child nodes, or the
	nodes to which our current node connects in
	the tree. *)
	children : REF DList := NIL ;
END ;

TYPE DList = RECORD
	(* Current element *)
	cur : REF T := NIL ;
	(* Previous element *)
	prev : REF DList := NIL ;
	(* Next element*)
	next : REF DList := NIL ;
END ;

(**************)
(* Procedures *)
(**************)

(* Node *)
PROCEDURE Equal( NodeA , NodeB : T ) : BOOLEAN ;

PROCEDURE DeepCopy( NewNode : REF T ; CurrentNode : REF T ) ;

PROCEDURE FindAllNonterms( newlist : REF DList ; root : REF T ; NontermVal : TEXT ) ;

(* DList *)
PROCEDURE IsEmpty( list : REF DList ) : BOOLEAN ;

PROCEDURE Length( list : REF DList ) : CARDINAL ;

PROCEDURE EqualDList( listA : REF DList ; listB : REF DList ) : BOOLEAN ;

PROCEDURE GoToBeginning( list : REF DList ) : REF DList ;

PROCEDURE GoToEnd( list : REF DList ) : REF DList ;

PROCEDURE DeepCopyDList( newlist : REF DList ; list : REF DList ) ;

PROCEDURE ShallowCopyDList( newlist : REF DList ; list : REF DList ) ;

PROCEDURE AppendNode( list : REF DList ; NodeToAppend : REF T ) ;

PROCEDURE AppendDList( listA : REF DList ; listB : REF DList ) ;

PROCEDURE PrependNode( list : REF DList ; NodeToAppend : REF T ) ;

PROCEDURE PrependDList( listA : REF DList ; listB : REF DList ) ;

PROCEDURE DeleteFromList( list : REF DList ) ;

PROCEDURE DeleteList( list : REF DList ) ;

PROCEDURE DefaultDList( list : REF DList ) ;

END Node.
