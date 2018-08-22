INTERFACE Node ;

(***********)
(* Imports *)
(***********)
IMPORT TextList ;

(**************)
(* Exceptions *)
(**************)

EXCEPTION NoMatchException ;

(*********)
(* Types *)
(*********)

TYPE Category = { NonTerminal , Constant , Identifier , Placeholder , NoCategory } ;

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
	(* Note: By convention, terminals have empty lists for
	children, NOT NIL for children. *)
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

PROCEDURE FindAllNodesWithCategory( newlist : REF DList ; root : REF T ; cat : Category ) ;

PROCEDURE FindAllNonterms( newlist : REF DList ; root : REF T ; NontermVal : TEXT ) ;

(* Returns the actual parent node and NOT a deep copy. *)
(* If root matches childnode, returns NIL. Otherwise, returns reference to parent node.
Throws NoMatchException is it cannot find childnode. *)
PROCEDURE GetParent( root : REF T ; childnode : REF T ) : REF T RAISES { NoMatchException } ;

(* Follow a path and return all the nodes at the end of it *)
(* Assumptions:
- path is nonempty and not NIL
- root node is a nonterminal and is the implicit starting point of the path
*)
(* TODO Ensure that it returns empty list with all NILs if no match *)
PROCEDURE FollowPath( list : REF DList ; root : REF T ; path : TextList.T ) ;

(* DList *)
PROCEDURE IsEmpty( list : REF DList ) : BOOLEAN ;

PROCEDURE Length( list : REF DList ) : CARDINAL ;

PROCEDURE EqualDList( listA : REF DList ; listB : REF DList ) : BOOLEAN ;

PROCEDURE GoToBeginning( list : REF DList ) : REF DList ;

PROCEDURE GoToEnd( list : REF DList ) : REF DList ;

PROCEDURE DeepCopyDList( newlist : REF DList ; list : REF DList ) ;

PROCEDURE AppendNode( list : REF DList ; NodeToAppend : REF T ) ;

PROCEDURE AppendDList( listA : REF DList ; listB : REF DList ) ;

PROCEDURE AppendDListDeep( listA : REF DList ; listB : REF DList ) ;

PROCEDURE PrependNode( list : REF DList ; NodeToAppend : REF T ) ;

PROCEDURE PrependDList( listA : REF DList ; listB : REF DList ) ;

PROCEDURE DeleteFromList( list : REF DList ) ;

PROCEDURE DeleteList( list : REF DList ) ;

PROCEDURE DefaultDList( list : REF DList ) ;

PROCEDURE DebugList( list : REF DList ) ;

END Node.
