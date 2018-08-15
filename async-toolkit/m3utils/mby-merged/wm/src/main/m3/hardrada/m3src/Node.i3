INTERFACE Node ;

(*************)
(* Constants *)
(*************)

CONST Brand = "SomeNode" ;

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
	children : REF ARRAY OF REF T := NIL ;
END ;

(**************)
(* Procedures *)
(**************)

PROCEDURE Equal( NodeA , NodeB : T ) : BOOLEAN ;

PROCEDURE DeepCopy( CurrentNode : REF T ) : REF T ;

END Node.
