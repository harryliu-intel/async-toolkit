INTERFACE DepGraph ;

(*************)
(** Imports **)
(*************)
IMPORT Node ;
IMPORT TextList ;
IMPORT REFANYList ;

(***********)
(** Types **)
(***********)

(*
next : dependency graph node
deps : dependency graph nodes in order of occurrence in dependency graph
parse_root : root node of corresponding parse tree (may contain placeholder nodes for other statements)
subdepgraphs : array of dependency graph roots in order of placeholder nodes in parse tree

Note: Any placeholders in parse_root are reserved for nested statements. Nothing more.
*)
TYPE T = RECORD
	next : REF T := NIL ;
	deps : REF ARRAY OF REF T := NIL ;
	parse_root : REF Node.T := NIL ;
	subdepgraph : REF ARRAY OF REF T := NIL ;
END ;

TYPE DepGraphParams = RECORD
	(* Start symbol is simply a list of statements
	with arbitrarily-placed optional separators.
	Start symbol is recognized as nonterminal node
	with value equal to start_symbol_val. *)
	start_symbol_val : TEXT := "" ;
	(* Optional separator between statements. Could
	technically be placed anywhere in the children
	list of a start symbol. Optional separator is
	a terminal node with value equal to separator. *)
	separator : TEXT := "" ;
	(* Path to get to the main start symbol in the procedure
	body from the procedure block. *)
	ProcedureBodyToSSPath : TextList.T := NIL ;
END ;

(* Assumes parse_root has one placeholder node. THAT node is assumed to be the
procedure body and is changed to a NonTerminal. Its children are then erased
and populated. *)
(* Note: subdepgraphs are added as children to the placeholder nodes. The category of
the placeholder node is then changed to NonTerminal. *)
PROCEDURE ConstructParseTree( parse_root : REF Node.T ; root : REF T ; depgraph_pms : REF DepGraphParams ) ;

(* Convert parse_root to my_depgraph. Doesn't break parse_root. Overwrites my_depgraph. *)
(* TODO Ensure readonlys and such by adding READONLY keyword everywhere in this code when necessary *)
PROCEDURE GetDepGraph( my_depgraph : REF T ; parse_root : REF Node.T ; depgraph_pms : REF DepGraphParams ) ;

PROCEDURE DefaultDepGraph( my_depgraph : REF T ) ;

END DepGraph .
