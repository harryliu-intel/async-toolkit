INTERFACE DepGraph ;

(*************)
(** Imports **)
(*************)
IMPORT Node ;

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

(* Assumes parse_root has one placeholder node. THAT node is assumed to be the
procedure body and is changed to a NonTerminal. Its children are then erased
and populated. *)
(* Note: subdepgraphs are added as children to the placeholder nodes. The category of
the placeholder node is then changed to NonTerminal. *)
PROCEDURE ConstructParseTree( parse_root : REF Node.T ; root : REF T ; stmt_sep : TEXT := "" ) ;

END DepGraph .
