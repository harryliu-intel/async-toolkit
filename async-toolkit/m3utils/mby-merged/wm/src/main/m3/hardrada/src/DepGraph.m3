MODULE DepGraph ;

(*************)
(** Imports **)
(*************)

IMPORT Node ;

(************************)
(** Visible Procedures **)
(************************)

PROCEDURE ConstructParseTree( parse_root : REF Node.T ; root : REF T ; stmt_sep : TEXT := "" ) =
VAR
	placeholder_list : REF Node.DList := NIL ;
	children_list : REF Node.DList := NIL ;
	tempnext : REF Node.DList := NIL ;
	stmt_sep_node : REF Node.T := NIL ;
BEGIN
	<* ASSERT parse_root # NIL *>
	<* ASSERT root # NIL *>
	placeholder_list := NEW( REF Node.DList ) ;
	Node.FindAllNodesWithCategory( placeholder_list , parse_root , Node.Category.Placeholder ) ;
	<* ASSERT Node.Length( placeholder_list ) = 1 *>
	placeholder_list^.cur^.cat := Node.Category.NonTerminal ;
	ConstructParseTreeRootList( placeholder_list^.cur^.children , root ) ;
	<* ASSERT placeholder_list^.cur^.children # NIL *>
	(* Add separators *)
	children_list := Node.GoToBeginning( placeholder_list^.cur^.children ) ;
	WHILE children_list # NIL DO
		tempnext := children_list^.next ;
		stmt_sep_node := NEW( REF Node.T , val := stmt_sep , cat := Node.Category.Constant , children := NIL ) ;
		children_list^.next := NEW( REF Node.DList , cur := stmt_sep_node , next := tempnext , prev := children_list ) ;
		IF tempnext # NIL THEN
			tempnext^.prev := children_list^.next ; 
		END ;
		children_list := tempnext ;
	END ;
END ConstructParseTree ;

(***********************)
(** Hidden Procedures **)
(***********************)

PROCEDURE ConstructParseTreeRootList( parse_root : REF Node.DList ; root : REF T ) =
VAR
	placeholder_list : REF Node.DList := NIL ;
BEGIN
	<* ASSERT root # NIL *>
	<* ASSERT parse_root # NIL *>
	Node.DefaultDList( parse_root ) ;
	parse_root^.cur := NEW( REF Node.T ) ;
	Node.DeepCopy( parse_root^.cur , root^.parse_root ) ;
	placeholder_list := NEW( REF Node.DList ) ;
	Node.FindAllNodesWithCategory( placeholder_list , parse_root^.cur , Node.Category.Placeholder ) ;
	IF root^.subdepgraph = NIL THEN
		<* ASSERT Node.Length( placeholder_list ) = 0 *>
		IF root^.next # NIL THEN
			parse_root^.next := NEW( REF Node.DList ) ;
			ConstructParseTreeRootList( parse_root^.next , root^.next ) ;
		END ;
	ELSE
		<* ASSERT Node.Length( placeholder_list ) = NUMBER( root^.subdepgraph^ ) *>
		FOR sdg_index := FIRST( root^.subdepgraph^ ) TO LAST( root^.subdepgraph^ ) DO
			ConstructParseTreeRootList( placeholder_list^.cur^.children , root^.subdepgraph[ sdg_index ] ) ;
			placeholder_list^.cur^.cat := Node.Category.NonTerminal ;
		END ;
	END ;
END ConstructParseTreeRootList ;

BEGIN END DepGraph .
