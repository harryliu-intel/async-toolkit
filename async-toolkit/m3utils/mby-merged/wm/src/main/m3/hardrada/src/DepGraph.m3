MODULE DepGraph ;

(*************)
(** Imports **)
(*************)

IMPORT Node ;
IMPORT IO ;
IMPORT Fmt ;
IMPORT Text ;

(************************)
(** Visible Procedures **)
(************************)

PROCEDURE ConstructParseTree( parse_root : REF Node.T ; root : REF T ; depgraph_pms : REF DepGraphParams ) =
VAR
	placeholder_list : REF Node.DList := NIL ;
	children_list : REF Node.DList := NIL ;
	tempnext : REF Node.DList := NIL ;
	stmt_sep_node : REF Node.T := NIL ;
BEGIN
	<* ASSERT parse_root # NIL *>
	<* ASSERT root # NIL *>
	<* ASSERT root^.parse_root # NIL *>
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
		stmt_sep_node := NEW( REF Node.T , val := depgraph_pms^.separator , cat := Node.Category.Constant , children := NIL ) ;
		children_list^.next := NEW( REF Node.DList , cur := stmt_sep_node , next := tempnext , prev := children_list ) ;
		IF tempnext # NIL THEN
			tempnext^.prev := children_list^.next ; 
		END ;
		children_list := tempnext ;
	END ;
END ConstructParseTree ;

PROCEDURE GetDepGraph( my_depgraph : REF T ; parse_root : REF Node.T ; depgraph_pms : REF DepGraphParams ) =
VAR
	ss : REF Node.DList := NIL ;
BEGIN
	(* TODO Ensure that my_depgraph is blank? *)
	<* ASSERT my_depgraph # NIL *>
	<* ASSERT parse_root # NIL *>
	<* ASSERT depgraph_pms # NIL *>
	ss := NEW( REF Node.DList ) ;
	Node.FollowPath( ss , parse_root , depgraph_pms^.ProcedureBodyToSSPath ) ;
	<* ASSERT Node.Length( ss ) = 1 *>
	GetDepGraphFromStartSymbol( my_depgraph , ss^.cur , depgraph_pms ) ;
END GetDepGraph ;

(* TOOD Is this really necessary since we have NEW? *)
PROCEDURE DefaultDepGraph( my_depgraph : REF T ) =
BEGIN
	<* ASSERT my_depgraph # NIL *>
	my_depgraph^.next := NIL ;
	my_depgraph^.deps := NIL ;
	my_depgraph^.parse_root := NIL ;
	my_depgraph^.subdepgraph := NIL ;
END DefaultDepGraph ;

(***********************)
(** Hidden Procedures **)
(***********************)

PROCEDURE ConstructParseTreeRootList( parse_root : REF Node.DList ; root : REF T ) =
VAR
	placeholder_list : REF Node.DList := NIL ;
BEGIN
	<* ASSERT root # NIL *>
	<* ASSERT root^.parse_root # NIL *>
	<* ASSERT parse_root # NIL *>
	Node.DefaultDList( parse_root ) ;
	parse_root^.cur := NEW( REF Node.T ) ;
	Node.DeepCopy( parse_root^.cur , root^.parse_root ) ;
	placeholder_list := NEW( REF Node.DList ) ;
	Node.FindAllNodesWithCategory( placeholder_list , parse_root^.cur , Node.Category.Placeholder ) ;
	IF root^.subdepgraph = NIL THEN
		<* ASSERT Node.Length( placeholder_list ) = 0 *>
		IO.Put( "Hit bottom level!\n" ) ;
		IF root^.next # NIL THEN
			parse_root^.next := NEW( REF Node.DList ) ;
			ConstructParseTreeRootList( parse_root^.next , root^.next ) ;
		END ;
	ELSE
		IO.Put( "Have subdepgraphs!\n" ) ;
		IO.Put( "Placeholder list: " ) ;
		Node.DebugList( placeholder_list ) ;
		IO.Put( "Length of subdepgraph list: " & Fmt.Int( NUMBER( root^.subdepgraph^ ) ) & "\n" ) ;
		IO.Put( "Parse root head: " & root^.parse_root^.val & "\n" ) ;
		IF root^.parse_root^.children^.cur^.children^.next^.next^.next^.cur^.cat # Node.Category.Placeholder THEN
			IO.Put( "Value: " & root^.parse_root^.children^.cur^.children^.next^.next^.next^.cur^.val & "\n" ) ;
			<* ASSERT FALSE *>
		END ;
		<* ASSERT Node.Length( placeholder_list ) = NUMBER( root^.subdepgraph^ ) *>
		FOR sdg_index := FIRST( root^.subdepgraph^ ) TO LAST( root^.subdepgraph^ ) DO
			ConstructParseTreeRootList( placeholder_list^.cur^.children , root^.subdepgraph[ sdg_index ] ) ;
			placeholder_list^.cur^.cat := Node.Category.NonTerminal ;
		END ;
	END ;
END ConstructParseTreeRootList ;

(* Note: Parse trees are deep copied. No need to worry about this procedure breaking your parse trees. *)
PROCEDURE GetDepGraphFromStartSymbol( my_depgraph : REF T ; parse_root : REF Node.T ; depgraph_pms : REF DepGraphParams ) =
VAR
	tempchild : REF Node.DList := NIL ;
	tempdepgraph : REF T := NIL ;
	tempstartsymbs : REF Node.DList := NIL ;
	templowerleveltree : REF Node.T := NIL ;
BEGIN
	<* ASSERT my_depgraph # NIL *>
	<* ASSERT parse_root # NIL *>
	<* ASSERT depgraph_pms # NIL *>
	(* TODO Should I assert to ensure there are no placeholders? *)
	tempchild := parse_root^.children ;
	tempdepgraph := my_depgraph ;
	WHILE tempchild # NIL DO
		(* Skip separators, which have the value of separator and are terminals. *)
		IF Text.Equal( tempchild^.cur^.val , depgraph_pms^.separator ) OR tempchild^.cur^.cat = Node.Category.NonTerminal THEN
			(* Parse root *)
			Node.DeepCopy( tempdepgraph^.parse_root , tempchild^.cur ) ;
			(* Deps *)
			tempdepgraph^.deps := NIL ; (* For now... *)
			(* Subdepgraph *)
			GetNextLevelOfStartSymbols( tempstartsymbs , tempdepgraph^.parse_root , depgraph_pms ) ;
			tempdepgraph^.subdepgraph := NEW( REF T , Node.Length( tempstartsymbs ) ) ;
			FOR sdg_index := FIRST( tempdepgraph^.subdepgraph^ ) TO LAST( tempdepgraph^.subdepgraph^ ) DO
				Node.DeepCopy( templowerleveltree , tempstartsymbs^.cur ) ;
				GetDepGraphFromStartSymbol( tempdepgraph^.subdepgraph[ sdg_index ] , templowerleveltree , depgraph_pms ) ;
				tempstartsymbs^.cur^.children := NEW( REF Node.DList ) ;
				tempstartsymbs^.cur^.cat := Node.Category.Placeholder ;
				tempstartsymbs := tempstartsymbs^.next ;
			END ;
			(* Next *)
			IF tempchild # NIL THEN
				tempdepgraph^.next := NEW( REF T ) ;
				tempdepgraph := tempdepgraph^.next ;
			ELSE
				tempdepgraph^.next := NIL ;
			END ;
		END ;
		tempchild := tempchild^.next ;
	END ;
END GetDepGraphFromStartSymbol ;

(* ss_list is overwritten with the start symbols. The start symbols are references to the actual nodes
and NOT deep copies thereof. *)
(* Returns empty list, not NIL, if no start symbols in parse tree with parse_root as its root. *)
PROCEDURE GetNextLevelOfStartSymbols( ss_list : REF Node.DList ; parse_root : REF Node.T ; depgraph_pms : REF DepGraphParams ) =
VAR
	tempchild : REF Node.DList := NIL ;
	templist : REF Node.DList := NIL ;
BEGIN
	<* ASSERT ss_list # NIL *>
	<* ASSERT parse_root # NIL *>
	<* ASSERT depgraph_pms # NIL *>
	IF parse_root^.val = depgraph_pms^.start_symbol_val THEN
		ss_list^.cur := parse_root ;
		ss_list^.next := NIL ;
		ss_list^.prev := NIL ;
	ELSE
		Node.DefaultDList( ss_list ) ;
		tempchild := ss_list^.cur^.children ;
		WHILE tempchild # NIL DO
			(* Write to temp list *)
			templist := NEW( REF Node.DList ) ;
			GetNextLevelOfStartSymbols( templist , tempchild^.cur , depgraph_pms ) ;
			(* Append list to ss_list *)
			Node.AppendDList( ss_list , templist ) ;
		END ;
	END ;
END GetNextLevelOfStartSymbols ;

BEGIN END DepGraph .
