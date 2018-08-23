MODULE DepGraph ;

(*************)
(** Imports **)
(*************)

IMPORT Node ;
IMPORT IO ;
IMPORT Fmt ;
IMPORT Text ;
IMPORT REFANYList ;

IMPORT Spec ;

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
	IO.Put( "root length is " & Fmt.Int( Length( root ) ) & "\n" ) ;
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
	tempchild : REF Node.DList := NIL ;
	tempdepgraph : REF T := NIL ;
	tempstartsymbs : REF Node.DList := NIL ;
	templowerleveltree : REF Node.T := NIL ;
	first_subdepgraph : REF REFANYList.T := NIL ;
BEGIN
	<* ASSERT my_depgraph # NIL *>
	<* ASSERT parse_root # NIL *>
	<* ASSERT parse_root^.children # NIL *>
	<* ASSERT depgraph_pms # NIL *>
	<* ASSERT parse_root^.val = depgraph_pms^.start_symbol_val *>
	(* TODO Should I assert to ensure there are no placeholders? *)
	tempchild := parse_root^.children ;
	tempdepgraph := my_depgraph ;
	(* Reaching while loop via ->next being nil -> tempchild # NIL,
	Reaching while loop via recursion, would hit the IsEmpty scenario *)
	WHILE tempchild # NIL AND NOT Node.IsEmpty( tempchild ) DO
		(* Skip separators, which have the value of separator and are terminals. *)
		IF NOT( Text.Equal( tempchild^.cur^.val , depgraph_pms^.separator ) AND tempchild^.cur^.cat # Node.Category.NonTerminal ) THEN
			tempdepgraph^.subdepgraph := NIL ;
			IO.Put( "Initial Length of depgraph list (should be 0): " & Fmt.Int( REFANYList.Length( my_depgraph^.subdepgraph ) ) & "\n" ) ;
			IO.Put( "CONSIDERING STATEMENT FOR DEPGRAPH GENERATION: " & tempchild^.cur^.val & "\n" ) ;
			tempstartsymbs := NEW( REF Node.DList ) ;
			Node.DefaultDList( tempstartsymbs ) ;
			templowerleveltree := NEW( REF Node.T ) ;
			(* Parse root *)
			(* For now, parse_root is simply the tree whose root is at the current
			child *)
			tempdepgraph^.parse_root := NEW( REF Node.T ) ;
			Node.DeepCopy( tempdepgraph^.parse_root , tempchild^.cur ) ;
			(* Deps *)
			tempdepgraph^.deps := NIL ; (* For now... *)
			(* Subdepgraph *)
			(* Get the next level of start symbols... *)
			GetNextLevelOfStartSymbols( tempstartsymbs , tempdepgraph^.parse_root , depgraph_pms ) ;
			(* Again, NIL case must be considered if you reach it via .next,
			IsEmpty must be considered if GetNextLevelOfStartSymbols returns an empty
			start symbol list *)
			WHILE tempstartsymbs # NIL AND NOT Node.IsEmpty( tempstartsymbs ) DO
				(* For each start symbol, create a subdependency graph *)
				Node.DeepCopy( templowerleveltree , tempstartsymbs^.cur ) ;
				IF first_subdepgraph = NIL THEN
					IO.Put( "Starting the subdepgraph list!\n" ) ;
					first_subdepgraph := NEW( REF REFANYList.T ) ;
					first_subdepgraph^ := REFANYList.Cons( NEW( REF T ) , NIL ) ;
					tempdepgraph^.subdepgraph := first_subdepgraph^ ;
					IO.Put( "Length of depgraph list (should now be 1): " & Fmt.Int( REFANYList.Length( tempdepgraph^.subdepgraph ) ) & "\n" ) ;
				ELSIF REFANYList.Length( first_subdepgraph^ ) = 1 THEN
					IO.Put( "Appending to the subdepgraph list. If you only see this and no starting message, it means you're going to get an empty element.\n" ) ;
					first_subdepgraph^ := REFANYList.Append( first_subdepgraph^ , REFANYList.Cons( NEW( REF T ) , NIL ) ) ;
					tempdepgraph^.subdepgraph := first_subdepgraph^ ;
					IO.Put( "Length of depgraph list (should now be 2): " & Fmt.Int( REFANYList.Length( tempdepgraph^.subdepgraph ) ) & "\n" ) ;
					tempdepgraph^.subdepgraph := tempdepgraph^.subdepgraph.tail ;
				ELSE
					IO.Put( "Appending to the subdepgraph list. If you only see this and no starting message, it means you're going to get an empty element.\n" ) ;
					tempdepgraph^.subdepgraph := REFANYList.Append( tempdepgraph^.subdepgraph , REFANYList.Cons( NEW( REF T ) , NIL ) ) ;
					tempdepgraph^.subdepgraph := tempdepgraph^.subdepgraph.tail ;
				END ;
				GetDepGraph( tempdepgraph^.subdepgraph.head , templowerleveltree , depgraph_pms ) ;
				(* Be sure to replace the original tree's start symbols with placeholders. *)
				tempstartsymbs^.cur^.children := NEW( REF Node.DList ) ;
				tempstartsymbs^.cur^.cat := Node.Category.Placeholder ;
				tempstartsymbs := tempstartsymbs^.next ;
			END ;
			IF first_subdepgraph # NIL THEN
				tempdepgraph^.subdepgraph := first_subdepgraph^ ;
			END ;
			IO.Put( "Ending Length of depgraph list (should be 2): " & Fmt.Int( REFANYList.Length( tempdepgraph^.subdepgraph ) ) & "\n" ) ;
			(* Next *)
			IF tempchild^.next # NIL AND NOT( Text.Equal( tempchild^.next^.cur^.val , depgraph_pms^.separator ) AND tempchild^.next^.cur^.cat # Node.Category.NonTerminal ) THEN
				(* If next is not nil nor is a separator *)
				tempdepgraph^.next := NEW( REF T ) ;
				tempdepgraph := tempdepgraph^.next ;
			ELSE
				tempdepgraph^.next := NIL ;
			END ;
		END ;
		tempchild := tempchild^.next ;
	END ;
END GetDepGraph ;

PROCEDURE PutPlaceholderInProcBlock( start : REF Node.T ; root : REF Node.T ; depgraph_pms : REF DepGraphParams ) =
VAR
	start_sym : REF Node.DList := NIL ;
BEGIN
	<* ASSERT start # NIL *>
	<* ASSERT root # NIL *>
	<* ASSERT depgraph_pms # NIL *>
	start_sym := NEW( REF Node.DList ) ;
	Node.FollowPath( start_sym , root , depgraph_pms^.ProcedureBodyToSSPath ) ;
	Node.DeepCopy( start , start_sym^.cur ) ;
	start_sym^.cur^.cat := Node.Category.Placeholder ;
	Node.DefaultDList( start_sym^.cur^.children ) ;
	<* ASSERT Node.IsEmpty( start_sym^.cur^.children ) *> (* Just a sanity check *)
END PutPlaceholderInProcBlock ;

PROCEDURE IsEmpty( root : REF T ) : BOOLEAN =
VAR
	mydefault : REF T := NIL ;
BEGIN
	<* ASSERT root # NIL *>
	mydefault := NEW( REF T ) ;
	DefaultDepGraph( mydefault ) ;
	IF root^.next = mydefault^.next AND root^.deps = mydefault^.deps AND root^.parse_root = mydefault^.parse_root AND root^.subdepgraph = mydefault^.subdepgraph THEN
		RETURN TRUE ;
	ELSE
		RETURN FALSE ;
	END ;
END IsEmpty ;

PROCEDURE Length( root : REF T ) : CARDINAL =
VAR
	cnt : CARDINAL := 0 ;
BEGIN
	IF IsEmpty( root ) THEN
		RETURN 0 ;
	ELSE
		WHILE root # NIL DO
			INC( cnt ) ;
			root := root^.next ;
		END ;
		RETURN cnt ;
	END ;
END Length ;

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
	tempsubdepgraph : REFANYList.T := NIL ; 
BEGIN
	<* ASSERT root # NIL *>
	<* ASSERT root^.parse_root # NIL *>
	<* ASSERT parse_root # NIL *>
	IO.Put( "Head of tree: " & root^.parse_root^.val & "\n" ) ;
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
		Node.DebugList( placeholder_list ) ;
		IO.Put( "Top of tree: " & parse_root^.cur^.val & "\n" ) ;
		IO.Put( "Placeholder list: " & Fmt.Int( Node.Length( placeholder_list ) ) & "\n" ) ;
		IO.Put( "Subdepgraph list: " & Fmt.Int( REFANYList.Length( root^.subdepgraph ) ) & "\n" ) ;
		<* ASSERT Node.Length( placeholder_list ) = REFANYList.Length( root^.subdepgraph ) *>
		tempsubdepgraph := root^.subdepgraph ;
		WHILE tempsubdepgraph # NIL DO
			ConstructParseTreeRootList( placeholder_list^.cur^.children , tempsubdepgraph.head ) ;
			placeholder_list^.cur^.cat := Node.Category.NonTerminal ;
			tempsubdepgraph := tempsubdepgraph.tail ;
		END ;
	END ;
END ConstructParseTreeRootList ;

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
		tempchild := parse_root^.children ;
		WHILE tempchild # NIL AND NOT Node.IsEmpty( tempchild ) DO
			(* Write to temp list *)
			templist := NEW( REF Node.DList ) ;
			GetNextLevelOfStartSymbols( templist , tempchild^.cur , depgraph_pms ) ;
			(* Append list to ss_list *)
			Node.AppendDList( ss_list , templist ) ;
			tempchild := tempchild^.next ;
		END ;
	END ;
END GetNextLevelOfStartSymbols ;

BEGIN END DepGraph .
