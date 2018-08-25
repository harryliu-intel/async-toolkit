MODULE DepGraph ;

(*************)
(** Imports **)
(*************)

IMPORT Node ;
IMPORT IO ;
IMPORT Fmt ;
IMPORT Text ;
IMPORT REFANYList ;
IMPORT TextList ;

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
	IO.Put( "--- CONSTRUCTING PARSE TREE ---\n" ) ;
	IO.Put( "parse_root val: " & parse_root^.val & "\n" ) ;
	IO.Put( "Starting to look for all placeholders...\n" ) ;
	Node.FindAllNodesWithCategoryDeep( placeholder_list , parse_root , Node.Category.Placeholder ) ;
	IO.Put( "Got the placeholders...\n" ) ;
	IO.Put( "Number of placeholders (I expect 1 for the top level): " & Fmt.Int( Node.Length( placeholder_list ) ) & "\n" ) ;
	<* ASSERT Node.Length( placeholder_list ) = 1 *>
	placeholder_list^.cur^.cat := Node.Category.NonTerminal ;
	IO.Put( "Number of children for the placeholder before (we expect 0): " & Fmt.Int( Node.Length( placeholder_list^.cur^.children ) ) & "\n" ) ;
	ConstructParseTreeRootList( placeholder_list^.cur^.children , root ) ;
	IO.Put( "Number of children for the placeholder after (we expect 2): " & Fmt.Int( Node.Length( placeholder_list^.cur^.children ) ) & "\n" ) ;
	IO.Put( "First child (I expect Rule8): " & placeholder_list^.cur^.children^.cur^.val & "\n" ) ;
	IO.Put( "Second child (I expect Rule13): " & placeholder_list^.cur^.children^.next^.cur^.val & "\n" ) ;
	<* ASSERT placeholder_list^.cur^.children # NIL *>
	(* Add separators *)
	children_list := Node.GoToBeginning( placeholder_list^.cur^.children ) ;
	WHILE children_list # NIL DO
		tempnext := children_list^.next ;
		stmt_sep_node := NEW( REF Node.T , val := depgraph_pms^.separator , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ;
		children_list^.next := NEW( REF Node.DList , cur := stmt_sep_node , next := tempnext , prev := children_list ) ;
		IF tempnext # NIL THEN
			tempnext^.prev := children_list^.next ; 
		END ;
		children_list := tempnext ;
	END ;
	IO.Put( "Children of placeholder: " ) ;
	Node.DebugList( placeholder_list^.cur^.children ) ;
	IO.Put( "Number of children for the placeholder after separators added (we expect 4): " & Fmt.Int( Node.Length( Node.GoToBeginning( placeholder_list^.cur^.children ) ) ) & "\n" ) ;
	IO.Put( "--- CONSTRUCTING PARSE TREE ---\n" ) ;
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
		IO.Put( "-------------------------\n" ) ;
		IF NOT( Text.Equal( tempchild^.cur^.val , depgraph_pms^.separator ) AND tempchild^.cur^.cat # Node.Category.NonTerminal ) THEN
			IO.Put( "Getting dep graph for a non-separator: " & tempchild^.cur^.val & "\n" ) ;
			tempdepgraph^.subdepgraph := NIL ;
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
			IO.Put( "Number of start symbols: " & Fmt.Int( Node.Length( tempstartsymbs ) ) & "\n" ) ;
			(* Again, NIL case must be considered if you reach it via .next,
			IsEmpty must be considered if GetNextLevelOfStartSymbols returns an empty
			start symbol list *)
			WHILE tempstartsymbs # NIL AND NOT Node.IsEmpty( tempstartsymbs ) DO
				(* For each start symbol, create a subdependency graph *)
				Node.DeepCopy( templowerleveltree , tempstartsymbs^.cur ) ;
				IF first_subdepgraph = NIL THEN
					first_subdepgraph := NEW( REF REFANYList.T ) ;
					first_subdepgraph^ := REFANYList.Cons( NEW( REF T ) , NIL ) ;
					tempdepgraph^.subdepgraph := first_subdepgraph^ ;
				ELSIF REFANYList.Length( first_subdepgraph^ ) = 1 THEN
					first_subdepgraph^ := REFANYList.Append( first_subdepgraph^ , REFANYList.Cons( NEW( REF T ) , NIL ) ) ;
					tempdepgraph^.subdepgraph := first_subdepgraph^ ;
					tempdepgraph^.subdepgraph := tempdepgraph^.subdepgraph.tail ;
				ELSE
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
			tempchild := tempchild^.next ;
			(* Next *)
			IF tempchild # NIL (* AND NOT( Text.Equal( tempchild^.cur^.val , depgraph_pms^.separator ) AND tempchild^.cur^.cat # Node.Category.NonTerminal ) *) THEN
				WHILE tempchild # NIL DO
					IF NOT( Text.Equal( tempchild^.cur^.val , depgraph_pms^.separator ) AND tempchild^.cur^.cat # Node.Category.NonTerminal ) THEN
						(* Not a separator. Go to next. *)
						tempdepgraph^.next := NEW( REF T ) ;
						tempdepgraph := tempdepgraph^.next ;
						EXIT ;
					ELSE
						(* Separator skip *)
						tempchild := tempchild^.next ;
					END ;
				END ;
			ELSE
				tempdepgraph^.next := NIL ;
			END ;
		ELSE
			IO.Put( "Getting dep graph for a separator: " & tempchild^.cur^.val & "\n" ) ;
			tempchild := tempchild^.next ;
		END ;
		first_subdepgraph := NIL ;
		IO.Put( "-------------------------\n" ) ;
	END ;
	IO.Put( "Length of depgraph for this call: " & Fmt.Int( Length( my_depgraph ) ) & "\n" ) ;
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
	Node.DefaultDList( parse_root ) ;
	parse_root^.cur := NEW( REF Node.T ) ;
	IO.Put( " --- CONSTRUCTING PARSE TREE ROOT LIST ---\n" ) ;
	IO.Put( "Current root value: " & root^.parse_root^.val & "\n" ) ;
	IO.Put( "Deep copying root^.parse_root to parse_root...\n" ) ;
	Node.DeepCopy( parse_root^.cur , root^.parse_root ) ;
	IO.Put( "Successfully deep copied root^.parse_root to parse_root...\n" ) ;
	placeholder_list := NEW( REF Node.DList ) ;
	IO.Put( "Looking for all placeholders...\n" ) ;
	Node.FindAllNodesWithCategoryDeep( placeholder_list , parse_root^.cur , Node.Category.Placeholder ) ;
	IO.Put( "Found all placeholders...\n" ) ;
	IO.Put( "Number of placeholders: " & Fmt.Int( Node.Length( placeholder_list ) ) & "\n" ) ;

	(* If no subdepgraph, you already deep copied the tree. So, you're fine. *)
	(* If you have a subdepgraph, you gotta consider those too. *)
	IF root^.subdepgraph # NIL THEN
		Node.DebugList( placeholder_list ) ;
		<* ASSERT Node.Length( placeholder_list ) = REFANYList.Length( root^.subdepgraph ) *>
		tempsubdepgraph := root^.subdepgraph ;
		WHILE tempsubdepgraph # NIL DO
			ConstructParseTreeRootList( placeholder_list^.cur^.children , tempsubdepgraph.head ) ;
			placeholder_list^.cur^.cat := Node.Category.NonTerminal ;
			placeholder_list := placeholder_list^.next ;
			tempsubdepgraph := tempsubdepgraph.tail ;
		END ;
	END ;

	IF root^.next # NIL THEN
		parse_root^.next := NEW( REF Node.DList ) ;
		ConstructParseTreeRootList( parse_root^.next , root^.next ) ;
	END ;

	IO.Put( " --- CONSTRUCTING PARSE TREE ROOT LIST ---\n" ) ;
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

PROCEDURE DebugDepGraph( my_depgraph : REF T ; default_start : TEXT := "" ) =
VAR
	tempsdgptr : REFANYList.T := NIL ;
	tempdeps : REFANYList.T := NIL ;
	tempdeps_underlying_depgraph : REF T := NIL ;
	temp_assigned_var : TextList.T := NIL ;
BEGIN
	<* ASSERT my_depgraph # NIL *>
	<* ASSERT my_depgraph^.parse_root # NIL *>
	IF IsEmpty( my_depgraph ) THEN
		IO.Put( "( Empty )\n" ) ;
	ELSE
		(* For each element of depgraph, print the parse
		root head. Indent and do the same for its children. *)
		WHILE my_depgraph # NIL DO
			IO.Put( default_start & my_depgraph^.parse_root^.val ) ;
			IF my_depgraph^.is_static = TRUE THEN
				IO.Put( " ( Static ) " ) ;
			ELSE
				IO.Put( " ( Dynamic ) " ) ;
			END ;
			IF my_depgraph^.deps = NIL THEN
				IO.Put( " ( No dependencies ) " ) ;
			ELSE
				IO.Put( " ( Dependencies : " ) ;
				tempdeps := my_depgraph^.deps ;
				WHILE tempdeps.tail # NIL DO
					tempdeps_underlying_depgraph := tempdeps.head ;
					<* ASSERT tempdeps_underlying_depgraph^.parse_root # NIL *>
					<* ASSERT NOT IsEmpty( tempdeps_underlying_depgraph ) *>
					IO.Put( tempdeps_underlying_depgraph^.parse_root^.val & " , " ) ;
					tempdeps := tempdeps.tail ;
				END ;
				<* ASSERT NOT IsEmpty( tempdeps_underlying_depgraph ) *>
				IO.Put( tempdeps_underlying_depgraph^.parse_root^.val ) ;
				IO.Put( " ) " ) ;
			END ;
			IF my_depgraph^.assigned_vars = NIL THEN
				IO.Put( " ( No assigned vars ) " ) ;
			ELSE
				IO.Put( " ( Assigned vars : " ) ;
				temp_assigned_var := my_depgraph^.assigned_vars ;
				WHILE temp_assigned_var.tail # NIL DO
					IO.Put( temp_assigned_var.head & " , " ) ;
					temp_assigned_var := temp_assigned_var.tail ;
				END ;
				IO.Put( temp_assigned_var.head ) ;
				IO.Put( " ) " ) ;
			END ;
			IO.Put( "\n" ) ;
			IF my_depgraph^.subdepgraph # NIL THEN
				tempsdgptr := my_depgraph^.subdepgraph ;
				WHILE tempsdgptr # NIL DO
					DebugDepGraph( tempsdgptr.head , default_start & "  " ) ;
					tempsdgptr := tempsdgptr.tail ;
				END ;
			END ;
			my_depgraph := my_depgraph^.next ;
		END ;
	END ;
END DebugDepGraph ;

BEGIN END DepGraph .
