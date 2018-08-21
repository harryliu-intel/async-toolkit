MODULE Spec;

(***********)
(* Imports *)
(***********)
IMPORT Node ;
IMPORT FileWr ;
IMPORT FileRd ;
IMPORT Rd , Wr ;
IMPORT OSError ;
IMPORT Pathname ;
IMPORT Thread ;
IMPORT Text ;
IMPORT TextList ;
IMPORT StyleRulesTbl ;
IMPORT NextCharTbl ;
IMPORT IO ;
IMPORT Fmt ;

(**********************)
(* Visible Procedures *)
(**********************)

(* PROCEDURE Parse( in_fname : Pathname.T ) : REF Node.T RAISES { InvalidFname } = *)
(* VAR *)
(* 	rd_handle : Rd.T ; *)
(* 	lexer := NEW( LexerExt.T ) ; *)
(* 	parser := NEW( ParserExt.T ) ; *)
(* 	root : REF Node.T := NIL ; *)
(* BEGIN *)
 	(* Take file name *)
(* 	TRY *)
(* 		rd_handle := FileRd.Open( in_fname ) ; *)
(* 	EXCEPT *)
(* 		| OSError.E( ErrCode ) => *)
(* 			EVAL ErrCode ; *)
(* 			RAISE InvalidFname ; *)
(* 	END ; *)
 	(* Parse it *)
(* 	EVAL lexer.setRd( rd_handle ) ; *)
(* 	EVAL parser.setLex( lexer ).parse( ) ; *)
(* 	root := ParserExt.GetParseTree( ) ; *)
(* 	RETURN root ; *)
(* END Parse ; *)

(* Deep copy of procedure defn. Specialize the copy and then append it
just after the original defn. This way, it will work for languages like
Python and C in which functions are defined in order, which lets you
call the original function and other functions from the residual function in the inlined
code. It is the user's responsibility to order their residual functions properly
to ensure that they call one from the other. *)
PROCEDURE Specialize( root : REF Node.T ; spec_pms : REF SpecParams ; ptree_pms : REF PTreeParams ) =
VAR
	procdef : REF Node.T := NIL ;
	spec_procdef := NEW( REF Node.T ) ;
	my_static_args : TextList.T := NIL ;
BEGIN
	<* ASSERT root # NIL *>
	<* ASSERT spec_pms # NIL *>
	<* ASSERT spec_pms^.specblock # NIL *>
	<* ASSERT spec_pms^.static_args # NIL *>
	<* ASSERT NOT Text.Equal( spec_pms^.procname , "" ) *>
	<* ASSERT ptree_pms # NIL *>
	(* TODO You should be asserting that more ptree_pms are not nil. *)
	procdef := GetNthProcDef( root , ptree_pms , spec_pms^.procname , spec_pms^.procdefnumber ) ;
	<* ASSERT procdef # NIL *>
	Node.DeepCopy( spec_procdef , procdef ) ;
	my_static_args := spec_pms^.static_args ;
	WHILE my_static_args # NIL DO
		DeleteArgWName( spec_procdef , ptree_pms , my_static_args.head ) ;
		my_static_args := my_static_args.tail ;
	END ;
	PrependCodeToProcBlock( spec_procdef , spec_pms^.specblock , ptree_pms ) ;
	AddNewProcedure( root , spec_procdef , ptree_pms ) ;
END Specialize ;

PROCEDURE GenCode( root : REF Node.T ; style_rules_array : StyleRulesTbl.Default ; out_fname : Pathname.T ) RAISES { InvalidFname , OutError } =
VAR
	out_file_handle : Wr.T := NIL ;
	term_list : TextList.T := NIL ;
BEGIN
	<* ASSERT root # NIL *>
	<* ASSERT style_rules_array # NIL *>
	TRY
		EVAL FileRd.Open( out_fname ) ;
		RAISE InvalidFname ;
	EXCEPT
		(* ...open output stream... *)
		| OSError.E =>
			TRY
				out_file_handle := FileWr.Open( out_fname ) ;
			EXCEPT
				| OSError.E( ErrCode ) =>
					EVAL ErrCode ; (* To silence warnings *)
					RAISE InvalidFname ;
			END ;
	END ;
	TRY
		term_list := GetTokenList( root , style_rules_array ) ;
		<* ASSERT term_list # NIL *>
		WHILE term_list # NIL DO
			Wr.PutText( out_file_handle , term_list.head ) ;
			term_list := term_list.tail ;
		END ;
	EXCEPT
		| Wr.Failure( ErrCode ) => EVAL ErrCode ; RAISE OutError ;
		| Thread.Alerted => RAISE OutError ;
	END ;
	TRY
		Wr.Close( out_file_handle ) ;
	EXCEPT
		| Wr.Failure( ErrCode ) =>
			EVAL ErrCode ; (* To silence warnings *)
			RAISE OutError ;
		| Thread.Alerted => RAISE OutError ;
	END ;
END GenCode ;

PROCEDURE DebugTree( root : REF Node.T ; out_fname : Pathname.T ) RAISES { InvalidFname , OutError } =
VAR
	out_file_handle : Wr.T ;
BEGIN
	(* If file does not exist... *)
	TRY
		EVAL FileRd.Open( out_fname ) ;
		RAISE InvalidFname ;
	EXCEPT
		(* ...open output stream... *)
		| OSError.E =>
			TRY
				out_file_handle := FileWr.Open( out_fname ) ;
			EXCEPT
				| OSError.E( ErrCode ) =>
					EVAL ErrCode ; (* To silence warnings *)
					RAISE OutError ;
			END ;
	END ;
	(* ...and print the tree structure. *)
	TRY
		IndentedTreePrint( root , 0 , out_file_handle ) ;
	EXCEPT
		| Wr.Failure( ErrCode ) =>
			EVAL ErrCode ; (* To silence warnings *)
			RAISE OutError ;
		| Thread.Alerted => RAISE OutError ;
	END ;
	TRY
		Wr.Close( out_file_handle ) ;
	EXCEPT
		| Wr.Failure( ErrCode ) =>
			EVAL ErrCode ; (* To silence warnings *)
			RAISE OutError ;
		| Thread.Alerted => RAISE OutError ;
	END ;
END DebugTree ;

(*********************)
(* Hidden Procedures *)
(*********************)

(* TODO Organize and document these *)

PROCEDURE AddNewProcedure( root : REF Node.T ; newprocdef : REF Node.T ; ptree_pms : REF PTreeParams ) =
VAR
	all_proc_defs_raw := NEW( REF Node.DList ) ;
	last_proc_def : REF Node.T := NIL ;
	parent_of_last_proc_def : REF Node.T := NIL ;
	temp_child : REF Node.DList := NIL ;
	nextchild : REF Node.DList := NIL ;
BEGIN
	<* ASSERT root # NIL *>
	<* ASSERT newprocdef # NIL *>
	<* ASSERT newprocdef^.val = ptree_pms^.ProcedureDefnVal *>
	(* Get all the procedure defs *)
	Node.FindAllNonterms( all_proc_defs_raw , root , ptree_pms^.ProcedureDefnVal ) ;
	(* Find the last one *)
	last_proc_def := Node.GoToEnd( all_proc_defs_raw )^.cur ;
	(* Get their parent *)
	TRY
		parent_of_last_proc_def := Node.GetParent( root , last_proc_def ) ;
		<* ASSERT parent_of_last_proc_def # NIL *>
	EXCEPT
		| Node.NoMatchException => <* ASSERT FALSE *>
	END ;
	(* Add him in the children's list afterward *)
	temp_child := parent_of_last_proc_def^.children ;
	WHILE temp_child # NIL DO
		IF temp_child^.cur = last_proc_def THEN
			nextchild := temp_child^.next ;
			temp_child^.next := NEW( REF Node.DList ) ;
			temp_child^.next^.cur := newprocdef ;
			temp_child^.next^.prev := temp_child ;
			temp_child^.next^.next := nextchild ;
		END ;
		temp_child := temp_child^.next ;
	END ;
END AddNewProcedure ;

PROCEDURE GetTokenList( root : REF Node.T ; srules_tbl : StyleRulesTbl.Default ) : TextList.T =
VAR
	tlist_to_return : TextList.T := NIL ;
	child_len : CARDINAL := 0 ;
	index : CARDINAL := 0 ;
	temp_child : REF Node.DList := NIL ;
	endchar : TEXT := "" ;
	mynextchartbl : NextCharTbl.T ;
BEGIN
	(* TODO More assertions? *)
	<* ASSERT root # NIL *>
	IO.Put( "Current node: " & root^.val & "\n" ) ;
	IF root^.cat # Node.Category.NonTerminal THEN
		RETURN TextList.Cons( root^.val , NIL ) ;
	ELSE
		tlist_to_return := NIL ;
		child_len := Node.Length( root^.children ) ;
		index := 0 ;
		temp_child := Node.GoToBeginning( root^.children ) ;
		WHILE temp_child # NIL DO
			endchar := "" ;
			IF NOT srules_tbl.get( root^.val , mynextchartbl ) OR NOT mynextchartbl.get( Fmt.Int( index ) , endchar ) THEN
				IF index # child_len - 1 THEN
					endchar := " " ;
				END ;
			END ;
			IF tlist_to_return = NIL THEN
				tlist_to_return := GetTokenList( temp_child^.cur , srules_tbl ) ;
			ELSE
				tlist_to_return := TextList.Append( tlist_to_return , GetTokenList( temp_child^.cur , srules_tbl ) ) ;
			END ;
			tlist_to_return := TextList.Append( tlist_to_return , TextList.Cons( endchar , NIL ) ) ;
			temp_child := temp_child^.next ;
			INC( index ) ;
		END ;
		RETURN tlist_to_return ;
	END ;
END GetTokenList ;

PROCEDURE DebugTextList( tlist : TextList.T ) =
BEGIN
	IF tlist = NIL THEN
		IO.Put( "( Empty )\n" ) ;
	ELSE
		WHILE tlist.tail # NIL DO
			IO.Put( tlist.head & " -> " ) ;
			tlist := tlist.tail ;
		END ;
		IO.Put( tlist.head & "\n" ) ;
	END ;
END DebugTextList ;

(* Follow a path and return all the nodes at the end of it *)
(* Assumptions:
- path is nonempty and not NIL
- root node is a nonterminal and is the implicit starting point of the path
*)
PROCEDURE FollowPath( list : REF Node.DList ; root : REF Node.T ; path : TextList.T ) =
VAR
	return_end_of_path := NEW( REF Node.DList ) ;
	recursive_return_end_of_path := NEW( REF Node.DList ) ;
	recursive_templist := NEW( REF Node.DList ) ;
	current_child : REF Node.DList := NIL ;
BEGIN
	<* ASSERT TextList.Length( path ) > 0 *>
	<* ASSERT list # NIL *>
	<* ASSERT root # NIL *>
	<* ASSERT root^.cat = Node.Category.NonTerminal *>
	Node.DefaultDList( return_end_of_path ) ;
	Node.DefaultDList( recursive_return_end_of_path ) ;
	Node.DefaultDList( recursive_templist ) ;
	Node.DefaultDList( list ) ;
	current_child := Node.GoToBeginning( root^.children ) ;
	LOOP
		IF current_child^.cur^.val = path.head THEN
			Node.AppendNode( return_end_of_path , current_child^.cur ) ;
		END ;
		IF current_child^.next = NIL THEN
			EXIT ;
		ELSE
			current_child := current_child^.next ;
		END ;
	END ;
	IF TextList.Length( path ) > 1 THEN
		path := path.tail ;
		current_child := Node.GoToBeginning( return_end_of_path ) ;
		LOOP
			FollowPath( recursive_templist , current_child^.cur , path ) ;
			Node.AppendDList( recursive_return_end_of_path , recursive_templist ) ;
			IF current_child^.next = NIL THEN
				EXIT ;
			ELSE
				current_child := current_child^.next ;
			END ;
		END ;
		recursive_return_end_of_path := Node.GoToBeginning( recursive_return_end_of_path ) ;
		list^.cur := recursive_return_end_of_path^.cur ;
		list^.prev := recursive_return_end_of_path^.prev ;
		list^.next := recursive_return_end_of_path^.next ;
	ELSE
		return_end_of_path := Node.GoToBeginning( return_end_of_path ) ;
		list^.cur := return_end_of_path^.cur ;
		list^.prev := return_end_of_path^.prev ;
		list^.next := return_end_of_path^.next ;
	END ;
END FollowPath ;

(* Get args list from procedure definition *)
(* Return array should only have 1 element, the Formals nonterminal, but
it is theoretically possible to have multiple elements. *)
PROCEDURE GetArgsList( list : REF Node.DList ; root : REF Node.T ; ptree_pms : REF PTreeParams ) =
BEGIN
	<* ASSERT root # NIL *>
	<* ASSERT root^.val = ptree_pms^.ProcedureDefnVal *>
	<* ASSERT root^.cat = Node.Category.NonTerminal *>
	<* ASSERT ptree_pms # NIL *>
	<* ASSERT ptree_pms^.PathToArgList # NIL *>
	<* ASSERT TextList.Length( ptree_pms^.PathToArgList ) > 0 *>
	<* ASSERT list # NIL *>
	Node.DefaultDList( list ) ;
	FollowPath( list , root , ptree_pms^.PathToArgList ) ;
	(* TOOD What if user makes a grammar error and has
	two separate argument lists for same procedure? *)
	<* ASSERT Node.Length( list ) = 1 *>
END GetArgsList ;

(* Get procedure name from procedure definition *)
PROCEDURE GetProcName( root : REF Node.T ; ptree_pms : REF PTreeParams ) : TEXT =
VAR
	list := NEW( REF Node.DList ) ;
BEGIN
	<* ASSERT root # NIL *>
	<* ASSERT root^.cat = Node.Category.NonTerminal *>
	<* ASSERT root^.val = ptree_pms^.ProcedureDefnVal *>
	<* ASSERT ptree_pms^.PathToProcedureName # NIL *>
	<* ASSERT TextList.Length( ptree_pms^.PathToProcedureName ) > 0 *>
	FollowPath( list , root , ptree_pms^.PathToProcedureName ) ;
	(* TODO Again, probably want proper error handling *)
	<* ASSERT Node.Length( list ) = 1 *>
	<* ASSERT Node.Length( list^.cur^.children ) = 1 *>
	<* ASSERT list^.cur^.children^.cur^.cat = Node.Category.Identifier *>
	RETURN list^.cur^.children^.cur^.val ;
END GetProcName ;

(* Get nth proc def with name *)
(* Return NIL if we can't find it *)
PROCEDURE GetNthProcDef( root : REF Node.T ; ptree_pms : REF PTreeParams ; ProcName : TEXT ; N : CARDINAL ) : REF Node.T =
VAR
	all_proc_defs_raw := NEW( REF Node.DList ) ;
	all_proc_defs : REF Node.DList := NIL ;
	proc_match_ctr : CARDINAL := 0 ;
BEGIN
	<* ASSERT root # NIL *>
	<* ASSERT ptree_pms # NIL *>
	(* Find all procedure defs *)
	Node.FindAllNonterms( all_proc_defs_raw , root , ptree_pms^.ProcedureDefnVal ) ;
	(* Look through each one. If name matches, increment counter. *)
	(* When right counter value hit, return node. *)
	all_proc_defs := Node.GoToBeginning( all_proc_defs_raw ) ;
	LOOP
		IF GetProcName( all_proc_defs^.cur , ptree_pms ) = ProcName THEN
			IF proc_match_ctr = N THEN
				RETURN all_proc_defs^.cur ;
			END ;
			INC( proc_match_ctr ) ;
		END ;
		IF all_proc_defs^.next = NIL THEN
			EXIT ;
		ELSE
			all_proc_defs := all_proc_defs^.next ;
		END ;
	END ;
	(* If Nth hit not reached, return NIL. *)
	RETURN NIL ;
END GetNthProcDef ;

PROCEDURE DeleteArgWName( root : REF Node.T ; ptree_pms : REF PTreeParams ; argname : TEXT ) =
VAR
	argslist := NEW( REF Node.DList ) ;
	child := NEW( REF Node.DList ) ;
	childname := NEW( REF Node.DList ) ;
	nextchild := NEW( REF Node.DList ) ;
	adjusted_path : TextList.T := NIL ;
	level_above_arglist : REF Node.DList := NIL ;
	children_at_arglist_level : REF Node.DList := NIL ;
	nextchild_below_arglist : REF Node.DList := NIL ;
BEGIN
	<* ASSERT root # NIL *>
	<* ASSERT ptree_pms # NIL *>
	<* ASSERT NOT Text.Equal( argname , "" ) *>
	GetArgsList( argslist , root , ptree_pms ) ;
	<* ASSERT Node.Length( argslist ) <= 1 *>
	(* TODO What if language allows separators but not args? *)
	IF Node.Length( argslist ) = 1 THEN
		child := argslist^.cur^.children ;
		<* ASSERT child # NIL *>
		WHILE child # NIL DO
			FollowPath( childname , child^.cur , ptree_pms^.PathToArgNameFromArg ) ;
			IF childname^.cur^.children^.cur^.val = argname THEN
				nextchild := child^.next ;
				Node.DeleteFromList( child ) ;
				child := nextchild ;
				IF child # NIL AND child^.cur^.val = ptree_pms^.ArgSeparator THEN
					nextchild := child^.next ;
					Node.DeleteFromList( child ) ;
					child := nextchild ;
				END ;
			ELSE
				child := child^.next ;
			END ;
		END ;
		(* If no children, delete the argslist from the tree *)
		IF Node.Length( argslist^.cur^.children ) = 0 THEN
			(* Cut off the last element from the path.
			We're going one level above the arglist. *)
			adjusted_path := CutOffLastListElement( ptree_pms^.PathToArgList ) ;
			<* ASSERT adjusted_path # NIL *>
			level_above_arglist := NEW( REF Node.DList ) ;
			FollowPath( level_above_arglist , root , adjusted_path ) ;
			(* Delete any references to THAT arglist *)
			<* ASSERT Node.Length( level_above_arglist ) > 0 *>
			WHILE level_above_arglist # NIL DO
				<* ASSERT level_above_arglist^.cur # NIL *>
				children_at_arglist_level := level_above_arglist^.cur^.children ;
				WHILE children_at_arglist_level # NIL DO
					<* ASSERT children_at_arglist_level # NIL *>
					IF children_at_arglist_level^.cur = argslist^.cur THEN
						nextchild_below_arglist := children_at_arglist_level^.next ;
						Node.DeleteFromList( children_at_arglist_level ) ;
						children_at_arglist_level := nextchild_below_arglist ;
					ELSE
						children_at_arglist_level := children_at_arglist_level^.next ;
					END ;
				END ;
				level_above_arglist := level_above_arglist^.next ;
			END ;
		END ;
	END ;
END DeleteArgWName ;

PROCEDURE CutOffLastListElement( list : TextList.T ) : TextList.T =
VAR
	templist : TextList.T := NIL ;
BEGIN
	IF TextList.Length( list ) = 0 THEN
		RETURN list ;
	ELSIF TextList.Length( list ) = 1 THEN
		RETURN list.tail ;
	ELSE
		templist := list ;
		WHILE templist.tail.tail # NIL DO
			templist := templist.tail ;
		END ;
		templist.tail := NIL ;
		RETURN list ;
	END ;
END CutOffLastListElement ;

PROCEDURE PrependCodeToProcBlock( root : REF Node.T ; CodeToPrepend : REF Node.T ; ptree_pms : REF PTreeParams ) =
VAR
	blocklist := NEW( REF Node.DList ) ;
BEGIN
	<* ASSERT root # NIL *>
	<* ASSERT root^.val = ptree_pms^.ProcedureDefnVal *>
	<* ASSERT root^.cat = Node.Category.NonTerminal *>
	<* ASSERT CodeToPrepend # NIL *>
	<* ASSERT ptree_pms # NIL *>
	FollowPath( blocklist , root , ptree_pms^.PathToProcedureBlock ) ;
	Node.PrependNode( blocklist^.cur^.children , CodeToPrepend ) ;
END PrependCodeToProcBlock ;

(* IndentedTreePrint
- root :: reference to the starting node of the parse tree
- num_indents :: number of indents (defined as two spaces) preceding the root node
- write_stream :: Wr.T stream to which the tree is printed
It is the user's response to close write_stream. It is not closed by IndentedTreePrint.

example:
If I have the following tree...

                       A
                      /|\
                     B C D
                    / \
                   E   F

...and num_indents is 1, IndentedTreePrint will print...

  A
    B
      E
      F
    C
    D

...to write_stream.

*)
(* Format inspired by https://stackoverflow.com/questions/1649027/how-do-i-print-out-a-tree-structure *)
PROCEDURE IndentedTreePrint( root : REF Node.T ; num_indents : INTEGER ; write_stream : Wr.T ) RAISES { Wr.Failure , Thread.Alerted } =
VAR
	node_cat : TEXT := "" ;
	current_child := NEW( REF Node.DList ) ;
BEGIN
	IF root # NIL THEN
		(* Print initial indentation *)
		FOR indent_count := 1 TO num_indents DO
			TRY
				Wr.PutText( write_stream , "  " ) ;
			EXCEPT
				| Wr.Failure( ErrCode ) => RAISE Wr.Failure( ErrCode ) ;
				| Thread.Alerted => RAISE Thread.Alerted ;
			END ;
		END ;
		(* Print node's value and category *)
		IF root^.cat = Node.Category.NonTerminal THEN
			node_cat := "NonTerminal" ;
		ELSIF root^.cat = Node.Category.Constant THEN
			node_cat := "Constant" ;
		ELSIF root^.cat = Node.Category.Identifier THEN
			node_cat := "Identifier" ;
		ELSIF root^.cat = Node.Category.NoCategory THEN
			node_cat := "NoCategory" ;
		(* TODO Do I need an else? *)
		END ;
		TRY
			Wr.PutText( write_stream , root^.val & " ( " & node_cat & " )\n" ) ;
		EXCEPT
			| Wr.Failure( ErrCode ) => RAISE Wr.Failure( ErrCode ) ;
			| Thread.Alerted => RAISE Thread.Alerted ;
		END ;
		(* Do this recursively for all of its children *)
		current_child := root^.children ;
		<* ASSERT current_child # NIL *>
		IF NOT Node.IsEmpty( root^.children ) THEN
			current_child := Node.GoToBeginning( root^.children ) ;
			LOOP
				TRY
					IndentedTreePrint( current_child^.cur , num_indents + 1 , write_stream ) ;
				EXCEPT
					| Wr.Failure( ErrCode ) => RAISE Wr.Failure( ErrCode ) ;
					| Thread.Alerted => RAISE Thread.Alerted ;
				END ;
				IF current_child^.next = NIL THEN
					EXIT ;
				ELSE
					current_child := current_child^.next ;
				END ;
			END ;
		END ;
	END ;
END IndentedTreePrint ;

BEGIN END Spec.
