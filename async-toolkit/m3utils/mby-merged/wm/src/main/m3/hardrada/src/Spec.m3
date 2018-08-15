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

(*
PROCEDURE Specialize( root : REF Node.T ; ptree_pms : REF PTreeParams ) =
BEGIN
	
END Specialize ;
*)

(*
PROCEDURE GenCode( root : REF Node.T ; out_fname : Pathname.T ) RAISES { InvalidFname } =
BEGIN
END GenCode ;
*)

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

(* Follow a path and return all the nodes at the end of it *)
(* Assumptions:
- path is nonempty and not NIL
- root node is a nonterminal and is the implicit starting point of the path
*)
PROCEDURE FollowPath( root : REF Node.T ; path : REF ARRAY OF TEXT ) : REF ARRAY OF REF Node.T =
VAR
	return_end_of_path : REF ARRAY OF REF Node.T := NIL ;
	recursive_return_end_of_path : REF ARRAY OF REF Node.T := NIL ;
	adjusted_path : REF ARRAY OF TEXT := NIL ;
BEGIN
	<* ASSERT path # NIL *>
	<* ASSERT NUMBER( path^ ) # 0 *>
	<* ASSERT root^.cat = Node.Category.NonTerminal *>
	return_end_of_path := NEW( REF ARRAY OF REF Node.T , 0 ) ;
	FOR child_index := FIRST( root^.children^ ) TO LAST( root^.children^ ) DO
		IF root^.children[ child_index ]^.val = path[ FIRST( path^ ) ] THEN
			Node.AppendArr( return_end_of_path , root^.children[ child_index ] ) ;
		END ;
	END ;
	IF NUMBER( path^ ) > 1 THEN
		recursive_return_end_of_path := NEW( REF ARRAY OF REF Node.T , 0 ) ;
		adjusted_path := NEW( REF ARRAY OF TEXT , NUMBER( path^ ) - 1 ) ;
		FOR path_index := FIRST( path^ ) + 1 TO LAST( path^ ) DO
			adjusted_path[ ( path_index - ( FIRST( path^ ) + 1 ) ) + FIRST( adjusted_path^ ) ] := path[ path_index ] ;
		END ;
		FOR end_of_path_index := FIRST( return_end_of_path^ ) TO LAST( return_end_of_path^ ) DO
			Node.AppendArrToArr( recursive_return_end_of_path , FollowPath( return_end_of_path[ end_of_path_index ] , adjusted_path ) ) ;
		END ;
		RETURN recursive_return_end_of_path ;
	END ;
	RETURN return_end_of_path ;
END FollowPath ;

(* Get args list from procedure definition *)
(* Return array should only have 1 element, the Formals nonterminal, but
it is theoretically possible to have multiple elements. *)
PROCEDURE GetArgsList( root : REF Node.T ; ptree_pms : REF PTreeParams ) : REF ARRAY OF REF Node.T =
VAR
	argslist : REF ARRAY OF REF Node.T := NIL ;
BEGIN
	<* ASSERT root^.val = ptree_pms^.ProcedureDefnVal *>
	<* ASSERT root^.cat = Node.Category.NonTerminal *>
	<* ASSERT ptree_pms^.PathToArgList # NIL *>
	<* ASSERT NUMBER( ptree_pms^.PathToArgList^ ) > 0 *>
	argslist := FollowPath( root , ptree_pms^.PathToArgList ) ;
	(* TOOD What if user makes a grammar error and has
	two separate argument lists for same procedure? *)
	<* ASSERT argslist # NIL *>
	<* ASSERT NUMBER( argslist^ ) = 1 *>
	RETURN argslist ;
END GetArgsList ;

(* Get procedure name from procedure definition *)
PROCEDURE GetProcName( root : REF Node.T ; ptree_pms : REF PTreeParams ) : TEXT =
VAR
	procnamelist : REF ARRAY OF REF Node.T := NIL ;
BEGIN
	<* ASSERT root # NIL *>
	<* ASSERT root^.cat = Node.Category.NonTerminal *>
	<* ASSERT root^.val = ptree_pms^.ProcedureDefnVal *>
	<* ASSERT ptree_pms^.PathToProcedureName # NIL *>
	<* ASSERT NUMBER( ptree_pms^.PathToProcedureName^ ) # 0 *>
	procnamelist := FollowPath( root , ptree_pms^.PathToProcedureName ) ;
	(* TODO Again, probably want proper error handling *)
	<* ASSERT NUMBER( procnamelist^ ) = 1 *>
	<* ASSERT procnamelist[ FIRST( procnamelist^ ) ].cat = Node.Category.Identifier *>
	RETURN procnamelist[ FIRST( procnamelist^ ) ].val ;
END GetProcName ;

(* Get nth proc def with name *)
(* Return NIL if we can't find it *)
PROCEDURE GetNthProcDef( root : REF Node.T ; ptree_pms : REF PTreeParams ; ProcName : TEXT ; N : CARDINAL ) : REF Node.T =
VAR
	all_proc_defs : REF ARRAY OF REF Node.T := NIL ;
	proc_match_ctr : CARDINAL := 0 ;
BEGIN
	<* ASSERT root # NIL *>
	<* ASSERT ptree_pms # NIL *>
	(* Find all procedure defs *)
	all_proc_defs := Node.FindAllNonterms( root , ptree_pms^.ProcedureDefnVal ) ;
	(* Look through each one. If name matches, increment counter. *)
	(* When right counter value hit, return node. *)
	FOR proc_index := FIRST( all_proc_defs^ ) TO LAST( all_proc_defs^ ) DO
		IF GetProcName( all_proc_defs[ proc_index ] , ptree_pms ) = ProcName THEN
			IF proc_match_ctr = N THEN
				RETURN all_proc_defs[ proc_index ] ;
			END ;
			INC( proc_match_ctr ) ;
		END ;
	END ;
	(* If Nth hit not reached, return NIL. *)
	RETURN NIL ;
END GetNthProcDef ;

PROCEDURE DeleteArgWName( root : REF Node.T ; ptree_pms : REF PTreeParams ; argname : TEXT ) =
VAR
	new_args_list : REF ARRAY OF REF Node.T := NEW( REF ARRAY OF REF Node.T , 0 ) ;
	current_args_list : REF ARRAY OF REF Node.T := NIL ;
	current_arg_name : TEXT := "" ;
	current_arg_name_arr : REF ARRAY OF REF Node.T := NIL ;
BEGIN
END DeleteArgWName ;

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
		IF root^.children # NIL THEN
			FOR child_index := FIRST( root^.children^ ) TO LAST( root^.children^ ) DO
				TRY
					IndentedTreePrint( root^.children[ child_index ] , num_indents + 1 , write_stream ) ;
				EXCEPT
					| Wr.Failure( ErrCode ) => RAISE Wr.Failure( ErrCode ) ;
					| Thread.Alerted => RAISE Thread.Alerted ;
				END ;
			END ;
		END ;
	END ;
END IndentedTreePrint ;

BEGIN END Spec.
