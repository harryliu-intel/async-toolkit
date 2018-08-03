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
IMPORT ParserExt ;
IMPORT LexerExt ;

(**********************)
(* Visible Procedures *)
(**********************)

PROCEDURE Parse( in_fname : Pathname.T ) : REF Node.T RAISES { InvalidFname } =
VAR
	rd_handle : Rd.T ;
	parser := NEW( ParserExt.T ) ;
	lexer := NEW( LexerExt.T ) ;
	root : REF Node.T := NIL ;
BEGIN
	(* Take file name *)
	TRY
		rd_handle := FileRd.Open( in_fname ) ;
	EXCEPT
		| OSError.E( ErrCode ) =>
			EVAL ErrCode ;
			RAISE InvalidFname ;
	END ;
	(* Parse it *)
	EVAL lexer.setRd( rd_handle ) ;
	EVAL parser.setLex( lexer ).parse( ) ;
	root := ParserExt.GetParseTree( ) ;
	RETURN root ;
END Parse ;

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
