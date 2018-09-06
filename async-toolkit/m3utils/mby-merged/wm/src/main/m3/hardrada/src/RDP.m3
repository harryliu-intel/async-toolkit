MODULE RDP ;

(*************)
(** Imports **)
(*************)
IMPORT Rd ;
IMPORT Node ;
IMPORT RegEx ;
IMPORT ParseQueue ;
IMPORT Thread ;
IMPORT Text ;
IMPORT IO ;
IMPORT Fmt ;

(***********)
(** Types **)
(***********)
TYPE ProgText = RECORD
	txt : TEXT := "" ;
	index : CARDINAL := 0 ;
	offset_fifo : REF ParseQueue.T := NIL ;
END ;

(**************************)
(*** Visible Procedures ***)
(**************************)

PROCEDURE Parse( text_stream : Rd.T ) : BOOLEAN (* Node.T *) =
VAR
	progtext := NEW( REF ProgText ) ;
	proglen : CARDINAL := 0 ;
BEGIN
	(* TODO Error checking? *)
	(* Read the program's text *)
	TRY
		proglen := Rd.Length( text_stream ) ;
		progtext^.txt := Rd.GetText( text_stream , proglen ) ;
	EXCEPT
		| Rd.Failure => RETURN FALSE ;
		| Thread.Alerted => RETURN FALSE ;
	END ;
	(* If deducible from start symbol, fantastic. *)
	progtext^.index := 0 ;
	progtext^.offset_fifo := NIL ;
	RETURN Compilation( progtext ) ;
END Parse ;

(*************************)
(*** Hidden Procedures ***)
(*************************)

(** Stream manipulators **)

(* Advance one token *)
PROCEDURE NextGrammarRule( progtext : REF ProgText ) =
BEGIN
	<* ASSERT progtext # NIL *>
	IF progtext^.offset_fifo = NIL THEN
		progtext^.offset_fifo := NEW( REF ParseQueue.T ) ;
	END ;
	(* Push 0 to the top of FIFO queue *)
	ParseQueue.Push( progtext^.offset_fifo ) ;
END NextGrammarRule ;

PROCEDURE ResetGrammarRule( progtext : REF ProgText ) =
BEGIN
	<* ASSERT progtext # NIL *>
	<* ASSERT progtext^.offset_fifo # NIL *>
	<* ASSERT ParseQueue.Depth( progtext^.offset_fifo ) > 0 *>
	(* Pop 0 off FIFO top *)
	ParseQueue.Pop( progtext^.offset_fifo ) ;
	(* Reset index *)
	progtext^.index := ParseQueue.Sum( progtext^.offset_fifo ) ;
END ResetGrammarRule ;

PROCEDURE FinishGrammarRule( progtext : REF ProgText ) =
VAR
	fifo_top : CARDINAL := 0 ;
BEGIN
	(* Assert to ensure FIFO queue is not empty *)
	<* ASSERT progtext # NIL *>
	<* ASSERT progtext^.offset_fifo # NIL *>
	<* ASSERT ParseQueue.Depth( progtext^.offset_fifo ) > 0 *>
	(* Add the top FIFO queue value to the value beneath it *)
	fifo_top := ParseQueue.Peek( progtext^.offset_fifo ) ;
	ParseQueue.Pop( progtext^.offset_fifo ) ;
	IF progtext^.offset_fifo # NIL AND ParseQueue.Depth( progtext^.offset_fifo ) >= 1 THEN
		ParseQueue.Inc( progtext^.offset_fifo , fifo_top ) ;
	END ;
	INC( progtext^.index , fifo_top ) ;
END FinishGrammarRule ;

PROCEDURE IsWhitespace( char : CHAR ) : BOOLEAN =
BEGIN
	IF Text.Equal( Text.FromChar( char ) , " " ) OR Text.Equal( Text.FromChar( char ) , "\n" ) OR Text.Equal( Text.FromChar( char ) , "\t" ) THEN
		RETURN TRUE ;
	ELSE
		RETURN FALSE ;
	END ;
END IsWhitespace ;

PROCEDURE Match( progtext : REF ProgText ; pattern : TEXT ; optional : BOOLEAN := FALSE ) : BOOLEAN =
VAR
	regex_pattern : RegEx.Pattern ;
	token_len_cnt : CARDINAL := 0 ;
BEGIN
	<* ASSERT progtext # NIL *>
	(* Skip whitespace *)
	WHILE IsWhitespace( Text.GetChar( progtext^.txt , progtext^.index ) ) DO
		IO.Put( "Skipping ws char: " & Text.FromChar( Text.GetChar( progtext^.txt , progtext^.index ) ) & " (index: " & Fmt.Int( progtext^.index ) & ")\n" ) ;
		INC( progtext^.index ) ;
		IF progtext^.offset_fifo # NIL AND ParseQueue.Depth( progtext^.offset_fifo ) > 0 THEN
			ParseQueue.Inc( progtext^.offset_fifo ) ;
		END ;
	END ;
	(* Push 0 to top of FIFO queue *)
	NextGrammarRule( progtext ) ;
	(* Get the length of the token *)
	token_len_cnt := 0 ;
	WHILE NOT IsWhitespace( Text.GetChar( progtext^.txt , progtext^.index + token_len_cnt ) ) DO
		IO.Put( "Non-ws char: " & Text.FromChar( Text.GetChar( progtext^.txt , progtext^.index + token_len_cnt ) ) & " (index: " & Fmt.Int( progtext^.index ) & ")\n" ) ;
		ParseQueue.Inc( progtext^.offset_fifo ) ;
		INC( token_len_cnt ) ;
	END ;
	(* Regex match the character *)
	TRY
		regex_pattern := RegEx.Compile( pattern ) ;
		IO.Put( "Current token (index: " & Fmt.Int( progtext^.index ) & "): " & Text.Sub( progtext^.txt , progtext^.index , token_len_cnt ) & "\n" ) ;
		IF RegEx.Execute( regex_pattern , progtext^.txt , progtext^.index , token_len_cnt , NIL ) # -1 THEN
			IO.Put( "Proper match. Parsed " & Text.Sub( progtext^.txt , progtext^.index , token_len_cnt ) & " . Expecting '" & pattern & "' Returning TRUE!\n" ) ;
			FinishGrammarRule( progtext ) ;
			RETURN TRUE ;
		ELSE
			IO.Put( "Mismatch. Parsed " & Text.Sub( progtext^.txt , progtext^.index , token_len_cnt ) & " . Expecting '" & pattern & "'. Returning " & Fmt.Bool( optional ) & "!\n" ) ;
			IO.Put( "Index reset to " & Fmt.Int( progtext^.index ) & "\n" ) ;
			ResetGrammarRule( progtext ) ;
			RETURN optional ;
		END ;
	EXCEPT
		(* TODO More elegant way of handling errors? *)
		| RegEx.Error( ErrMsg ) => EVAL ErrMsg ; RETURN FALSE ;
	END ;
END Match ;

(** M3 Grammar BNF **)
(* All functions guarantee that if it returns FALSE, you only need
to go back 1 token. *)

PROCEDURE Compilation( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "UNSAFE" , TRUE ) AND ( Interface( progtext ) OR Module( progtext ) OR GenInf( progtext ) OR GenMod( progtext ) ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
	
END Compilation ;

PROCEDURE Interface( progtext : REF ProgText ) : BOOLEAN =
BEGIN

	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;

	(* Mandatory INTERFACE keyword *)
	IF NOT Match( progtext , "INTERFACE" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;

	(* Mandatory Id *)
	IF NOT Id( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;

	(* Route 1 *)
	IF Interface_Route1( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;

	(* Route 2 *)
	IF Interface_Route2( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;

	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;

END Interface ;

PROCEDURE Interface_Route1( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Match( progtext , ";" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Import( progtext ) DO
	END ;
	WHILE Decl( progtext ) DO
	END ;
	IF Match( progtext , "END" ) AND Id( progtext ) AND Match( progtext , "." ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSE
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	
END Interface_Route1 ;

PROCEDURE Interface_Route2( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "=" ) AND Id( progtext ) AND GenActls( progtext ) AND Match( progtext , "END" ) AND Id( progtext ) AND Match( progtext , "." ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSE
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
END Interface_Route2 ;

PROCEDURE Module( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Module_Route1( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	IF Module_Route2( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Module ;

PROCEDURE Exports_IdList( progtext : REF ProgText ; optional : BOOLEAN := FALSE ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "EXPORTS" ) AND IdList( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN optional ;
END Exports_IdList ;

PROCEDURE Module_Route1( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "MODULE" ) AND Id( progtext ) AND Exports_IdList( progtext , TRUE ) AND Match( progtext , ";" ) THEN
		WHILE Import( progtext ) DO
		END ;
		IF Block( progtext ) AND Id( progtext ) AND Match( progtext , "." ) THEN
			FinishGrammarRule( progtext ) ;
			RETURN TRUE ;
		ELSE
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	ELSE
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ; 
END Module_Route1 ;

PROCEDURE Module_Route2( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Match( progtext , "MODULE" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	IF NOT Id( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL Exports_IdList( progtext ) ;
	IF NOT Match( progtext , "=" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	IF Id( progtext ) AND GenActls( progtext ) AND Match( progtext , "END" ) AND Id( progtext ) AND Match( progtext , "." ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSE
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
END Module_Route2 ;

PROCEDURE GenInf( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT( Match( progtext , "GENERIC" ) AND Match( progtext , "INTERFACE" ) AND Id( progtext ) AND GenFmls( progtext ) AND Match( progtext , ";" ) ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Import( progtext ) DO
	END ;
	WHILE Decl( progtext ) DO
	END ;
	IF Match( progtext , "END" ) AND Id( progtext ) AND Match( progtext , "." ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END GenInf ;

PROCEDURE GenMod( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT( Match( progtext , "GENERIC" ) AND Match( progtext , "MODULE" ) AND Id( progtext ) AND GenFmls( progtext ) AND Match( progtext , ";" ) ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Import( progtext ) DO
	END ;
	IF Block( progtext ) AND Id( progtext ) AND Match( progtext , "." ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END GenMod ;

PROCEDURE Import( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF AsImport( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	IF FromImport( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Import ;

PROCEDURE AsImport( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT ( Match( progtext , "IMPORT" ) AND ImportItem( progtext ) ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Match( progtext , "," ) DO
		IF NOT ImportItem( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	IF Match( progtext , ";" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END AsImport ;

PROCEDURE FromImport( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "FROM" ) AND Id( progtext ) AND Match( progtext , "IMPORT" ) AND IdList( progtext ) AND Match( progtext , ";" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END FromImport ;

PROCEDURE Block( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	WHILE Decl( progtext ) DO
	END ;
	IO.Put( "=============================== PARSING BLOCK ===============================\n" ) ;
	IF Match( progtext , "BEGIN" ) AND S( progtext ) AND Match( progtext , "END" ) THEN
		IO.Put( "Successfully parsed block!\n" ) ;
		FinishGrammarRule( progtext ) ;
		IO.Put( "=============================== PARSING BLOCK ===============================\n" ) ;
		RETURN TRUE ;
	END ;
	IO.Put( "Unsuccessfully parsed block!\n" ) ;
	IO.Put( "=============================== PARSING BLOCK ===============================\n" ) ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Block ;

PROCEDURE Decl( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF DeclRule1( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	IF DeclRule2( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	IF DeclRule3( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	IF DeclRule4( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	IF DeclRule5( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	IF DeclRule6( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Decl ;

PROCEDURE DeclRule1( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Match( progtext , "CONST" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE ConstDecl( progtext ) DO
		IF NOT Match( progtext , ";" ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	RETURN TRUE ;
END DeclRule1 ;

PROCEDURE DeclRule2( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Match( progtext , "TYPE" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE TypeDecl( progtext ) DO
		IF NOT Match( progtext , ";" ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	RETURN TRUE ;
END DeclRule2 ;

PROCEDURE DeclRule3( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Match( progtext , "EXCEPTION" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE ExceptionDecl( progtext ) DO
		IF NOT Match( progtext , ";" ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	RETURN TRUE ;
END DeclRule3 ;

PROCEDURE DeclRule4( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Match( progtext , "VAR" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE VariableDecl( progtext ) DO
		IF NOT Match( progtext , ";" ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	RETURN TRUE ;
END DeclRule4 ;

PROCEDURE Optional_Block_Id( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT ( Match( progtext , "=" ) AND Block( progtext ) AND Id( progtext ) ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	RETURN TRUE ;
END Optional_Block_Id ;

PROCEDURE DeclRule5( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT ProcedureHead( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL Optional_Block_Id( progtext ) ;
	IF NOT Match( progtext , ";" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	RETURN TRUE ;
END DeclRule5 ;

PROCEDURE Reveal_QualId_Block( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT QualId( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	IF NOT ( Match( progtext , "=" ) OR Match( progtext , "<:" ) ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	IF Type( progtext ) AND Match( progtext , ";" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Reveal_QualId_Block ;

PROCEDURE DeclRule6( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Match( progtext , "REVEAL" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Reveal_QualId_Block( progtext ) DO
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END DeclRule6 ;

PROCEDURE GenFmls( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Match( progtext , "(" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL IdList( progtext ) ;
	IF Match( progtext , ")" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END GenFmls ;

PROCEDURE GenActls( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Match( progtext , "(" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL IdList( progtext ) ;
	IF Match( progtext , ")" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END GenActls ;

PROCEDURE ImportItem( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Id( progtext ) OR ( Id( progtext ) AND Match( progtext , "AND" ) AND Id( progtext ) ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END ImportItem ;

PROCEDURE Optional_Colon_Type( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , ":" ) AND Type( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Optional_Colon_Type ;

PROCEDURE ConstDecl( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Id( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL Optional_Colon_Type( progtext ) ;
	IF Match( progtext , "=" ) AND ConstExpr( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END ConstDecl ;

PROCEDURE TypeDecl( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Id( progtext ) AND ( Match( progtext , "=" ) OR Match( progtext , "<:" ) ) AND Type( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END TypeDecl ;

PROCEDURE ExceptionDecl( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Id( progtext ) OR ( Match( progtext , "(" ) AND Type( progtext ) AND Match( progtext , ")" ) ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END ExceptionDecl ;

PROCEDURE VariableDecl( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF ( IdList( progtext ) AND Match( progtext , ":" ) AND Type( progtext ) AND Match( progtext , ":=" ) AND Expr( progtext ) ) OR
	   ( IdList( progtext ) AND Match( progtext , ":" ) AND Type( progtext ) ) OR
	   ( IdList( progtext ) AND Match( progtext , ":=" ) AND Expr( progtext ) ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END VariableDecl ;

PROCEDURE ProcedureHead( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "PROCEDURE" ) AND Id( progtext ) AND Signature( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END ProcedureHead ;

PROCEDURE MatchAndType( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	IF Match( progtext , ":" ) AND Type( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END MatchAndType ;

PROCEDURE MatchAndRaises( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	IF Match( progtext , "RAISES" ) AND Raises( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END MatchAndRaises ;

PROCEDURE Signature( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT ( Match( progtext , "(" ) AND Formals( progtext ) AND Match( progtext , ")" ) ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL MatchAndType( progtext ) ;
	EVAL MatchAndRaises( progtext ) ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END Signature ;

PROCEDURE Opt_Formals( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Formal( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Match( progtext , ";" ) DO
		IF NOT Formal( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	EVAL Match( progtext , ";" ) ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END Opt_Formals ;

PROCEDURE Formals( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	EVAL Opt_Formals( progtext ) ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END Formals ;

PROCEDURE Formal( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	EVAL Mode( progtext ) ;
	IF IdList( progtext ) AND
	   ( ( Match( progtext , ":" ) AND Type( progtext ) AND Match( progtext , ":=" ) AND ConstExpr( progtext ) ) OR
	     ( Match( progtext , ":" ) AND Type( progtext ) ) OR
	     ( Match( progtext , ":=" ) AND ConstExpr( progtext ) )
	   ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Formal ;

PROCEDURE Mode( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "VALUE" ) OR Match( progtext , "VAR" ) OR Match( progtext , "READONLY" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Mode ;

PROCEDURE Optional_QualId_List( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT QualId( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Match( progtext , "," ) DO
		IF NOT QualId( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END Optional_QualId_List ;

PROCEDURE Raises( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Raises_Route1( progtext ) OR Match( progtext , "ANY" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Raises ;

PROCEDURE Raises_Route1( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Match( progtext , "{" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL Optional_QualId_List( progtext ) ;
	IF NOT Match( progtext , "}" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END Raises_Route1 ;

PROCEDURE Stmt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF EvalSt( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Stmt ;

PROCEDURE RealStmt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Block( progtext ) OR CaseSt( progtext ) OR ExitSt( progtext ) OR EvalSt( progtext ) OR
	   ForSt( progtext ) OR IfSt( progtext ) OR LockSt( progtext ) OR LoopSt( progtext ) OR
	   RaiseSt( progtext ) OR RepeatSt( progtext ) OR ReturnSt( progtext ) OR TCaseSt( progtext ) OR 
	   TryXptSt( progtext ) OR TryFinSt( progtext ) OR WhileSt( progtext ) OR WithSt( progtext ) OR
	   AssignSt( progtext ) OR CallSt( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END RealStmt ;

PROCEDURE Optional_Stmt_List( progtext : REF ProgText ; optional : BOOLEAN := FALSE ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Stmt( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN optional ;
	END ;
	WHILE Match( progtext , ";" ) DO
		IF NOT Stmt( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN optional ;
		END ;
	END ;
	IF Match( progtext , ";" , optional ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN optional ;
END Optional_Stmt_List ;

PROCEDURE S( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Optional_Stmt_List( progtext , TRUE ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END S ;

PROCEDURE AssignSt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Expr( progtext ) AND Match( progtext , ":=" ) AND Expr( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END AssignSt ;

PROCEDURE Optional_Actual_List( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Actual( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Match( progtext , "," ) DO
		IF NOT Actual( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END Optional_Actual_List ;

PROCEDURE CallSt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT ( Expr( progtext ) AND Match( progtext , "(" ) ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL Optional_Actual_List( progtext ) ;
	IF Match( progtext , ")" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END CallSt ;

PROCEDURE Optional_Else_S( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF ( Match( progtext , "ELSE" ) AND S( progtext ) ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Optional_Else_S ;

PROCEDURE CaseSt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT ( Match( progtext , "CASE" ) AND Expr( progtext ) AND Match( progtext , "OF" ) ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL Case( progtext ) ;
	WHILE Match( progtext , "|" ) DO
		IF NOT Case( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	EVAL Optional_Else_S( progtext ) ;
	IF NOT Match( progtext , "END" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END CaseSt ;

PROCEDURE ExitSt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "EXIT" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END ExitSt ;

PROCEDURE EvalSt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT ( Match( progtext , "EVAL" ) AND Expr( progtext ) ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END EvalSt ;

PROCEDURE ForSt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT ( Match( progtext , "FOR" ) AND Id( progtext ) AND Match( progtext , ":=" ) AND
		 Expr( progtext ) AND Match( progtext , "TO" ) AND Expr( progtext ) ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL ByExpr( progtext ) ;
	IF NOT ( Match( progtext , "DO" ) AND S( progtext ) AND Match( progtext , "END" ) ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END ForSt ;

PROCEDURE ByExpr( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT ( Match( progtext , "BY" ) AND Expr( progtext ) ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END ByExpr ;

PROCEDURE Optional_Elsif( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "ELSIF" ) AND Expr( progtext ) AND Match( progtext , "THEN" ) AND S( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Optional_Elsif ;

PROCEDURE IfSt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT ( Match( progtext , "IF" ) AND Expr( progtext ) AND Match( progtext , "THEN" ) AND S( progtext ) ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Optional_Elsif( progtext ) DO
	END ;
	EVAL Optional_Else_S( progtext ) ;
	IF Match( progtext , "END" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END IfSt ;

PROCEDURE LockSt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "LOCK" ) AND Expr( progtext ) AND Match( progtext , "DO" ) AND S( progtext ) AND Match( progtext , "END" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END LockSt ;

PROCEDURE LoopSt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "LOOP" ) AND S( progtext ) AND Match( progtext , "END" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END LoopSt ;

PROCEDURE Optional_Expr( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "(" ) AND Expr( progtext ) AND Match( progtext , ")" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Optional_Expr ;

PROCEDURE RaiseSt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT ( Match( progtext , "RAISE" ) AND QualId( progtext ) ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL Optional_Expr( progtext ) ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END RaiseSt ;

PROCEDURE RepeatSt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "REPEAT" ) AND S( progtext ) AND Match( progtext , "UNTIL" ) AND Expr( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END RepeatSt ;

PROCEDURE ReturnSt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Match( progtext , "RETURN" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL Expr( progtext ) ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END ReturnSt ;

PROCEDURE TCaseSt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT ( Match( progtext , "TYPECASE" ) AND Expr( progtext ) AND Match( progtext , "OF" ) ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL TCase( progtext ) ;
	WHILE Match( progtext , "|" ) DO
		IF NOT TCase( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	EVAL Optional_Else_S( progtext ) ;
	IF NOT Match( progtext , "END" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END TCaseSt ;

PROCEDURE TryXptSt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT ( Match( progtext , "TRY" ) AND S( progtext ) AND Match( progtext , "EXCEPT" ) ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL Handler( progtext ) ;
	WHILE Match( progtext , "|" ) DO
		IF NOT Handler( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	EVAL Optional_Else_S( progtext ) ;
	IF Match( progtext , "END" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END TryXptSt ;

PROCEDURE TryFinSt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "TRY" ) AND S( progtext ) AND Match( progtext , "FINALLY" ) AND S( progtext ) AND Match( progtext , "END" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END TryFinSt ;

PROCEDURE WhileSt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "WHILE" ) AND Expr( progtext ) AND Match( progtext , "DO" ) AND S( progtext ) AND Match( progtext , "END" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END WhileSt ;

PROCEDURE WithSt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT ( Match( progtext , "WITH" ) AND Binding( progtext ) ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Match( progtext , "," ) DO
		IF NOT Binding( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	IF Match( progtext , "DO" ) AND S( progtext ) AND Match( progtext , "END" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END WithSt ;

PROCEDURE Case( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Labels( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Match( progtext , "," ) DO
		IF NOT Labels( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	IF Match( progtext , "=>" ) AND S( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Case ;

PROCEDURE Optional_ConstExpr_DotDot( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , ".." ) AND ConstExpr( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Optional_ConstExpr_DotDot ;

PROCEDURE Labels( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT ConstExpr( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL Optional_ConstExpr_DotDot( progtext ) ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END Labels ;

PROCEDURE Optional_Id( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "(" ) AND Id( progtext ) AND Match( progtext , ")" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Optional_Id ;

PROCEDURE Handler( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT QualId( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Match( progtext , "," ) DO
		IF NOT QualId( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	EVAL Optional_Id( progtext ) ;
	IF Match( progtext , "=>" ) AND S( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Handler ;

PROCEDURE TCase( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Type( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Match( progtext , "," ) DO
		IF NOT Type( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	EVAL Optional_Id( progtext ) ;
	IF Match( progtext , "=>" ) AND S( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END TCase ;

PROCEDURE Binding( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Id( progtext ) AND Match( progtext , "=" ) AND Expr( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Binding ;

PROCEDURE Optional_Id_Assign( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Id( progtext ) AND Match( progtext , ":=" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Optional_Id_Assign ;

PROCEDURE Actual( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Type( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	EVAL Optional_Id_Assign( progtext ) ;
	IF Expr( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Actual ;

PROCEDURE Type( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF ObjectType( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSIF TypeName( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSIF ArrayType( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSIF PackedType( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSIF EnumType( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSIF ProcedureType( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSIF RecordType( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSIF RefType( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSIF SetType( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSIF SubrangeType( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSIF Match( progtext , "(" ) AND Type( progtext ) AND Match( progtext , ")" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSE
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
END Type ;

PROCEDURE Optional_Type_List( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Type( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Match( progtext , "," ) DO
		IF NOT Type( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END Optional_Type_List ;

PROCEDURE ArrayType( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Match( progtext , "ARRAY" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL Optional_Type_List( progtext ) ;
	IF Match( progtext , "OF" ) AND Type( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END ArrayType ;

PROCEDURE PackedType( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "BITS" ) AND ConstExpr( progtext ) AND Match( progtext , "FOR" ) AND Type( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END PackedType ;

PROCEDURE EnumType( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Match( progtext , "{" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL IdList( progtext ) ;
	IF Match( progtext , "}" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END EnumType ;

PROCEDURE Optional_TypeName_OR_ObjectType( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF TypeName( progtext ) OR ObjectType( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Optional_TypeName_OR_ObjectType ;

PROCEDURE Optional_Methods( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "METHODS" ) AND Methods( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Optional_Methods ;

PROCEDURE Optional_Overrides( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "OVERRIDES" ) AND Overrides( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Optional_Overrides ;

PROCEDURE ObjectType( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	EVAL Optional_TypeName_OR_ObjectType( progtext ) ;
	EVAL Brand( progtext ) ;
	IF NOT ( Match( progtext , "OBJECT" ) AND Fields( progtext ) ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL Optional_Methods( progtext ) ;
	EVAL Optional_Overrides( progtext ) ;
	IF Match( progtext , "END" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END ObjectType ;

PROCEDURE ProcedureType( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "PROCEDURE" ) AND Signature( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END ProcedureType ;

PROCEDURE RecordType( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "RECORD" ) AND Fields( progtext ) AND Match( progtext , "END" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END RecordType ;

PROCEDURE RefType( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	EVAL Match( progtext , "UNTRACED" ) ;
	EVAL Brand( progtext ) ;
	IF Match( progtext , "REF" ) AND Type( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END RefType ;

PROCEDURE SetType( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "SET" ) AND Match( progtext , "OF" ) AND Type( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END SetType ;

PROCEDURE SubrangeType( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "[" ) AND ConstExpr( progtext ) AND Match( progtext , ".." ) AND ConstExpr( progtext ) AND Match( progtext , "]" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END SubrangeType ;

PROCEDURE Brand( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Match( progtext , "BRANDED" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL TextLiteral( progtext ) ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END Brand ;

PROCEDURE Optional_Fields_List( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Field( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Match( progtext , ";" ) DO
		IF NOT Field( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	EVAL Match( progtext , ";" ) ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END Optional_Fields_List ;

PROCEDURE Fields( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	EVAL Optional_Fields_List( progtext ) ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END Fields ;

PROCEDURE Field( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF IdList( progtext ) AND
	   ( ( Match( progtext , ":" ) AND Type( progtext ) AND Match( progtext , ":=" ) AND ConstExpr( progtext ) ) OR
	     ( Match( progtext , ":" ) AND Type( progtext ) ) OR 
	     ( Match( progtext , ":=" ) AND ConstExpr( progtext ) ) ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Field ;

PROCEDURE Optional_Methods_List( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Method( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Match( progtext , ";" ) DO
		IF NOT Method( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	EVAL Match( progtext , ";" ) ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END Optional_Methods_List ;

PROCEDURE Methods( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	EVAL Optional_Methods_List( progtext ) ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END Methods ;

PROCEDURE Optional_Assign_ConstExpr( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , ":=" ) AND ConstExpr( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Optional_Assign_ConstExpr ;

PROCEDURE Method( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT ( Id( progtext ) AND Signature( progtext ) ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL Optional_Assign_ConstExpr( progtext ) ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END Method ;

PROCEDURE Overrides( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Override( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Match( progtext , ";" ) DO
		IF NOT Override( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	EVAL Match( progtext , ";" ) ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END Overrides ;

PROCEDURE Override( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Id( progtext ) AND Match( progtext , ":=" ) AND ConstExpr( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Override ;

PROCEDURE ConstExpr( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Expr( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END ConstExpr ;

PROCEDURE Expr_Route1( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF E1( progtext ) AND Match( progtext , "OR" ) AND Expr( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Expr_Route1 ;

PROCEDURE Expr( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Expr_Route1( progtext ) OR E1( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Expr ;

PROCEDURE E1_Route1( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF E2( progtext ) AND Match( progtext , "AND" ) AND E1( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END E1_Route1 ;

PROCEDURE E1( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF E1_Route1( progtext ) OR E2( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END E1 ;

PROCEDURE E2_Route1( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "NOT" ) AND E2( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END E2_Route1 ;

PROCEDURE E2( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF E2_Route1( progtext ) OR E3( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END E2 ;

PROCEDURE E3_Route1( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF E4( progtext ) AND Relop( progtext ) AND E3( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END E3_Route1 ;


PROCEDURE E3( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF E3_Route1( progtext ) OR E4( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END E3 ;

PROCEDURE E4_Route1( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF E5( progtext ) AND Addop( progtext ) AND E4( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END E4_Route1 ;

PROCEDURE E4( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF E4_Route1( progtext ) OR E5( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END E4 ;

PROCEDURE E5_Route1( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF E6( progtext ) AND Mulop( progtext ) AND E5( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END E5_Route1 ;

PROCEDURE E5( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF E5_Route1( progtext ) OR E6( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END E5 ;

PROCEDURE E6_Route1( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF ( Match( progtext , "+" ) OR Match( progtext , "-" ) ) AND E6( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END E6_Route1 ;

PROCEDURE E6( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF E6_Route1( progtext ) OR E7( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END E6 ;

PROCEDURE E7_Route1( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF E7( progtext ) AND Selector( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END E7_Route1 ;

PROCEDURE E7( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF (* E7_Route1( progtext ) OR *) E8( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END E7 ;

PROCEDURE E8( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Number( progtext ) (* OR Id( progtext ) OR CharLiteral( progtext ) OR TextLiteral( progtext ) OR
	   Constructor( progtext ) OR ( Match( progtext , "(" ) AND Expr( progtext ) AND Match( progtext , ")" ) ) *) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END E8 ;

PROCEDURE Relop( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "=" ) OR Match( progtext , "#" ) OR Match( progtext , "<" ) OR Match( progtext , "<=" ) OR
	   Match( progtext , ">" ) OR Match( progtext , ">=" ) OR Match( progtext , "IN" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Relop ;

PROCEDURE Addop( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "+" ) OR Match( progtext , "-" ) OR Match( progtext , "&" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Addop ;

PROCEDURE Mulop( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "*" ) OR Match( progtext , "/" ) OR Match( progtext , "DIV" ) OR Match( progtext , "MOD" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Mulop ;

PROCEDURE Optional_Cons( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF SetCons( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSIF RecordCons( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSIF ArrayCons( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSE
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
END Optional_Cons ;

PROCEDURE Selector( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Selector_Route1( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSIF Selector_Route2( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSIF Selector_Route3( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSIF Selector_Route4( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	ELSE
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
END Selector ;

PROCEDURE Selector_Route1( progtext : REF ProgText ) : BOOLEAN =
VAR
	char_for_carat : CHAR := VAL( 94 , CHAR ) ;
	txt_for_carat : TEXT := Text.FromChar( char_for_carat ) ;
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	(*
	IF Match( progtext , txt_for_carat ) THEN
		IO.Put( "Match!\n" ) ;
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	*)
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Selector_Route1 ;

PROCEDURE Selector_Route2( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "." ) AND Id( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Selector_Route2 ;

PROCEDURE Selector_Route3( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "[" ) AND Expr( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	WHILE Match( progtext , "," ) DO
		IF NOT Expr( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	IF Match( progtext , "]" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Selector_Route3 ;

PROCEDURE Selector_Route4( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Match( progtext , "(" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL Optional_Actual_List( progtext ) ;
	IF Match( progtext , ")" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Selector_Route4 ;

PROCEDURE Constructor( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Type( progtext ) AND Match( progtext , "{" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL Optional_Cons( progtext ) ;
	IF Match( progtext , "}" ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END Constructor ;

PROCEDURE SetCons( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT SetElt( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Match( progtext , "," ) DO
		IF NOT SetElt( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END SetCons ;

PROCEDURE SetElt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Expr( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Match( progtext , ".." ) DO
		IF NOT Expr( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END SetElt ;

PROCEDURE RecordCons( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT RecordElt( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Match( progtext , "," ) DO
		IF NOT RecordElt( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END RecordCons ;

PROCEDURE RecordElt( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	EVAL Optional_Id_Assign( progtext ) ;
	IF Expr( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END RecordElt ;

PROCEDURE Optional_Comma_DotDot( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "," ) AND Match( progtext , ".." ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Optional_Comma_DotDot ;

PROCEDURE ArrayCons( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Expr( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Match( progtext , "," ) DO
		IF NOT Expr( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	EVAL Optional_Comma_DotDot( progtext ) ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END ArrayCons ;

PROCEDURE IdList( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Id( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	WHILE Match( progtext , "," ) DO
		IF NOT Id( progtext ) THEN
			ResetGrammarRule( progtext ) ;
			RETURN FALSE ;
		END ;
	END ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END IdList ;

PROCEDURE DotId( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "." ) AND Id( progtext ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END DotId ;

PROCEDURE QualId( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF NOT Id( progtext ) THEN
		ResetGrammarRule( progtext ) ;
		RETURN FALSE ;
	END ;
	EVAL DotId( progtext ) ;
	FinishGrammarRule( progtext ) ;
	RETURN TRUE ;
END QualId ;

PROCEDURE TypeName( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF QualId( progtext ) OR Match( progtext , "ROOT" ) OR Match( progtext , "UNTRACED ROOT" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END TypeName ;

(** M3 Tokens **)

PROCEDURE Id( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	IF Match( progtext , "[a-zA-Z][a-zA-Z0-9_]*" ) THEN
		RETURN TRUE ;
	END ;
	RETURN FALSE ;
END Id ;

PROCEDURE CharLiteral( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "[a-zA-Z0-9 !#$%&()*+,-./:;<=>?@[]^_'{|}\\t\\n\\r\\v\\f]\"" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END CharLiteral ;

PROCEDURE Number( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "[0-9][0-9]*" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END Number ;

PROCEDURE TextLiteral( progtext : REF ProgText ) : BOOLEAN =
BEGIN
	<* ASSERT progtext # NIL *>
	NextGrammarRule( progtext ) ;
	IF Match( progtext , "\"[a-zA-Z0-9 !#$%&()*+,-./:;<=>?@[]^_'{|}\\t\\n\\r\\v\\f]*\"" ) THEN
		FinishGrammarRule( progtext ) ;
		RETURN TRUE ;
	END ;
	ResetGrammarRule( progtext ) ;
	RETURN FALSE ;
END TextLiteral ;

BEGIN END RDP .
