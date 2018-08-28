UNSAFE MODULE Spec;

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
IMPORT DepGraph ;
IMPORT SymbolTbl ;
IMPORT TypeUseTbl ;
IMPORT CARDINALList ;
IMPORT REFANYList ;
IMPORT Unix ;
IMPORT Ctypes ;
IMPORT M3toC ;
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

PROCEDURE GenCodeText( root : REF Node.T ; style_rules_array : StyleRulesTbl.Default ) : TEXT =
VAR
	term_list : TextList.T := NIL ;
	return_text : TEXT := "" ;
BEGIN
	<* ASSERT root # NIL *>
	<* ASSERT style_rules_array # NIL *>
	term_list := GetTokenList( root , style_rules_array ) ;
	<* ASSERT term_list # NIL *>
	WHILE term_list # NIL DO
		return_text := return_text & term_list.head ;
		term_list := term_list.tail ;
	END ;
	RETURN return_text ;
END GenCodeText ;

(* Get a particular variable's value after the execution of the particular statement *)
(* This function's definition restricts the partial evaluator to only running on x86-64 architectures and Unix OSs *)
PROCEDURE GetVarValueAfterStatement( src : REF DepGraph.T ; depgraph_pms : REF DepGraph.DepGraphParams ; style_rules_array : StyleRulesTbl.Default ; varname : TEXT ; symbtbl : SymbolTbl.Default ; abs_path_to_top_dir : Pathname.T ) : TEXT RAISES { InvalidFname , OutError , ReadError } =
VAR
	specdir_dirname : Ctypes.char_star ;
	specdir_src_dirname : Ctypes.char_star ;
	specdir_src_genfiles_dirname : Ctypes.char_star ;
	specdir_dir : TEXT := "" ;
	specdir_src_dir : TEXT := "" ;
	specdir_src_genfiles_dir : TEXT := "" ;
	m3makefile_fhandle : Wr.T ;
	main_fhandle : Wr.T ;
	compiler_name : TEXT := "cm3" ;
	compiler_name_ptr : Ctypes.char_star ;
	program_abs_path : TEXT := "" ;
	program_abs_path_ptr : Ctypes.char_star ;
	dumpfile_name : TEXT := "" ;
	dumpfile_handle : Rd.T ;
	dumpfile_len : CARDINAL := 0 ;
	return_text : TEXT := "" ;
	temp_assigned_vars : TextList.T := NIL ;
	varname_valid : BOOLEAN := FALSE ;
BEGIN
	IO.Put( "===== RUNNING GetVarValueAfterStatement in Spec.m3 =====\n" ) ;
	(* Assertions *)
	IO.Put( "Initial assertions...\n" ) ;
	<* ASSERT src # NIL *>
	<* ASSERT depgraph_pms # NIL *>
	(* Check to ensure varname is in assigned_vars list of depgraph *)
	IO.Put( "Check to ensure varname is in assigned_vars list of depgraph...\n" ) ;
	temp_assigned_vars := src^.assigned_vars ;
	IO.Put( "My varname: " & varname & "\n" ) ;
	WHILE temp_assigned_vars # NIL DO
		IO.Put( "Assigned var: " & temp_assigned_vars.head & "\n" ) ;
		IF temp_assigned_vars.head = varname THEN
			varname_valid := TRUE ;
		END ;
		temp_assigned_vars := temp_assigned_vars.tail ;
	END ;
	<* ASSERT varname_valid = TRUE *>
	(* Ensure that abs_path_to_top_dir is an absolute path ending with "/" *)
	IO.Put( "Ensure that abs_path_to_top_dir is a valid absolute path ending with /...\n" ) ;
	<* ASSERT Pathname.Valid( abs_path_to_top_dir ) = TRUE *> (* Valid path *)
	<* ASSERT Pathname.Absolute( abs_path_to_top_dir ) = TRUE *> (* Abs path *)
	<* ASSERT Text.Equal( Text.Sub( abs_path_to_top_dir , Text.Length( abs_path_to_top_dir ) - 1 , 1 ) , "/" ) *> (* Ends in / *)
	(* Make specdir/ *)
	IO.Put( "Making specdir/...\n" ) ;
	specdir_dir := abs_path_to_top_dir & "specdir/" ;
	specdir_dirname := M3toC.CopyTtoS( specdir_dir ) ;
	EVAL Unix.mkdir( specdir_dirname , 8_040000 ) ;
	EVAL Unix.chmod( specdir_dirname , 16_FFF ) ;
	(* Make specdir/src/ *)
	IO.Put( "Making specdir/src/...\n" ) ;
	specdir_src_dir := abs_path_to_top_dir & "specdir/src/" ;
	specdir_src_dirname := M3toC.CopyTtoS( specdir_src_dir ) ;
	EVAL Unix.mkdir( specdir_src_dirname , 8_040000 ) ;
	EVAL Unix.chmod( specdir_src_dirname , 16_FFF ) ;
	(* Make specdir/src/genfiles/ *)
	IO.Put( "Making specdir/src/genfiles/...\n" ) ;
	specdir_src_genfiles_dir := abs_path_to_top_dir & "specdir/src/genfiles/" ;
	specdir_src_genfiles_dirname := M3toC.CopyTtoS( specdir_src_genfiles_dir ) ;
	EVAL Unix.mkdir( specdir_src_genfiles_dirname , 8_040000 ) ;
	EVAL Unix.chmod( specdir_src_genfiles_dirname , 16_FFF ) ;
	(* Generate m3makefile in specdir/src/ *)
	IO.Put( "Generating m3makefile in specdir/src/...\n" ) ;
	TRY
		IO.Put( "Creating m3makefile at " & specdir_src_dir & "/m3makefile\n" ) ;
		m3makefile_fhandle := FileWr.Open( specdir_src_dir & "/m3makefile" ) ;
		IO.Put( "Wrote text...\n" ) ;
		Wr.PutText( m3makefile_fhandle , GenM3MakefileCode( "Main" , "Main" ) ) ;
		IO.Put( "Closing m3makefile\n" ) ;
		Wr.Close( m3makefile_fhandle ) ;
	EXCEPT
		| OSError.E( ErrCode ) => EVAL ErrCode ; RAISE InvalidFname ;
		| Wr.Failure( ErrCode ) => EVAL ErrCode ; RAISE OutError ;
		| Thread.Alerted => RAISE OutError ;
	END ;
	(* Generate Main.m3 in specdir/src/... *)
	IO.Put( "Generating Main.m3 in specdir/src/...\n" ) ; 
	TRY
		main_fhandle := FileWr.Open( specdir_src_dir & "/Main.m3" ) ;
		dumpfile_name := specdir_src_genfiles_dir & "/dumpfile" ;
		Wr.PutText( main_fhandle , GenSpecFileCode( src , depgraph_pms , style_rules_array , varname , symbtbl , dumpfile_name ) ) ;
		Wr.Close( main_fhandle ) ;
	EXCEPT
		| OSError.E( ErrCode ) => EVAL ErrCode ; RAISE InvalidFname ;
		| Wr.Failure( ErrCode ) => EVAL ErrCode ; RAISE OutError ;
		| Thread.Alerted => RAISE OutError ;
	END ;
	(* Run cm3 to compile *)
	IO.Put( "Running cm3 to compile...\n" ) ;
	compiler_name_ptr := M3toC.CopyTtoS( compiler_name ) ;
	EVAL Unix.execve( compiler_name_ptr , NIL , NIL ) ;
	(* Run program *)
	IO.Put( "Running program...\n" ) ; 
	program_abs_path := specdir_src_genfiles_dir & "/AMD64_LINUX/Main" ; (* Changes with architecture *)
	program_abs_path_ptr := M3toC.CopyTtoS( program_abs_path ) ;
	EVAL Unix.execve( program_abs_path_ptr , NIL , NIL ) ;
	(* Read file for text value *)
	IO.Put( "Reading file for text value...\n" ) ; 
	TRY
		dumpfile_handle := FileRd.Open( dumpfile_name ) ;
		TRY
			dumpfile_len := Rd.Length( dumpfile_handle ) ;
			return_text := Rd.GetText( dumpfile_handle , dumpfile_len ) ;
			Rd.Close( dumpfile_handle ) ;
		EXCEPT
			| Rd.Failure => RAISE ReadError ;
			| Thread.Alerted => RAISE ReadError ;
		END ;
	EXCEPT
		| OSError.E( ErrCode ) => EVAL ErrCode ; RAISE ReadError ;
		| ReadError => RAISE ReadError ;
	END ;
	(* Delete specdir/ *)
	IO.Put( "Deleting specdir/...\n" ) ; 
	EVAL Unix.rmdir( specdir_dirname ) ;
	EVAL Unix.rmdir( specdir_src_dirname ) ;
	EVAL Unix.rmdir( specdir_src_genfiles_dirname ) ;
	(* Manage memory *)
	IO.Put( "Managing memory...\n" ) ; 
	M3toC.FreeCopiedS( specdir_dirname ) ;
	M3toC.FreeCopiedS( specdir_src_dirname ) ;
	M3toC.FreeCopiedS( specdir_src_genfiles_dirname ) ;
	M3toC.FreeCopiedS( compiler_name_ptr ) ;
	M3toC.FreeCopiedS( program_abs_path_ptr ) ;
	(* Return the text value *)
	IO.Put( "===== RUNNING GetVarValueAfterStatement in Spec.m3 =====\n" ) ;
	RETURN return_text ;
END GetVarValueAfterStatement ;

PROCEDURE DebugTree( root : REF Node.T ; out_fname : Pathname.T ) RAISES { InvalidFname , OutError } =
VAR
	out_file_handle : Wr.T ;
BEGIN
	<* ASSERT root # NIL *>
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

PROCEDURE GenM3MakefileCode( src_fname : TEXT ; program_fname : TEXT ) : TEXT =
VAR
	text_to_return : TEXT := "" ;
BEGIN
	text_to_return := text_to_return & "implementation( \"" & src_fname & "\" )\n" ;
	text_to_return := text_to_return & "program( \"" & program_fname & "\" )\n" ;
	RETURN text_to_return ;
END GenM3MakefileCode ;

(* out_fname must be abs path *)
PROCEDURE GenSpecFileCode( src : REF DepGraph.T ; depgraph_pms : REF DepGraph.DepGraphParams ; style_rules_array : StyleRulesTbl.Default ; varname : TEXT ; symbtbl : SymbolTbl.Default ; out_fname : Pathname.T ) : TEXT =
VAR
	text_to_return : TEXT := "" ;
BEGIN
	text_to_return := text_to_return & "MODULE Main ;\n" ;
	text_to_return := text_to_return & "IMPORT FileWr ;\n" ;
	text_to_return := text_to_return & "IMPORT Wr ;\n" ;
	text_to_return := text_to_return & "IMPORT Fmt ;\n" ;
	text_to_return := text_to_return & "IMPORT OSError ;\n" ;
	text_to_return := text_to_return & "IMPORT Thread ;\n" ;
	text_to_return := text_to_return & GenProcDefCode( src , depgraph_pms , style_rules_array , varname , symbtbl ) & "\n" ;
	text_to_return := text_to_return & "VAR\n" ;
	text_to_return := text_to_return & "\tout_file_handle : Wr.T\n" ;
	text_to_return := text_to_return & "BEGIN\n" ;
	text_to_return := text_to_return & "TRY\n" ;
	text_to_return := text_to_return & "\tout_file_handle := FileWr.Open( \"" & out_fname & "\" ) ;\n" ;
	text_to_return := text_to_return & "\tWr.PutText( out_file_handle , SpecVar( ) ) ;\n" ;
	text_to_return := text_to_return & "\tWr.Close( out_file_handle ) ;\n" ;
	text_to_return := text_to_return & "EXCEPT\n" ;
	text_to_return := text_to_return & "\t| OSError.E( ErrCode ) => EVAL ErrCode ;\n" ;
	text_to_return := text_to_return & "\t| Wr.Failure( ErrCode ) => EVAL ErrCode ;\n" ;
	text_to_return := text_to_return & "\t| Thread.Alerted => EVAL 0 ;\n" ;
	text_to_return := text_to_return & "END ;\n" ;
	text_to_return := text_to_return & "END Main ;" ;
	RETURN text_to_return ;
END GenSpecFileCode ;

(* TODO Is there a way to make style_rules_array optional later? *)
(* NOTE: SOME LANGUAGE SPECIFIC M3 FEATURES *)
PROCEDURE GenProcDefCode( src : REF DepGraph.T ; depgraph_pms : REF DepGraph.DepGraphParams ; style_rules_array : StyleRulesTbl.Default ; varname : TEXT ; symbtbl : SymbolTbl.Default ) : TEXT =
VAR
	text_to_return : TEXT := "" ;
	var_type : TypeUseTbl.T ;
	type_text : TEXT := "" ;
	iter : TypeUseTbl.Iterator ;
	dead_bool : BOOLEAN := FALSE ;
	fmt_fn : TEXT := "" ;
BEGIN
	<* ASSERT src # NIL *>
	<* ASSERT depgraph_pms # NIL *>
	(* TODO Assert that varname must be in assigned_vars textlist *)
	IF symbtbl.get( varname , var_type ) THEN
		iter := var_type.iterate( ) ;
		WHILE iter.next( type_text , dead_bool ) DO
			<* ASSERT NOT Text.Equal( type_text , "" ) *>
			text_to_return := text_to_return & "PROCEDURE SpecVar( ) : TEXT =\n" ;
			text_to_return := text_to_return & "VAR\n" ;
			text_to_return := text_to_return & GenVarDefs( src , depgraph_pms , symbtbl ) ;
			text_to_return := text_to_return & "BEGIN\n" ;
			text_to_return := text_to_return & GenProcBodyCode( src , depgraph_pms , style_rules_array ) & "\n" ;
			(* TODO Tricky to do for arrays *)
			IF Text.Equal( type_text , "BOOLEAN" ) THEN
				fmt_fn := "Fmt.Bool" ;
			ELSIF Text.Equal( type_text , "INTEGER" ) THEN
				fmt_fn := "Fmt.Int" ;
			ELSIF Text.Equal( type_text , "ADDRESS" ) THEN
				fmt_fn := "Fmt.Addr" ;
			ELSIF Text.Equal( Text.Sub( type_text , 0 , Text.Length( "REF" ) ) , "REF" ) THEN
				fmt_fn := "Fmt.Ref" ;
			ELSIF Text.Equal( type_text , "REAL" ) THEN
				fmt_fn := "Fmt.Real" ;
			ELSIF Text.Equal( type_text , "LONGREAL" ) THEN
				fmt_fn := "Fmt.LongReal" ;
			ELSIF Text.Equal( type_text , "EXTENDED" ) THEN
				fmt_fn := "Fmt.Extended" ;
			ELSIF Text.Equal( type_text , "CHAR" ) THEN
				fmt_fn := "Fmt.Char" ;
			ELSIF Text.Equal( type_text , "TEXT" ) THEN
				fmt_fn := "" ;
			ELSE
				(* You haven't provided support for this datatype yet. *)
				<* ASSERT FALSE *>
			END ;
			text_to_return := text_to_return & "RETURN " & fmt_fn & "( " & varname & " ) ;\n" ;
			text_to_return := text_to_return & "END SpecVar ;\n" ;
		END ;
	ELSE
		<* ASSERT FALSE *>
	END ;
	RETURN text_to_return ;
END GenProcDefCode ;

PROCEDURE GenVarDefs( src : REF DepGraph.T ; depgraph_pms : REF DepGraph.DepGraphParams ; symbtbl : SymbolTbl.Default ) : TEXT =
VAR
	temp_dep_order : CARDINALList.T := NIL ;
	dep_index : CARDINAL := 0 ;
	temp_dep_list : REFANYList.T := NIL ;
	temp_dep : REF DepGraph.T := NIL ;
	temp_assigned_vars : TextList.T := NIL ;
	tut : TypeUseTbl.T ;
	the_typename : TEXT := "" ;
	assigned_bool : BOOLEAN := FALSE ;
	return_str : TEXT := "" ;
	iter :  TypeUseTbl.Iterator ;
BEGIN
	(* Initial assertions *)
	<* ASSERT src # NIL *>
	<* ASSERT depgraph_pms # NIL *>
	(* Default the symbol table to all false *)
	symbtbl := DefaultSymbolTbl( symbtbl ) ;
	(* For each dependency in use, look at assigned vars. *)
	temp_dep_order := src^.dep_order ;
	temp_dep_list := src^.deps ;
	WHILE temp_dep_order # NIL DO
		temp_dep := temp_dep_list.head ;
		IF dep_index = temp_dep_order.head THEN
			temp_assigned_vars := temp_dep^.assigned_vars ;
			WHILE temp_assigned_vars # NIL DO
				IF symbtbl.get( temp_assigned_vars.head , tut ) THEN
					iter := tut.iterate( ) ;
					WHILE iter.next( the_typename , assigned_bool ) DO
						IF assigned_bool = FALSE THEN
							(* If true, var already assigned, forget it. *)
							(* If false, concatenate with varname and type. *)
							return_str := return_str & temp_assigned_vars.head & " : " & the_typename & " ;\n" ;
							EVAL tut.put( the_typename , TRUE ) ;
						END ;
					END ;
				ELSE
					<* ASSERT FALSE *>
				END ;
				temp_assigned_vars := temp_assigned_vars.tail ;
			END ;
		END ;
		temp_dep_list := temp_dep_list.tail ;
		temp_dep_order := temp_dep_order.tail ;
		INC( dep_index ) ;
	END ;
	(* At the end, briefly check the node itself *)
	temp_assigned_vars := src^.assigned_vars ;
	WHILE temp_assigned_vars # NIL DO
		IF symbtbl.get( temp_assigned_vars.head , tut ) THEN
			iter := tut.iterate( ) ;
			WHILE iter.next( the_typename , assigned_bool ) DO
				IF assigned_bool = FALSE THEN
					(* If true, var already assigned, forget it. *)
					(* If false, concatenate with varname and type. *)
					return_str := return_str & temp_assigned_vars.head & " : " & the_typename & " ;\n" ;
					EVAL tut.put( the_typename , TRUE ) ;
				END ;
			END ;
		ELSE
			<* ASSERT FALSE *>
		END ;
		temp_assigned_vars := temp_assigned_vars.tail ;
	END ;
	(* Return text string *)
	RETURN return_str ;
END GenVarDefs ;

PROCEDURE DefaultSymbolTbl( symbtbl : SymbolTbl.Default ) : SymbolTbl.Default =
VAR
	typeusetbl : TypeUseTbl.T ;
	iter : SymbolTbl.Iterator ;
	deeper_iter : TypeUseTbl.Iterator ;
	the_typename : TEXT := "" ;
	varname : TEXT := "" ;
	my_bool : BOOLEAN := FALSE ;
BEGIN
	(* TODO Any assertions? *)
	iter := symbtbl.iterate( ) ;
	WHILE iter.next( varname , typeusetbl ) DO
		deeper_iter := typeusetbl.iterate( ) ;
		WHILE deeper_iter.next( the_typename , my_bool ) DO
			EVAL typeusetbl.put( the_typename , FALSE ) ;
		END ;
	END ; 
	RETURN symbtbl ;
END DefaultSymbolTbl ;

PROCEDURE GenProcBodyCode( src : REF DepGraph.T ; depgraph_pms : REF DepGraph.DepGraphParams ; style_rules_array : StyleRulesTbl.Default ) : TEXT =
VAR
	new_parse_root : REF Node.T := NIL ;
BEGIN
	<* ASSERT src # NIL *>
	<* ASSERT depgraph_pms # NIL *>
	new_parse_root := NEW( REF Node.T ) ;
	DepGraph.GenProcBodyParseTreeFromSingleNodeWithDeps( new_parse_root , src , depgraph_pms ) ;
	RETURN GenCodeText( new_parse_root , style_rules_array ) ;
END GenProcBodyCode ;

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
	Node.FollowPath( list , root , ptree_pms^.PathToArgList ) ;
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
	Node.FollowPath( list , root , ptree_pms^.PathToProcedureName ) ;
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
			Node.FollowPath( childname , child^.cur , ptree_pms^.PathToArgNameFromArg ) ;
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
			Node.FollowPath( level_above_arglist , root , adjusted_path ) ;
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
	Node.FollowPath( blocklist , root , ptree_pms^.PathToProcedureBlock ) ;
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
			IF PrintFileDebug = TRUE THEN
				TRY
					Wr.PutText( write_stream , "  " ) ;
				EXCEPT
					| Wr.Failure( ErrCode ) => RAISE Wr.Failure( ErrCode ) ;
					| Thread.Alerted => RAISE Thread.Alerted ;
				END ;
			ELSE
				IO.Put( "  " ) ;
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
		ELSIF root^.cat = Node.Category.Placeholder THEN
			node_cat := "Placeholder" ;
		(* TODO Do I need an else? *)
		END ;
		IF PrintFileDebug = TRUE THEN
			TRY
				Wr.PutText( write_stream , root^.val & " ( " & node_cat & " )\n" ) ;
			EXCEPT
				| Wr.Failure( ErrCode ) => RAISE Wr.Failure( ErrCode ) ;
				| Thread.Alerted => RAISE Thread.Alerted ;
			END ;
		ELSE
			IO.Put( root^.val & " ( " & node_cat & " )\n" ) ;
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
