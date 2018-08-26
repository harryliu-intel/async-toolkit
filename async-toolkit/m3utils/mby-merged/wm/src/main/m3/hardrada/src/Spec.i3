INTERFACE Spec ;

(***********)
(* Imports *)
(***********)
IMPORT Node ;
IMPORT Pathname ;
IMPORT TextList ;
IMPORT StyleRulesTbl ;

(**************)
(* Exceptions *)
(**************)
EXCEPTION InvalidFname ;
EXCEPTION OutError ;

(*************)
(* Constants *)
(*************)

(* True - DebugTree prints to a file
   False - DebugTree prints to the console *)
CONST PrintFileDebug = TRUE ;

(*********)
(* Types *)
(*********)

(* Args and separators are the immediate children of ArgList *)
TYPE PTreeParams = RECORD
	ProcedureDefnVal : TEXT := "" ;
	ArgSeparator : TEXT := "" ;
	PathToProcedureBlock : TextList.T := NIL ;
	PathToProcedureName : TextList.T := NIL ;
	PathToArgList : TextList.T := NIL ;
	PathToArgNameFromArg : TextList.T := NIL ;
END ;

TYPE SpecParams = RECORD
	specblock : REF Node.T := NIL ;
	static_args : TextList.T := NIL ;
	procname : TEXT := "" ;
	procdefnumber : CARDINAL := 0 ;
END ;

TYPE StyleRule = RECORD
	Grule : TEXT := "" ;
	Index : CARDINAL := 0 ;
	TextToPrintAfter : TEXT := "" ;
END ;

(**************)
(* Procedures *)
(**************)
(* Note: These procedures assume that every node in the tree
has at most one unique parent node, and no recursive feedback
loops exist. *)

(* Parse
- in_fname :: relative input file path
Returns a reference to the root node of the generated parse tree
when EOF character is printed to the stream
Raises InvalidFname exception if path does not exist
in filesystem
*)
(* PROCEDURE Parse( in_fname : Pathname.T ) : REF Node.T RAISES { InvalidFname } ; *)

(* Specialize 
Note: root node will most likely be changed as a result of specialization. *)
PROCEDURE Specialize( root : REF Node.T ; spec_pms : REF SpecParams ; ptree_pms : REF PTreeParams ) ;

(* GenCode
- root :: reference to the starting node of the parse tree
- out_fname :: relative output file path
Raise InvalidFname if unable to write to file because of permissions,
directories not existing, or file already existing.
*)
PROCEDURE GenCode( root : REF Node.T ; style_rules_array : StyleRulesTbl.Default ; out_fname : Pathname.T ) RAISES { InvalidFname , OutError } ;

PROCEDURE GenCodeText( root : REF Node.T ; style_rules_array : StyleRulesTbl.Default ) : TEXT RAISES { InvalidFname , OutError } ;

(* DebugTree
- root :: the starting node of the parse tree
- out_fname :: relative output file path
Write formatted parse tree to a file for debugging purposes.

Raise InvalidFname if unable to write to file because of permissions,
directories not existing, or FILE ALREADY EXISTING. DebugTree refuses
to overwrite files that already exist.

Raise OutError if any issues occur with writing to the output
file.
*)
PROCEDURE DebugTree( root : REF Node.T ; out_fname : Pathname.T ) RAISES { InvalidFname , OutError } ;

(* Get nth proc def with name *)
(* Return NIL if we can't find it *)
(* TODO Exception instead of returning NIL since that forces the user to consider that case? *)
PROCEDURE GetNthProcDef( root : REF Node.T ; ptree_pms : REF PTreeParams ; ProcName : TEXT ; N : CARDINAL ) : REF Node.T ;

END Spec.
