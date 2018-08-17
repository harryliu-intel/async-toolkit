INTERFACE Spec ;

(***********)
(* Imports *)
(***********)
IMPORT Node ;
IMPORT Pathname ;
IMPORT TextList ;

(**************)
(* Exceptions *)
(**************)
EXCEPTION InvalidFname ;
EXCEPTION OutError ;

(*********)
(* Types *)
(*********)

TYPE PTreeParams = RECORD
	ProcedureDefnVal : TEXT := "" ;
	ArgSeparator : TEXT := "" ;
	PathToProcedureBlock : TextList.T := NIL ;
	PathToProcedureName : TextList.T := NIL ;
	PathToArgList : TextList.T := NIL ;
	PathToArgNameFromArgList : TextList.T := NIL ;
END ;

TYPE SpecParams = RECORD
	specblock : REF Node.T := NIL ;
	static_args : TextList.T := NIL ;
	procname : TEXT := "" ;
	procdefnumber : CARDINAL := 0 ;
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
(* PROCEDURE GenCode( root : REF Node.T ; out_fname : Pathname.T ) RAISES { InvalidFname } ; *)

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

END Spec.
