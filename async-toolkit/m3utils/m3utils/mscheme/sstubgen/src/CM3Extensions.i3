(* $Id$ *)

INTERFACE CM3Extensions;
IMPORT RefRefTbl;
FROM AstToType IMPORT Handle;
IMPORT Type, M3AST_AS;

PROCEDURE InitAstTable(astTable : RefRefTbl.T);

PROCEDURE ProcessTypeSpec(h: Handle; ts: M3AST_AS.TYPE_SPEC) : Type.T;

END CM3Extensions.
