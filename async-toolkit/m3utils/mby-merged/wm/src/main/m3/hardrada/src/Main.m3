MODULE Main ;

IMPORT Spec ;
IMPORT Node ;
IMPORT IO ;
IMPORT Fmt ;
IMPORT TextList ;

BEGIN
VAR root := NEW( REF Node.T ) ;
VAR inline_root := NEW( REF Node.T ) ;
VAR current_node := NEW( REF Node.T ) ;
VAR current_node2 := NEW( REF Node.T ) ;
VAR current_node3 := NEW( REF Node.T ) ;
VAR current_node4 := NEW( REF Node.T ) ;
VAR current_node5 := NEW( REF Node.T ) ;
VAR current_node6 := NEW( REF Node.T ) ;
VAR block_node := NEW( REF Node.T ) ;
VAR decl_node := NEW( REF Node.T ) ;
VAR start_node := NEW( REF Node.T ) ;
VAR proc_head := NEW( REF Node.T ) ;
VAR deeper_block_node := NEW( REF Node.T ) ;
VAR signature_node := NEW( REF Node.T ) ;
VAR deeper_start_node := NEW( REF Node.T ) ;

VAR ptree_pms := NEW( REF Spec.PTreeParams ) ;
VAR spec_pms := NEW( REF Spec.SpecParams ) ;

BEGIN

(* Program *)

root^.val := "Module.Rule1" ;
root^.cat := Node.Category.NonTerminal ;
root^.children := NEW( REF Node.DList ) ;

current_node^.val := "MODULE" ;
current_node^.cat := Node.Category.Constant ;
current_node^.children := NEW( REF Node.DList ) ;

Node.AppendNode( root^.children , current_node ) ;

current_node := NEW( REF Node.T ) ;
current_node^.val := "Id.Rule1" ;
current_node^.cat := Node.Category.NonTerminal ;
current_node^.children := NEW( REF Node.DList ) ;

Node.AppendNode( current_node^.children , NEW( REF Node.T , val := "Main" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( root^.children , current_node ) ;
Node.AppendNode( root^.children , NEW( REF Node.T , val := ";" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;

block_node^.val := "Block.Rule1" ;
block_node^.cat := Node.Category.NonTerminal ;
block_node^.children := NEW( REF Node.DList ) ;
Node.AppendNode( root^.children , block_node ) ;

current_node := NEW( REF Node.T ) ;
current_node^.val := "Id.Rule1" ;
current_node^.cat := Node.Category.NonTerminal ;
current_node^.children := NEW( REF Node.DList ) ;

Node.AppendNode( current_node^.children , NEW( REF Node.T , val := "Main" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( root^.children , current_node ) ;
Node.AppendNode( root^.children , NEW( REF Node.T , val := "." , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;

decl_node^.val := "Decl.Rule5" ;
decl_node^.cat := Node.Category.NonTerminal ;
decl_node^.children := NEW( REF Node.DList ) ;
Node.AppendNode( block_node^.children , decl_node ) ;
Node.AppendNode( block_node^.children , NEW( REF Node.T , val := "BEGIN" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;

start_node^.val := "S" ;
start_node^.cat := Node.Category.NonTerminal ;
start_node^.children := NEW( REF Node.DList ) ;
Node.AppendNode( block_node^.children , start_node ) ;
Node.AppendNode( block_node^.children , NEW( REF Node.T , val := "END" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;

proc_head^.val := "ProcedureHead.Rule1" ;
proc_head^.cat := Node.Category.NonTerminal ;
proc_head^.children := NEW( REF Node.DList ) ;
Node.AppendNode( decl_node^.children , proc_head ) ;
Node.AppendNode( decl_node^.children , NEW( REF Node.T , val := "=" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;

deeper_block_node^.val := "Block.Rule1" ;
deeper_block_node^.cat := Node.Category.NonTerminal ;
deeper_block_node^.children := NEW( REF Node.DList ) ;
Node.AppendNode( decl_node^.children , deeper_block_node ) ;

current_node := NEW( REF Node.T ) ;
current_node^.val := "Id.Rule1" ;
current_node^.cat := Node.Category.NonTerminal ;
current_node^.children := NEW( REF Node.DList ) ;
Node.AppendNode( current_node^.children , NEW( REF Node.T , val := "sgn" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( decl_node^.children , current_node ) ;
Node.AppendNode( decl_node^.children , NEW( REF Node.T , val := ";" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;

Node.AppendNode( proc_head^.children , NEW( REF Node.T , val := "PROCEDURE" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
current_node := NEW( REF Node.T ) ;
current_node^.val := "Id.Rule1" ;
current_node^.cat := Node.Category.NonTerminal ;
current_node^.children := NEW( REF Node.DList ) ;
Node.AppendNode( current_node^.children , NEW( REF Node.T , val := "sgn" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( proc_head^.children , current_node ) ;

signature_node^.val := "Signature.Rule1" ;
signature_node^.cat := Node.Category.NonTerminal ;
signature_node^.children := NEW( REF Node.DList ) ;
Node.AppendNode( proc_head^.children , signature_node ) ;

Node.AppendNode( deeper_block_node^.children , NEW( REF Node.T , val := "BEGIN" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;
deeper_start_node := NEW( REF Node.T ) ;
deeper_start_node^.val := "S" ;
deeper_start_node^.cat := Node.Category.NonTerminal ;
deeper_start_node^.children := NEW( REF Node.DList ) ;
Node.AppendNode( deeper_block_node^.children , deeper_start_node ) ;
Node.AppendNode( deeper_block_node^.children , NEW( REF Node.T , val := "END" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;

current_node := NEW( REF Node.T ) ;
current_node^.val := "Stmt.Rule6" ;
current_node^.cat := Node.Category.NonTerminal ;
current_node^.children := NEW( REF Node.DList ) ;
Node.AppendNode( start_node^.children , current_node ) ;
Node.AppendNode( start_node^.children , NEW( REF Node.T , val := ";" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;

current_node2^.val := "EvalSt.Rule1" ;
current_node2^.cat := Node.Category.NonTerminal ;
current_node2^.children := NEW( REF Node.DList ) ;
Node.AppendNode( current_node^.children , current_node2 ) ;
Node.AppendNode( current_node2^.children , NEW( REF Node.T , val := "EVAL" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;

current_node := NEW( REF Node.T ) ;
current_node^.val := "Expr.Rule1" ;
current_node^.cat := Node.Category.NonTerminal ;
current_node^.children := NEW( REF Node.DList ) ;
Node.AppendNode( current_node2^.children , current_node ) ;

current_node2 := NEW( REF Node.T ) ;
current_node2^.val := "CallSt.Rule2" ;
current_node2^.cat := Node.Category.NonTerminal ;
current_node2^.children := NEW( REF Node.DList ) ;
Node.AppendNode( current_node^.children , current_node2 ) ;

current_node := NEW( REF Node.T ) ;
current_node^.val := "Expr.Rule1" ;
current_node^.cat := Node.Category.NonTerminal ;
current_node^.children := NEW( REF Node.DList ) ;

current_node3^.val := "Id.Rule1" ;
current_node3^.cat := Node.Category.NonTerminal ;
current_node3^.children := NEW( REF Node.DList ) ;
Node.AppendNode( current_node3^.children , NEW( REF Node.T , val := "sgn" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( current_node^.children , current_node3 ) ;
Node.AppendNode( current_node2^.children , current_node ) ;

Node.AppendNode( current_node2^.children , NEW( REF Node.T , val := "(" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;

current_node := NEW( REF Node.T ) ;
current_node^.val := "Actual.Rule1" ;
current_node^.cat := Node.Category.NonTerminal ;
current_node^.children := NEW( REF Node.DList ) ;
current_node3 := NEW( REF Node.T ) ;
current_node3^.val := "Number.Rule1" ;
current_node3^.cat := Node.Category.NonTerminal ;
current_node3^.children := NEW( REF Node.DList ) ;
Node.AppendNode( current_node^.children , current_node3 ) ;
Node.AppendNode( current_node3^.children , NEW( REF Node.T , val := "8" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( current_node2^.children , current_node ) ;
Node.AppendNode( current_node2^.children , NEW( REF Node.T , val := ")" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;

current_node := NEW( REF Node.T ) ;
current_node^.val := "Stmt.Rule13" ;
current_node^.cat := Node.Category.NonTerminal ;
current_node^.children := NEW( REF Node.DList ) ;
current_node2 := NEW( REF Node.T ) ;
current_node2^.val := "ReturnSt.Rule1" ;
current_node2^.cat := Node.Category.NonTerminal ;
current_node2^.children := NEW( REF Node.DList ) ;
Node.AppendNode( current_node2^.children , NEW( REF Node.T , val := "RETURN" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
current_node3 := NEW( REF Node.T ) ;
current_node3^.val := "Expr.Rule1" ;
current_node3^.cat := Node.Category.NonTerminal ;
current_node3^.children := NEW( REF Node.DList ) ;
current_node4^.val := "Id.Rule1" ;
current_node4^.cat := Node.Category.NonTerminal ;
current_node4^.children := NEW( REF Node.DList ) ;
Node.AppendNode( current_node4^.children , NEW( REF Node.T , val := "x" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( current_node3^.children , current_node4 ) ;
Node.AppendNode( current_node2^.children , current_node3 ) ;
Node.AppendNode( current_node^.children , current_node2 ) ;
Node.AppendNode( deeper_start_node^.children , current_node ) ;
Node.AppendNode( deeper_start_node^.children , NEW( REF Node.T , val := ";" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;

Node.AppendNode( signature_node^.children , NEW( REF Node.T , val := "(" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
current_node := NEW( REF Node.T ) ;
current_node^.val := "Formals.Rule1" ;
current_node^.cat := Node.Category.NonTerminal ;
current_node^.children := NEW( REF Node.DList ) ;
current_node2 := NEW( REF Node.T ) ;
current_node2^.val := "Formal.Rule1" ;
current_node2^.cat := Node.Category.NonTerminal ;
current_node2^.children := NEW( REF Node.DList ) ;
current_node3 := NEW( REF Node.T ) ;
current_node3^.val := "IdList.Rule1" ;
current_node3^.cat := Node.Category.NonTerminal ;
current_node3^.children := NEW( REF Node.DList ) ;
Node.AppendNode( current_node2^.children , current_node3 ) ;
current_node4 := NEW( REF Node.T ) ;
current_node4^.val := "Id.Rule1" ;
current_node4^.cat := Node.Category.NonTerminal ;
current_node4^.children := NEW( REF Node.DList ) ;
Node.AppendNode( current_node4^.children , NEW( REF Node.T , val := "x" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( current_node3^.children , current_node4 ) ;
Node.AppendNode( current_node2^.children , NEW( REF Node.T , val := ":" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( current_node^.children , current_node2 ) ;
Node.AppendNode( signature_node^.children , current_node ) ;
Node.AppendNode( signature_node^.children , NEW( REF Node.T , val := ")" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( signature_node^.children , NEW( REF Node.T , val := ":" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;

current_node3 := NEW( REF Node.T ) ;
current_node3^.val := "Type.Rule1" ;
current_node3^.cat := Node.Category.NonTerminal ;
current_node3^.children := NEW( REF Node.DList ) ;
current_node4 := NEW( REF Node.T ) ;
current_node4^.val := "TypeName.Rule1" ;
current_node4^.cat := Node.Category.NonTerminal ;
current_node4^.children := NEW( REF Node.DList ) ;
Node.AppendNode( current_node3^.children , current_node4 ) ;
current_node5 := NEW( REF Node.T ) ;
current_node5^.val := "QualId.Rule1" ;
current_node5^.cat := Node.Category.NonTerminal ;
current_node5^.children := NEW( REF Node.DList ) ;
Node.AppendNode( current_node4^.children , current_node5 ) ;
current_node6 := NEW( REF Node.T ) ;
current_node6^.val := "Id.Rule1" ;
current_node6^.cat := Node.Category.NonTerminal ;
current_node6^.children := NEW( REF Node.DList ) ;
Node.AppendNode( current_node5^.children , current_node6 ) ;
Node.AppendNode( current_node6^.children , NEW( REF Node.T , val := "INTEGER" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( current_node2^.children , current_node3 ) ;

current_node := NEW( REF Node.T ) ;
current_node^.val := "Type.Rule1" ;
current_node^.cat := Node.Category.NonTerminal ;
current_node^.children := NEW( REF Node.DList ) ;
Node.AppendNode( signature_node^.children , current_node ) ;
current_node2 := NEW( REF Node.T ) ;
current_node2^.val := "TypeName.Rule1" ;
current_node2^.cat := Node.Category.NonTerminal ;
current_node2^.children := NEW( REF Node.DList ) ;
Node.AppendNode( current_node^.children , current_node2 ) ;
current_node3 := NEW( REF Node.T ) ;
current_node3^.val := "QualId.Rule1" ;
current_node3^.cat := Node.Category.NonTerminal ;
current_node3^.children := NEW( REF Node.DList ) ;
Node.AppendNode( current_node2^.children , current_node3 ) ;
current_node4 := NEW( REF Node.T ) ;
current_node4^.val := "Id.Rule1" ;
current_node4^.cat := Node.Category.NonTerminal ;
current_node4^.children := NEW( REF Node.DList ) ;
Node.AppendNode( current_node3^.children , current_node4 ) ;
Node.AppendNode( current_node4^.children , NEW( REF Node.T , val := "INTEGER" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;

(* Inline *)
inline_root^.val := "Decl.Rule1" ;
inline_root^.cat := Node.Category.NonTerminal ;
inline_root^.children := NEW( REF Node.DList ) ;
Node.AppendNode( inline_root^.children , NEW( REF Node.T , val := "CONST" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
current_node := NEW( REF Node.T ) ;
current_node^.val := "Id.Rule1" ;
current_node^.cat := Node.Category.NonTerminal ;
current_node^.children := NEW( REF Node.DList ) ;
Node.AppendNode( current_node^.children , NEW( REF Node.T , val := "x" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( inline_root^.children , current_node ) ;
Node.AppendNode( inline_root^.children , NEW( REF Node.T , val := "=" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
current_node := NEW( REF Node.T ) ;
current_node^.val := "Expr.Rule1" ;
current_node^.cat := Node.Category.NonTerminal ;
current_node^.children := NEW( REF Node.DList ) ;
Node.AppendNode( inline_root^.children , current_node ) ;
current_node2 := NEW( REF Node.T ) ;
current_node2^.val := "Number.Rule2" ;
current_node2^.cat := Node.Category.NonTerminal ;
current_node2^.children := NEW( REF Node.DList ) ;
Node.AppendNode( current_node2^.children , NEW( REF Node.T , val := "8" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ; 
Node.AppendNode( current_node^.children , current_node2 ) ;
Node.AppendNode( inline_root^.children , NEW( REF Node.T , val := ";" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;

TRY
	Spec.DebugTree( root , "./test" ) ;
EXCEPT
	| Spec.InvalidFname => IO.Put( "Can't use that fname.\n" ) ;
	| Spec.OutError => IO.Put( "Outerror.\n" ) ;
END ;

TRY
	Spec.DebugTree( inline_root , "./test_inline" ) ;
EXCEPT
	| Spec.InvalidFname => IO.Put( "Can't use that fname.\n" ) ;
	| Spec.OutError => IO.Put( "Outerror.\n" ) ;
END ;

ptree_pms^.ProcedureDefnVal := "Decl.Rule5" ;
ptree_pms^.ArgSeparator := ";" ;
ptree_pms^.PathToProcedureBlock := TextList.Cons( "Block.Rule1" , NIL ) ;
ptree_pms^.PathToProcedureName := TextList.Cons( "ProcedureHead.Rule1" , TextList.Cons( "Id.Rule1" , NIL ) ) ;
ptree_pms^.PathToArgList := TextList.Cons( "ProcedureHead.Rule1" , TextList.Cons( "Signature.Rule1" , TextList.Cons( "Formals.Rule1" , NIL ) ) ) ;
ptree_pms^.PathToArgFromArgList := TextList.Cons( "Formal.Rule1" , NIL ) ;
ptree_pms^.PathToArgNameFromArg := TextList.Cons( "IdList.Rule1" , TextList.Cons( "Id.Rule1" , NIL ) ) ;

spec_pms^.specblock := inline_root ;
spec_pms^.static_args := TextList.Cons( "x" , NIL ) ;
spec_pms^.procname := "sgn" ;
spec_pms^.procdefnumber := 0 ;

Spec.Specialize( root , spec_pms , ptree_pms ) ;

TRY
	Spec.DebugTree( root , "./test_spec" ) ;
EXCEPT
	| Spec.InvalidFname => IO.Put( "Can't use that fname.\n" ) ;
	| Spec.OutError => IO.Put( "Outerror.\n" ) ;
END ;

END ;

END Main .
