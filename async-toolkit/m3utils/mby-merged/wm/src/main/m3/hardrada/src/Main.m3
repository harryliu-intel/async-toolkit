MODULE Main ;

IMPORT Spec ;
IMPORT Node ;
IMPORT IO ;
IMPORT Fmt ;
IMPORT TextList ;
IMPORT StyleRulesTbl ;
IMPORT NextCharTbl ;
IMPORT DepGraph ;

BEGIN
VAR root := NEW( REF Node.T ) ;
VAR root_for_dep := NEW( REF Node.T ) ;
VAR depgraph := NEW( REF DepGraph.T ) ;
VAR depgraph_pms := NEW( REF DepGraph.DepGraphParams ) ;
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

VAR procdef := NEW( REF Node.T ) ;

VAR look_for_start_sym := NEW( REF Node.DList ) ;

VAR tempchildren := NEW( REF Node.DList ) ;
VAR tempchildren2 := NEW( REF Node.DList ) ;
VAR tempchildren3 := NEW( REF Node.DList ) ;

VAR proc_block := NEW( REF Node.DList ) ;

VAR new_placeholder_list := NEW( REF Node.DList ) ;
VAR new_root_lolz := NEW( REF Node.T ) ;

VAR stmt_add_on := NEW( REF Node.T ) ;
VAR stmt_add_on_take2 := NEW( REF Node.T ) ;
VAR start_ifst_1 := NEW( REF Node.T ) ;
VAR real_start_ifst_1 := NEW( REF Node.T ) ;
VAR real_start_ifst_2 := NEW( REF Node.T ) ;
VAR ass_stmt_add_on := NEW( REF Node.T ) ;
VAR ass_stmt_add_on_take2 := NEW( REF Node.T ) ;
VAR return_stmt := NEW( REF Node.T ) ;
VAR expr_add_on := NEW( REF Node.T ) ;
VAR expr_add_on_take2 := NEW( REF Node.T ) ;
VAR expr_add_on2 := NEW( REF Node.T ) ;
VAR ifst := NEW( REF Node.T ) ;
VAR expr_add_on2_take2 := NEW( REF Node.T ) ;
VAR expr_add_on3 := NEW( REF Node.T ) ;
VAR expr_add_on3_take2 := NEW( REF Node.T ) ;
VAR expr_add_on4 := NEW( REF Node.T ) ;
VAR expr_add_on4_take2 := NEW( REF Node.T ) ;
VAR expr_add_on_if1 := NEW( REF Node.T ) ;
VAR qualid_if1 := NEW( REF Node.T ) ;
VAR id_add_on := NEW( REF Node.T ) ;
VAR id_add_on_take2 := NEW( REF Node.T ) ;
VAR id_add_on2 := NEW( REF Node.T ) ;
VAR id_add_on2_take2 := NEW( REF Node.T ) ;
VAR number_add_on := NEW( REF Node.T ) ;
VAR number_add_on_take2 := NEW( REF Node.T ) ;

VAR ptree_pms := NEW( REF Spec.PTreeParams ) ;
VAR spec_pms := NEW( REF Spec.SpecParams ) ;
VAR style_rules := NEW( StyleRulesTbl.Default ).init( ) ;
VAR nextchar1 := NEW( NextCharTbl.Default ).init( ) ;
VAR nextchar2 := NEW( NextCharTbl.Default ).init( ) ;
VAR nextchar3 := NEW( NextCharTbl.Default ).init( ) ;

VAR four := "4" ;
VAR newline1 := "\n" ;
VAR one := "1" ;
VAR newline2 := "\n" ;
VAR two := "2" ;
VAR newline3 := "\n" ;

VAR declrule5 := "Decl.Rule5" ;
VAR my_S := "S" ;
VAR modulerule1 := "Module.Rule1" ;

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
Node.AppendNode( deeper_block_node^.children , NEW( REF Node.T , val := "END" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;

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
return_stmt := current_node ;
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

(* Initial part of conditional *)
expr_add_on_if1^.val := "Expr.Rule1" ;
expr_add_on_if1^.cat := Node.Category.NonTerminal ;
expr_add_on_if1^.children := NEW( REF Node.DList ) ;
qualid_if1^.val := "QualId.Rule1" ;
qualid_if1^.cat := Node.Category.NonTerminal ;
qualid_if1^.children := NEW( REF Node.DList ) ;
Node.AppendNode( qualid_if1^.children , NEW( REF Node.T , val := "TRUE" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( expr_add_on_if1^.children , qualid_if1 ) ;
start_ifst_1^.val := "IfSt.Rule1" ;
start_ifst_1^.cat := Node.Category.NonTerminal ;
start_ifst_1^.children := NEW( REF Node.DList ) ;
Node.AppendNode( start_ifst_1^.children , NEW( REF Node.T , val := "IF" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( start_ifst_1^.children , expr_add_on_if1 ) ;
Node.AppendNode( start_ifst_1^.children , NEW( REF Node.T , val := "THEN" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
ifst^.val := "Stmt.Rule8" ;
ifst^.cat := Node.Category.NonTerminal ;
ifst^.children := NEW( REF Node.DList ) ;
real_start_ifst_1^.val := "S" ;
real_start_ifst_1^.cat := Node.Category.NonTerminal ;
real_start_ifst_1^.children := NEW( REF Node.DList ) ;
real_start_ifst_2^.val := "S" ;
real_start_ifst_2^.cat := Node.Category.NonTerminal ;
real_start_ifst_2^.children := NEW( REF Node.DList ) ;

(* Part 1 of conditional *)
stmt_add_on^.val := "Stmt.Rule1" ;
stmt_add_on^.cat := Node.Category.NonTerminal ;
stmt_add_on^.children := NEW( REF Node.DList ) ;
Node.AppendNode( real_start_ifst_1^.children , stmt_add_on ) ;
Node.AppendNode( real_start_ifst_1^.children , NEW( REF Node.T , val := ";" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( start_ifst_1^.children , real_start_ifst_1 ) ;
ass_stmt_add_on^.val := "AssignSt.Rule1" ;
ass_stmt_add_on^.cat := Node.Category.NonTerminal ;
ass_stmt_add_on^.children := NEW( REF Node.DList ) ;
expr_add_on^.val := "Expr.Rule1" ;
expr_add_on^.cat := Node.Category.NonTerminal ;
expr_add_on^.children := NEW( REF Node.DList ) ;
id_add_on^.val := "Id.Rule1" ;
id_add_on^.cat := Node.Category.NonTerminal ;
id_add_on^.children := NEW( REF Node.DList ) ;
Node.AppendNode( id_add_on^.children , NEW( REF Node.T , val := "y" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( expr_add_on^.children , id_add_on ) ;
Node.AppendNode( ass_stmt_add_on^.children , expr_add_on ) ;
Node.AppendNode( stmt_add_on^.children , ass_stmt_add_on ) ;
Node.AppendNode( ifst^.children , start_ifst_1 ) ;
Node.AppendNode( deeper_start_node^.children , ifst ) ;
Node.AppendNode( ass_stmt_add_on^.children , NEW( REF Node.T , val := ":=" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
expr_add_on2^.val := "Expr.Rule1" ;
expr_add_on2^.cat := Node.Category.NonTerminal ;
expr_add_on2^.children := NEW( REF Node.DList ) ;
expr_add_on3^.val := "Expr.Rule1" ;
expr_add_on3^.cat := Node.Category.NonTerminal ;
expr_add_on3^.children := NEW( REF Node.DList ) ;
id_add_on2^.val := "Id.Rule1" ;
id_add_on2^.cat := Node.Category.NonTerminal ;
id_add_on2^.children := NEW( REF Node.DList ) ;
Node.AppendNode( id_add_on2^.children , NEW( REF Node.T , val := "x" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( expr_add_on3^.children , id_add_on2 ) ;
Node.AppendNode( expr_add_on2^.children , expr_add_on3 ) ;
Node.AppendNode( expr_add_on2^.children , NEW( REF Node.T , val := "+" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
number_add_on^.val := "Number" ;
number_add_on^.cat := Node.Category.NonTerminal ;
number_add_on^.children := NEW( REF Node.DList ) ;
Node.AppendNode( number_add_on^.children , NEW( REF Node.T , val := "1" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
expr_add_on4^.val := "Expr.Rule1" ;
expr_add_on4^.cat := Node.Category.NonTerminal ;
expr_add_on4^.children := NEW( REF Node.DList ) ;
Node.AppendNode( expr_add_on4^.children , number_add_on ) ;
Node.AppendNode( expr_add_on2^.children , expr_add_on4 ) ;
Node.AppendNode( ass_stmt_add_on^.children , expr_add_on2 ) ;
Node.AppendNode( deeper_start_node^.children , NEW( REF Node.T , val := ";" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;

(* Part 2 of conditional *)
Node.AppendNode( start_ifst_1^.children , NEW( REF Node.T , val := "ELSE" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
stmt_add_on_take2^.val := "Stmt.Rule1" ;
stmt_add_on_take2^.cat := Node.Category.NonTerminal ;
stmt_add_on_take2^.children := NEW( REF Node.DList ) ;
Node.AppendNode( real_start_ifst_2^.children , stmt_add_on_take2 ) ;
Node.AppendNode( real_start_ifst_2^.children , NEW( REF Node.T , val := ";" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( start_ifst_1^.children , real_start_ifst_2 ) ;
Node.AppendNode( start_ifst_1^.children , NEW( REF Node.T , val := "END" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
ass_stmt_add_on_take2^.val := "AssignSt.Rule1" ;
ass_stmt_add_on_take2^.cat := Node.Category.NonTerminal ;
ass_stmt_add_on_take2^.children := NEW( REF Node.DList ) ;
expr_add_on_take2^.val := "Expr.Rule1" ;
expr_add_on_take2^.cat := Node.Category.NonTerminal ;
expr_add_on_take2^.children := NEW( REF Node.DList ) ;
id_add_on_take2^.val := "Id.Rule1" ;
id_add_on_take2^.cat := Node.Category.NonTerminal ;
id_add_on_take2^.children := NEW( REF Node.DList ) ;
Node.AppendNode( id_add_on_take2^.children , NEW( REF Node.T , val := "y" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( expr_add_on_take2^.children , id_add_on_take2 ) ;
Node.AppendNode( ass_stmt_add_on_take2^.children , expr_add_on_take2 ) ;
Node.AppendNode( stmt_add_on_take2^.children , ass_stmt_add_on_take2 ) ;
Node.AppendNode( ass_stmt_add_on_take2^.children , NEW( REF Node.T , val := ":=" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
expr_add_on2_take2^.val := "Expr.Rule1" ;
expr_add_on2_take2^.cat := Node.Category.NonTerminal ;
expr_add_on2_take2^.children := NEW( REF Node.DList ) ;
expr_add_on3_take2^.val := "Expr.Rule1" ;
expr_add_on3_take2^.cat := Node.Category.NonTerminal ;
expr_add_on3_take2^.children := NEW( REF Node.DList ) ;
id_add_on2_take2^.val := "Id.Rule1" ;
id_add_on2_take2^.cat := Node.Category.NonTerminal ;
id_add_on2_take2^.children := NEW( REF Node.DList ) ;
Node.AppendNode( id_add_on2_take2^.children , NEW( REF Node.T , val := "x" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( expr_add_on3_take2^.children , id_add_on2_take2 ) ;
Node.AppendNode( expr_add_on2_take2^.children , expr_add_on3_take2 ) ;
Node.AppendNode( expr_add_on2_take2^.children , NEW( REF Node.T , val := "+" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
number_add_on_take2^.val := "Number" ;
number_add_on_take2^.cat := Node.Category.NonTerminal ;
number_add_on_take2^.children := NEW( REF Node.DList ) ;
Node.AppendNode( number_add_on_take2^.children , NEW( REF Node.T , val := "2" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
expr_add_on4_take2^.val := "Expr.Rule1" ;
expr_add_on4_take2^.cat := Node.Category.NonTerminal ;
expr_add_on4_take2^.children := NEW( REF Node.DList ) ;
Node.AppendNode( expr_add_on4_take2^.children , number_add_on_take2 ) ;
Node.AppendNode( expr_add_on2_take2^.children , expr_add_on4_take2 ) ;
Node.AppendNode( ass_stmt_add_on_take2^.children , expr_add_on2_take2 ) ;

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

(* TODO At some point you need error checking. What if I put in a
grammar rule that doesn't exist *)
EVAL nextchar1.put( four , newline1 ) ;
EVAL style_rules.put( declrule5 , nextchar1 ) ;
EVAL nextchar2.put( one , newline2 ) ;
EVAL style_rules.put( my_S , nextchar2 ) ;
EVAL nextchar3.put( two , newline3 ) ;
EVAL style_rules.put( modulerule1 , nextchar3 ) ;

TRY
	Spec.DebugTree( root , "./test" ) ;
EXCEPT
	| Spec.InvalidFname => IO.Put( "Can't use that fname.\n" ) ;
	| Spec.OutError => IO.Put( "Outerror.\n" ) ;
END ;

TRY
	Spec.GenCode( root , style_rules , "./test.m3" ) ;
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
ptree_pms^.PathToArgNameFromArg := TextList.Cons( "IdList.Rule1" , TextList.Cons( "Id.Rule1" , NIL ) ) ;

(* Testing dep graph *)
depgraph_pms^.start_symbol_val := "S" ;
depgraph_pms^.separator := ";" ;
depgraph_pms^.ProcedureBodyToSSPath := TextList.Cons( "S" , NIL ) ;
(* Get procedure block *)
procdef := Spec.GetNthProcDef( root , ptree_pms , "sgn" , 0 ) ;
Node.FollowPath( proc_block , procdef , ptree_pms^.PathToProcedureBlock ) ;
<* ASSERT Node.Length( proc_block ) = 1 *>
TRY
	Spec.DebugTree( proc_block^.cur , "./depgraph_pretest_b4placeholder.m3" ) ;
EXCEPT
	| Spec.InvalidFname => IO.Put( "Can't use that fname.\n" ) ;
	| Spec.OutError => IO.Put( "Outerror.\n" ) ;
END ;
DepGraph.PutPlaceholderInProcBlock( root_for_dep , proc_block^.cur , depgraph_pms ) ;
TRY
	Spec.DebugTree( proc_block^.cur , "./depgraph_pretest_afterplaceholder.m3" ) ;
EXCEPT
	| Spec.InvalidFname => IO.Put( "Can't use that fname.\n" ) ;
	| Spec.OutError => IO.Put( "Outerror.\n" ) ;
END ;
Node.FollowPath( look_for_start_sym , proc_block^.cur , depgraph_pms^.ProcedureBodyToSSPath ) ;
<* ASSERT look_for_start_sym^.cur # NIL *>
IF look_for_start_sym^.cur^.cat = Node.Category.Placeholder THEN
	IO.Put( "Good. When you find the start symbol for the proc body, it is replaced with a placeholder.\n" ) ;
ELSE
	IO.Put( "Ugh. When you find the start symbol for the proc body, it is NOT replaced with a placeholder.\n" ) ;
END ;
DepGraph.GetDepGraph( depgraph , root_for_dep , depgraph_pms ) ;
TRY
	Spec.DebugTree( depgraph^.parse_root , "./depgraph_pretest.m3" ) ;
EXCEPT
	| Spec.InvalidFname => IO.Put( "Can't use that fname.\n" ) ;
	| Spec.OutError => IO.Put( "Outerror.\n" ) ;
END ;
(*
TRY
	Spec.DebugTree( depgraph^.next^.parse_root , "./depgraph_pretest_next.m3" ) ;
EXCEPT
	| Spec.InvalidFname => IO.Put( "Can't use that fname.\n" ) ;
	| Spec.OutError => IO.Put( "Outerror.\n" ) ;
END ;
*)
DepGraph.ConstructParseTree( proc_block^.cur , depgraph , depgraph_pms ) ;
TRY
	Spec.GenCode( root_for_dep , style_rules , "./depgraph.m3" ) ;
EXCEPT
	| Spec.InvalidFname => IO.Put( "Can't use that fname.\n" ) ;
	| Spec.OutError => IO.Put( "Outerror.\n" ) ;
END ;
(* Done Testing dep graph *)

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

TRY
	Spec.GenCode( root , style_rules , "./test_out.m3" ) ;
EXCEPT
	| Spec.InvalidFname => IO.Put( "Invalid output fname\n" ) ;
	| Spec.OutError => IO.Put( "Problem writing to outfile\n" ) ;
END ;

END ;

END Main .
