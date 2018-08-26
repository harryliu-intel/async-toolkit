MODULE Main ;

IMPORT Spec ;
IMPORT Node ;
IMPORT IO ;
IMPORT Fmt ;
IMPORT TextList ;
IMPORT StyleRulesTbl ;
IMPORT NextCharTbl ;
IMPORT DepGraph ;
IMPORT Text ;
IMPORT REFANYList ;
IMPORT CARDINALList ;

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

VAR exprrule1_assign := NEW( REF Node.T ) ;
VAR exprrule1_assign2 := NEW( REF Node.T ) ;
VAR idrule1 := NEW( REF Node.T ) ;
VAR idrule1_2 := NEW( REF Node.T ) ;
VAR assign_stmt := NEW( REF Node.T ) ;

VAR depgraph_stmt1_subdepgraph1 := NEW( REF DepGraph.T ) ;
VAR depgraph_stmt1_subdepgraph2 := NEW( REF DepGraph.T ) ;

VAR nextdeps : REFANY := NIL ;
VAR sdg1 : REFANY := NIL ;
VAR sdg2 : REFANY := NIL ;
VAR refany_depgraph : REFANYList.T := NIL ;
VAR refany_depgraph_2 : REFANYList.T := NIL ;
VAR sdg_refany_list : REFANYList.T := NIL ;

VAR procdef := NEW( REF Node.T ) ;

VAR ugh_another_temp := NEW( REF DepGraph.T ) ;

VAR look_for_start_sym := NEW( REF Node.DList ) ;

VAR tempchildren := NEW( REF Node.DList ) ;
VAR tempchildren2 := NEW( REF Node.DList ) ;
VAR tempchildren3 := NEW( REF Node.DList ) ;

VAR depgraph_stmt1 := NEW( REF DepGraph.T ) ;
VAR depgraph_stmt1_refany := depgraph_stmt1 ;
VAR depgraph_next_next : REFANY := NIL ;

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

VAR declrule4 := NEW( REF Node.T ) ;
VAR variabledecl := NEW( REF Node.T ) ;
VAR idlistrule1 := NEW( REF Node.T ) ;
VAR id_idlistrule1 := NEW( REF Node.T ) ;
VAR typenamerule1 := NEW( REF Node.T ) ;
VAR qualidrule1 := NEW( REF Node.T ) ;
VAR my_my_idrule1 := NEW( REF Node.T ) ;
VAR my_constexpr := NEW( REF Node.T ) ;
VAR my_number := NEW( REF Node.T ) ;
VAR my_number2 := NEW( REF Node.T ) ;

VAR typerule1 := NEW( REF Node.T ) ;

VAR stmtrule1 := NEW( REF Node.T ) ;
VAR exprrule1 := NEW( REF Node.T ) ;
VAR another_exprrule1 := NEW( REF Node.T ) ;
VAR my_my_my_idrule1 := NEW( REF Node.T ) ;
VAR my_selector := NEW( REF Node.T ) ;
VAR my_selector2 := NEW( REF Node.T ) ;
VAR my_selector3 := NEW( REF Node.T ) ;
VAR my_selector4 := NEW( REF Node.T ) ;

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

(*Append decl here*)
declrule4^.val := "Decl.Rule4" ;
declrule4^.cat := Node.Category.NonTerminal ;
declrule4^.children := NEW( REF Node.DList ) ;
declrule4^.children^.cur := NEW( REF Node.T , val := "VAR" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ;
(* Variable decl *)
variabledecl^.val := "VariableDecl.Rule1" ;
variabledecl^.cat := Node.Category.NonTerminal ;
variabledecl^.children := NEW( REF Node.DList ) ;
idlistrule1^.val := "IdList.Rule1" ;
idlistrule1^.cat := Node.Category.NonTerminal ;
idlistrule1^.children := NEW( REF Node.DList ) ;
id_idlistrule1 := NEW( REF Node.T , val := "y" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ;
Node.AppendNode( idlistrule1^.children , id_idlistrule1 ) ;
Node.AppendNode( id_idlistrule1^.children , NEW( REF Node.T , val := "y" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( variabledecl^.children , idlistrule1 ) ;
Node.AppendNode( variabledecl^.children , NEW( REF Node.T , val := ":" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
(* Type.Rule1 *)
typerule1^.val := "Type.Rule1" ;
typerule1^.cat := Node.Category.NonTerminal ;
typerule1^.children := NEW( REF Node.DList ) ;
typenamerule1^.val := "TypeName.Rule1" ;
typenamerule1^.cat := Node.Category.NonTerminal ;
typenamerule1^.children := NEW( REF Node.DList ) ;
Node.AppendNode( typerule1^.children , typenamerule1 ) ;
qualidrule1^.val := "QualId.Rule1" ;
qualidrule1^.cat := Node.Category.NonTerminal ;
qualidrule1^.children := NEW( REF Node.DList ) ;
Node.AppendNode( typenamerule1^.children , qualidrule1 ) ;
my_my_idrule1^.val := "Id.Rule1" ;
my_my_idrule1^.cat := Node.Category.NonTerminal ;
my_my_idrule1^.children := NEW( REF Node.DList ) ;
Node.AppendNode( qualidrule1^.children , my_my_idrule1 ) ;
Node.AppendNode( my_my_idrule1^.children , NEW( REF Node.T , val := "INTEGER" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( variabledecl^.children , typerule1 ) ;
(* Type.Rule1 *)
Node.AppendNode( variabledecl^.children , NEW( REF Node.T , val := ":=" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
(* ConstExpr *)
my_constexpr := NEW( REF Node.T , val := "ConstExpr" , cat := Node.Category.NonTerminal , children := NEW( REF Node.DList ) ) ;
my_number := NEW( REF Node.T , val := "Number" , cat := Node.Category.NonTerminal , children := NEW( REF Node.DList ) ) ;
Node.AppendNode( variabledecl^.children , my_constexpr ) ;
Node.AppendNode( my_constexpr^.children , my_number ) ;
Node.AppendNode( my_number^.children , NEW( REF Node.T , val := "0" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
(* Variable decl *)
Node.AppendNode( declrule4^.children , variabledecl ) ;
Node.AppendNode( declrule4^.children , NEW( REF Node.T , val := ";" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( deeper_block_node^.children , declrule4 ) ;
(*Append decl here*)

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
Node.AppendNode( current_node4^.children , NEW( REF Node.T , val := "y" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;
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
(* Stmt.Rule1 *)
stmtrule1^.val := "Stmt.Rule1" ;
stmtrule1^.cat := Node.Category.NonTerminal ;
stmtrule1^.children := NEW( REF Node.DList ) ;
Node.AppendNode( deeper_start_node^.children , stmtrule1 ) ;
exprrule1^.val := "Expr.Rule1" ;
exprrule1^.cat := Node.Category.NonTerminal ;
exprrule1^.children := NEW( REF Node.DList ) ;
Node.AppendNode( stmtrule1^.children , exprrule1 ) ;
my_my_my_idrule1^.val := "Id.Rule1" ;
my_my_my_idrule1^.cat := Node.Category.NonTerminal ;
my_my_my_idrule1^.children := NEW( REF Node.DList ) ;
Node.AppendNode( my_my_my_idrule1^.children , NEW( REF Node.T , val := "x" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( exprrule1^.children , my_my_my_idrule1 ) ;
my_selector^.val := "Selector" ;
my_selector^.cat := Node.Category.NonTerminal ;
my_selector^.children := NEW( REF Node.DList ) ;
Node.AppendNode( my_selector^.children , NEW( REF Node.T , val := "^" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( exprrule1^.children , my_selector ) ;
Node.AppendNode( stmtrule1^.children , NEW( REF Node.T , val := ":=" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
another_exprrule1^.val := "Expr.Rule1" ;
another_exprrule1^.cat := Node.Category.NonTerminal ;
another_exprrule1^.children := NEW( REF Node.DList ) ;
Node.AppendNode( stmtrule1^.children , another_exprrule1 ) ;
my_number2^.val := "Number" ;
my_number2^.cat := Node.Category.NonTerminal ;
my_number2^.children := NEW( REF Node.DList ) ;
Node.AppendNode( another_exprrule1^.children , my_number2 ) ;
Node.AppendNode( my_number2^.children , NEW( REF Node.T , val := "3" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
(* Stmt.Rule1 *)
Node.AppendNode( deeper_start_node^.children , NEW( REF Node.T , val := ";" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
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
my_selector3^.val := "Selector" ;
my_selector3^.cat := Node.Category.NonTerminal ;
my_selector3^.children := NEW( REF Node.DList ) ;
Node.AppendNode( my_selector3^.children , NEW( REF Node.T , val := "^" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( expr_add_on2^.children , expr_add_on3 ) ;
Node.AppendNode( expr_add_on3^.children , my_selector3 ) ;
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
my_selector4^.val := "Selector" ;
my_selector4^.cat := Node.Category.NonTerminal ;
my_selector4^.children := NEW( REF Node.DList ) ;
Node.AppendNode( my_selector4^.children , NEW( REF Node.T , val := "^" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( expr_add_on3_take2^.children , id_add_on2_take2 ) ;
Node.AppendNode( expr_add_on3_take2^.children , my_selector4 ) ;
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
Node.AppendNode( current_node6^.children , NEW( REF Node.T , val := "REF INTEGER" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
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
IO.Put( "Finding procedure def...\n" ) ;
procdef := Spec.GetNthProcDef( root , ptree_pms , "sgn" , 0 ) ;
IO.Put( "Found procedure def...\n" ) ;
IO.Put( "Finding procedure block...\n" ) ;
Node.FollowPath( proc_block , procdef , ptree_pms^.PathToProcedureBlock ) ;
IO.Put( "Found procedure block...\n" ) ;
<* ASSERT Node.Length( proc_block ) = 1 *>
TRY
	Spec.DebugTree( proc_block^.cur , "./depgraph_pretest_b4placeholder.m3" ) ;
EXCEPT
	| Spec.InvalidFname => IO.Put( "Can't use that fname.\n" ) ;
	| Spec.OutError => IO.Put( "Outerror.\n" ) ;
END ;
IO.Put( "Putting placeholders in procedure block...\n" ) ;
DepGraph.PutPlaceholderInProcBlock( root_for_dep , proc_block^.cur , depgraph_pms ) ;
IO.Put( "Successfully put placeholders in procedure block...\n" ) ;
TRY
	Spec.DebugTree( proc_block^.cur , "./depgraph_pretest_afterplaceholder.m3" ) ;
EXCEPT
	| Spec.InvalidFname => IO.Put( "Can't use that fname.\n" ) ;
	| Spec.OutError => IO.Put( "Outerror.\n" ) ;
END ;
Node.FollowPath( look_for_start_sym , proc_block^.cur , depgraph_pms^.ProcedureBodyToSSPath ) ;
<* ASSERT look_for_start_sym^.cur # NIL *>
IO.Put( "Number of placeholders in procedure block (should only be 1): " & Fmt.Int( Node.Length( look_for_start_sym ) ) & "\n" ) ;
IO.Put( "Getting dependency graph...\n" ) ;
DepGraph.GetDepGraph( depgraph , root_for_dep , depgraph_pms ) ;
(* Modify the depgraph *)
depgraph_stmt1^.next := depgraph ;
depgraph_stmt1^.is_static := TRUE ;
depgraph_stmt1^.deps := NIL ;
depgraph_stmt1^.assigned_vars := TextList.Cons( "y" , NIL ) ;
depgraph_stmt1^.subdepgraph := NIL ;
depgraph_stmt1^.parse_root := NEW( REF Node.T , val := "Stmt.Rule1" , cat := Node.Category.NonTerminal , children := NEW( REF Node.DList ) ) ;
assign_stmt := NEW( REF Node.T , val := "AssignSt.Rule1" , cat := Node.Category.NonTerminal , children := NEW( REF Node.DList ) ) ;
Node.AppendNode( depgraph_stmt1^.parse_root^.children , assign_stmt ) ;
exprrule1_assign := NEW( REF Node.T , val := "Expr.Rule1" , cat := Node.Category.NonTerminal , children := NEW( REF Node.DList ) ) ;
exprrule1_assign2 := NEW( REF Node.T , val := "Expr.Rule1" , cat := Node.Category.NonTerminal , children := NEW( REF Node.DList ) ) ;
idrule1 := NEW( REF Node.T , val := "Id.Rule1" , cat := Node.Category.NonTerminal , children := NEW( REF Node.DList ) ) ;
idrule1_2 := NEW( REF Node.T , val := "Id.Rule1" , cat := Node.Category.NonTerminal , children := NEW( REF Node.DList ) ) ;
Node.AppendNode( idrule1^.children , NEW( REF Node.T , val := "y" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( exprrule1_assign^.children , idrule1 ) ;
Node.AppendNode( idrule1_2^.children , NEW( REF Node.T , val := "0" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( exprrule1_assign2^.children , idrule1_2 ) ;
Node.AppendNode( depgraph_stmt1^.parse_root^.children , exprrule1_assign ) ;
Node.AppendNode( depgraph_stmt1^.parse_root^.children ,  NEW( REF Node.T , val := ":=" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( depgraph_stmt1^.parse_root^.children , exprrule1_assign2 ) ;
(* Modify the depgraph *)
(* Modify x^:=3 in depgraph *)
depgraph_stmt1^.next^.is_static := TRUE ;
depgraph_stmt1^.next^.assigned_vars := TextList.Cons( "x^" , NIL ) ;
(* Modify x^:=3 in depgraph *)
(* Modify IF in depgraph *)
depgraph_stmt1^.next^.next^.is_static := TRUE ;
depgraph_stmt1^.next^.next^.assigned_vars := TextList.Cons( "y" , NIL ) ;
depgraph_stmt1_subdepgraph1 := depgraph_stmt1^.next^.next^.subdepgraph.head ;
depgraph_stmt1_subdepgraph1^.is_static := TRUE ;
depgraph_stmt1_subdepgraph1^.assigned_vars := TextList.Cons( "y" , NIL ) ;
depgraph_stmt1_subdepgraph2 := depgraph_stmt1^.next^.next^.subdepgraph.tail.head ;
depgraph_stmt1_subdepgraph2^.is_static := TRUE ;
depgraph_stmt1_subdepgraph2^.assigned_vars := TextList.Cons( "y" , NIL ) ;

nextdeps := depgraph_stmt1^.next ;
sdg1 := depgraph_stmt1_subdepgraph1 ;
sdg2 := depgraph_stmt1_subdepgraph2 ;
sdg_refany_list := REFANYList.Cons( sdg1 , REFANYList.Cons( sdg2 , NIL ) ) ;
depgraph_stmt1^.next^.next^.deps := REFANYList.Cons( nextdeps , sdg_refany_list ) ;
depgraph_stmt1^.next^.next^.dep_order := CARDINALList.Cons( 0 , NIL ) ;
depgraph_stmt1_subdepgraph1.deps := NEW( REFANYList.T , head := nextdeps , tail := NIL ) ;
depgraph_stmt1_subdepgraph1.dep_order := CARDINALList.Cons( 0 , NIL ) ;
depgraph_stmt1_subdepgraph2.deps := NEW( REFANYList.T , head := nextdeps , tail := NIL ) ;
depgraph_stmt1_subdepgraph2.dep_order := CARDINALList.Cons( 0 , NIL ) ;
(* Modify IF in depgraph *)
(* Modify RETURN in depgraph *)
depgraph_stmt1^.next^.next^.next^.is_static := FALSE ;
depgraph_stmt1^.next^.next^.next^.assigned_vars := NIL ;

depgraph_next_next := depgraph_stmt1^.next^.next ;
refany_depgraph_2 := REFANYList.Cons( depgraph_stmt1_subdepgraph1 , REFANYList.Cons( depgraph_stmt1_subdepgraph2 , NIL ) ) ;
refany_depgraph := REFANYList.Cons( depgraph_next_next , refany_depgraph_2 ) ;
depgraph_stmt1^.next^.next^.next^.deps := REFANYList.Cons( depgraph_stmt1_refany , refany_depgraph ) ;
depgraph_stmt1^.next^.next^.next^.dep_order := CARDINALList.Cons( 0 , CARDINALList.Cons( 1 , NIL ) ) ;
(* Modify RETURN in depgraph *)
IO.Put( "Got dependency graph...\n" ) ;
<* ASSERT depgraph^.parse_root # NIL *>
IO.Put( "Length of dependency graph (should be 2 for IfSt and ReturnSt): " & Fmt.Int( DepGraph.Length( depgraph ) ) & "\n" ) ;
ugh_another_temp := depgraph ;
WHILE ugh_another_temp # NIL DO
	IO.Put( " - " & ugh_another_temp^.parse_root^.val & "\n" ) ;
	ugh_another_temp := ugh_another_temp^.next ;
END ;
TRY
	Spec.DebugTree( depgraph^.parse_root , "./depgraph_pretest.m3" ) ;
EXCEPT
	| Spec.InvalidFname => IO.Put( "Can't use that fname.\n" ) ;
	| Spec.OutError => IO.Put( "Outerror.\n" ) ;
END ;
TRY
	Spec.DebugTree( depgraph^.next^.parse_root , "./depgraph_pretest_next.m3" ) ;
EXCEPT
	| Spec.InvalidFname => IO.Put( "Can't use that fname.\n" ) ;
	| Spec.OutError => IO.Put( "Outerror.\n" ) ;
END ;
IO.Put( "Costructing parse tree from dep graph...\n" ) ;
DepGraph.ConstructParseTree( proc_block^.cur , depgraph , depgraph_pms ) ;
IO.Put( "=== DEPGRAPH DEBUG ===\n" ) ;
DepGraph.DebugDepGraph( depgraph_stmt1 ) ;
IO.Put( "=== DEPGRAPH DEBUG ===\n" ) ;
IO.Put( "Constructed parse tree from dep graph...\n" ) ;
TRY
	Spec.DebugTree( proc_block^.cur , "./depgraph_tree.m3" ) ;
EXCEPT
	| Spec.InvalidFname => IO.Put( "Can't use that fname.\n" ) ;
	| Spec.OutError => IO.Put( "Outerror.\n" ) ;
END ;
IO.Put( "Generating code from parse tree...\n" ) ;
TRY
	Spec.GenCode( root , style_rules , "./depgraph.m3" ) ;
EXCEPT
	| Spec.InvalidFname => IO.Put( "Can't use that fname.\n" ) ;
	| Spec.OutError => IO.Put( "Outerror.\n" ) ;
END ;
IO.Put( "Generated code from parse tree...\n" ) ;
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
