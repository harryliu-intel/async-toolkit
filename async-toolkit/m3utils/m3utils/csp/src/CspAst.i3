INTERFACE CspAst;

IMPORT CspExpression;
IMPORT CspExpressionSeq;
IMPORT CspStatement;
IMPORT CspGuardedCommand;
IMPORT CspGuardedCommandSeq;
IMPORT CspInterval;
IMPORT CspRange;
IMPORT CspType;
IMPORT CspDirection;
IMPORT Pathname;
IMPORT CspStatementSeq;
IMPORT CspDeclarationSeq;
IMPORT CspStructMemberSeq;
IMPORT BigInt;
IMPORT Atom;

TYPE
  Expr           = CspExpression.T;
  ExprSeq        = CspExpressionSeq.T;
  Stmt           = CspStatement.T;
  StmtSeq        = CspStatementSeq.T;
  DeclSeq        = CspDeclarationSeq.T;
  Type           = CspType.T;
  Range          = CspRange.T;
  Interval       = CspInterval.T;
  Direction      = CspDirection.T;
  
PROCEDURE AssignmentStmt(lhs, rhs : Expr) : Stmt;

PROCEDURE DetRepetitionStmt(gcs : CspGuardedCommandSeq.T) : Stmt;

PROCEDURE DetSelectionStmt(gcs : CspGuardedCommandSeq.T) : Stmt;

PROCEDURE NondetRepetitionStmt(gcs : CspGuardedCommandSeq.T) : Stmt;

PROCEDURE NondetSelectionStmt(gcs : CspGuardedCommandSeq.T) : Stmt;

PROCEDURE ErrorStmt(fn : Pathname.T; lno, cno : CARDINAL) : Stmt;

PROCEDURE ParallelStmt(stmts : StmtSeq) : Stmt;

PROCEDURE SequentialStmt(stmts : StmtSeq) : Stmt;

PROCEDURE SkipStmt() : Stmt;

PROCEDURE SendStmt(chan : Expr; val : Expr) : Stmt;

PROCEDURE RecvStmt(chan : Expr; val : Expr) : Stmt;

PROCEDURE VarStmt(decls : DeclSeq; stmt : Stmt) : Stmt;

PROCEDURE ExpressionStmt(expr : Expr) : Stmt;

(**********************************************************************)

PROCEDURE GuardedCommand(guard : Expr; command : Stmt) : CspGuardedCommand.T;
  
(**********************************************************************)

(* literal expressions *)

PROCEDURE BooleanExpr(val : BOOLEAN) : Expr;

PROCEDURE IntegerExpr(val : BigInt.T) : Expr;

PROCEDURE StringExpr(val : TEXT) : Expr;

(* identifiers *)

PROCEDURE IdentifierExpr(id : Atom.T) : Expr;
  
(* compound expressions *)  

PROCEDURE BinExpr(op : CspExpression.BinaryOp; l, r : Expr) : Expr;

PROCEDURE UnaExpr(op : CspExpression.UnaryOp; x : Expr) : Expr;

PROCEDURE ArrayAccessExpr(arr, idx : Expr) : Expr;

PROCEDURE MemberAccessExpr(struct, member : Expr) : Expr;

PROCEDURE StructureAccessExpr(struct, member : Expr) : Expr;

PROCEDURE BitRangeExpr(bits, minx, maxx : Expr) : Expr;

PROCEDURE RecvExpr(chan : Expr) : Expr;

PROCEDURE PeekExpr(chan : Expr) : Expr;

PROCEDURE ProbeExpr(chan : Expr) : Expr;

PROCEDURE FunctionCallExpr(f : Expr; args : ExprSeq) : Expr;

(**********************************************************************)

PROCEDURE ArrayType(range : Range; elemntType : Type) : Type;

PROCEDURE BooleanType() : Type;

PROCEDURE ChannelStructureType(members : CspStructMemberSeq.T) : Type;

PROCEDURE ChannelType(numValues : BigInt.T; dir : Direction) : Type;

PROCEDURE IntegerType(isConst, isSigned : BOOLEAN;
                      dw                : CARDINAL;
                      interval          : Interval) : Type;

PROCEDURE NodeType(arrayed   : BOOLEAN;
                   width     : [1..LAST(CARDINAL)];
                   direction : Direction) : Type;

PROCEDURE StringType() : Type;

PROCEDURE StructureType(isConst : BOOLEAN; name : TEXT) : Type;

(*
PROCEDURE TemporaryIntegerType() : Type; (* ?? *)
*)
 
END CspAst.
