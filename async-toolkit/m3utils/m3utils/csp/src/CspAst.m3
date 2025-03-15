MODULE CspAst;
IMPORT CspGuardedCommand;
IMPORT CspGuardedCommandSeq;
IMPORT Pathname;
IMPORT BigInt;
IMPORT Atom;
IMPORT CspExpression;

PROCEDURE AssignmentStmt(lhs, rhs : Expr) : Stmt =
  BEGIN
    RETURN NIL
  END AssignmentStmt;

PROCEDURE DetRepetitionStmt(gcs : CspGuardedCommandSeq.T) : Stmt =
  BEGIN
    RETURN NIL
  END DetRepetitionStmt;

PROCEDURE DetSelectionStmt(gcs : CspGuardedCommandSeq.T) : Stmt =
  BEGIN
    RETURN NIL
  END DetSelectionStmt;

PROCEDURE NondetRepetitionStmt(gcs : CspGuardedCommandSeq.T) : Stmt =
  BEGIN
    RETURN NIL
  END NondetRepetitionStmt;

PROCEDURE NondetSelectionStmt(gcs : CspGuardedCommandSeq.T) : Stmt =
  BEGIN
    RETURN NIL
  END NondetSelectionStmt;

PROCEDURE ErrorStmt(fn : Pathname.T; lno, cno : CARDINAL) : Stmt =
  BEGIN
    RETURN NIL
  END ErrorStmt;

PROCEDURE ParallelStmt(stmts : StmtSeq) : Stmt =
  BEGIN
    RETURN NIL
  END ParallelStmt;

PROCEDURE SequentialStmt(stmts : StmtSeq) : Stmt =
  BEGIN
    RETURN NIL
  END SequentialStmt;

PROCEDURE SkipStmt() : Stmt =
  BEGIN
    RETURN NIL
  END SkipStmt;

PROCEDURE SendStmt(chan : Expr; val : Expr) : Stmt =
  BEGIN
    RETURN NIL
  END SendStmt;

PROCEDURE RecvStmt(chan : Expr; val : Expr) : Stmt =
  BEGIN
    RETURN NIL
  END RecvStmt;

PROCEDURE VarStmt(decls : DeclSeq; stmt : Stmt) : Stmt =
  BEGIN
    RETURN NIL
  END VarStmt;

PROCEDURE ExpressionStmt(expr : Expr) : Stmt =
  BEGIN
    RETURN NIL
  END ExpressionStmt;

(**********************************************************************)

PROCEDURE GuardedCommand(guard : Expr; command : Stmt) : CspGuardedCommand.T =
  BEGIN
    RETURN NEW(CspGuardedCommand.T, guard := guard, command := command);
  END GuardedCommand;


(**********************************************************************)

PROCEDURE BooleanExpr(val : BOOLEAN) : Expr =
  BEGIN RETURN NIL END BooleanExpr;

PROCEDURE IntegerExpr(val : BigInt.T) : Expr =
  BEGIN RETURN NIL END IntegerExpr;

PROCEDURE StringExpr(val : TEXT) : Expr =
  BEGIN RETURN NIL END StringExpr;

PROCEDURE IdentifierExpr(atom : Atom.T) : Expr =
  BEGIN RETURN NIL END IdentifierExpr;
  
PROCEDURE BinExpr(op : CspExpression.BinaryOp; l, r : Expr) : Expr =
  BEGIN RETURN NIL END BinExpr;

PROCEDURE UnaExpr(op : CspExpression.UnaryOp; x : Expr) : Expr =
  BEGIN RETURN NIL END UnaExpr;

PROCEDURE ArrayAccessExpr(arr, idx : Expr) : Expr =
  BEGIN RETURN NIL END ArrayAccessExpr;

PROCEDURE MemberAccessExpr(struct, member : Expr) : Expr =
  BEGIN RETURN NIL END MemberAccessExpr;

PROCEDURE StructureAccessExpr(struct, member : Expr) : Expr =
  BEGIN RETURN NIL END StructureAccessExpr;

PROCEDURE BitRangeExpr(bits, minx, maxx : Expr) : Expr =
  BEGIN RETURN NIL END BitRangeExpr;

PROCEDURE RecvExpr(chan : Expr) : Expr =
  BEGIN RETURN NIL END RecvExpr;

PROCEDURE PeekExpr(chan : Expr) : Expr =
  BEGIN RETURN NIL END PeekExpr;

PROCEDURE ProbeExpr(chan : Expr) : Expr =
  BEGIN RETURN NIL END ProbeExpr;

PROCEDURE FunctionCallExpr(f : Expr; args : ExprSeq) : Expr =
  BEGIN RETURN NIL END FunctionCallExpr;

(**********************************************************************)

PROCEDURE ArrayType(range : Range; elemntType : Type) : Type =
  BEGIN
    RETURN NIL
  END ArrayType;
  
PROCEDURE BooleanType() : Type =
  BEGIN
    RETURN NIL
  END BooleanType;
  
PROCEDURE ChannelStructureType() : Type =
  BEGIN
    RETURN NIL
  END ChannelStructureType;
  
PROCEDURE ChannelType(numValues : BigInt.T; dir : Direction) : Type =
  BEGIN
    RETURN NIL
  END ChannelType;
  
PROCEDURE IntegerType(isConst, isSigned : BOOLEAN;
                      dw                : CARDINAL;
                      interval          : Interval) : Type =
  BEGIN
    RETURN NIL
  END IntegerType;
  
PROCEDURE NodeType(arrayed   : BOOLEAN;
                   width     : [1..LAST(CARDINAL)];
                   direction : Direction) : Type =
  BEGIN
    RETURN NIL
  END NodeType;

PROCEDURE StringType() : Type =
  BEGIN
    RETURN NIL
  END StringType;
  
PROCEDURE StructureType(isConst : BOOLEAN; name : TEXT) : Type =
  BEGIN
    RETURN NIL
  END StructureType;
  

BEGIN END CspAst.

