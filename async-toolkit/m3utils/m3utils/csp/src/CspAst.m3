MODULE CspAst;
IMPORT CspGuardedCommand;
IMPORT CspGuardedCommandSeq;
IMPORT Pathname;
IMPORT BigInt;
IMPORT Atom;

IMPORT CspExpression AS X;
IMPORT CspExpressionPublic;

IMPORT CspType AS T;
IMPORT CspTypePublic;

IMPORT CspStatement AS S;
IMPORT CspStatementPublic;

IMPORT CspStructMemberSeq;

IMPORT CspDeclaration AS D;
IMPORT CspDeclarationPublic;

IMPORT CspDeclaratorSeq;
IMPORT CspType;

PROCEDURE AssignmentStmt(lhs, rhs : Expr) : Stmt =
  BEGIN
    RETURN NEW(S.Assignment, lhs := lhs, rhs := rhs)
  END AssignmentStmt;

PROCEDURE DetRepetitionStmt(gcs : CspGuardedCommandSeq.T) : Stmt =
  BEGIN
    RETURN NEW(S.DetRepetition, gcs := gcs)
  END DetRepetitionStmt;

PROCEDURE DetSelectionStmt(gcs : CspGuardedCommandSeq.T) : Stmt =
  BEGIN
    RETURN NEW(S.DetSelection, gcs := gcs)
  END DetSelectionStmt;

PROCEDURE NondetRepetitionStmt(gcs : CspGuardedCommandSeq.T) : Stmt =
  BEGIN
    RETURN NEW(S.NondetRepetition, gcs := gcs)
  END NondetRepetitionStmt;

PROCEDURE NondetSelectionStmt(gcs : CspGuardedCommandSeq.T) : Stmt =
  BEGIN
    RETURN NEW(S.NondetSelection, gcs := gcs)
  END NondetSelectionStmt;

PROCEDURE ErrorStmt(fn : Pathname.T; lno, cno : CARDINAL) : Stmt =
  BEGIN
    RETURN NEW(S.Error, fn := fn, lno := lno, cno := cno)
  END ErrorStmt;

PROCEDURE ParallelStmt(stmts : StmtSeq) : Stmt =
  BEGIN
    RETURN NEW(S.Parallel, stmts := stmts)
  END ParallelStmt;

PROCEDURE SequentialStmt(stmts : StmtSeq) : Stmt =
  BEGIN
    RETURN NEW(S.Sequential, stmts := stmts)
  END SequentialStmt;

PROCEDURE SkipStmt() : Stmt =
  BEGIN
    RETURN NEW(S.Skip)
  END SkipStmt;

PROCEDURE SendStmt(chan : Expr; val : Expr) : Stmt =
  BEGIN
    RETURN NEW(S.Send, chan := chan, val := val)
  END SendStmt;

PROCEDURE RecvStmt(chan : Expr; val : Expr) : Stmt =
  BEGIN
    RETURN NEW(S.Recv, chan := chan, val := val)
  END RecvStmt;

PROCEDURE VarStmt(decls : DeclSeq; stmt : Stmt) : Stmt =
  BEGIN
    RETURN NEW(S.Var, decls := decls, stmt := stmt)
  END VarStmt;

PROCEDURE ExpressionStmt(expr : Expr) : Stmt =
  BEGIN
    RETURN NEW(S.Expression, expr := expr)
  END ExpressionStmt;

(**********************************************************************)

PROCEDURE GuardedCommand(guard : Expr; command : Stmt) : CspGuardedCommand.T =
  BEGIN
    RETURN NEW(CspGuardedCommand.T, guard := guard, command := command);
  END GuardedCommand;

(**********************************************************************)

PROCEDURE BooleanExpr(val : BOOLEAN) : Expr =
  BEGIN
    RETURN NEW(X.Boolean, val := val)
  END BooleanExpr;

PROCEDURE IntegerExpr(val : BigInt.T) : Expr =
  BEGIN
    RETURN NEW(X.Integer, val := val)
  END IntegerExpr;

PROCEDURE StringExpr(val : TEXT) : Expr =
  BEGIN
    RETURN NEW(X.String, val := val)
  END StringExpr;

PROCEDURE IdentifierExpr(id : Atom.T) : Expr =
  BEGIN
    RETURN NEW(X.Identifier, id := id)
  END IdentifierExpr;
  
PROCEDURE BinExpr(op : X.BinaryOp; l, r : Expr) : Expr =
  BEGIN
    RETURN NEW(X.Binary, op := op, l := l, r := r)
  END BinExpr;

PROCEDURE UnaExpr(op : X.UnaryOp; x : Expr) : Expr =
  BEGIN
    RETURN NEW(X.Unary, op := op, x := x)
  END UnaExpr;

PROCEDURE ArrayAccessExpr(arr, idx : Expr) : Expr =
  BEGIN
    RETURN NEW(X.ArrayAccess, arr := arr, idx := idx)
  END ArrayAccessExpr;

PROCEDURE MemberAccessExpr(struct : Expr; member : Atom.T) : Expr =
  BEGIN
    RETURN NEW(X.MemberAccess, struct := struct, member := member)
  END MemberAccessExpr;

PROCEDURE StructureAccessExpr(struct : Expr; member : Atom.T) : Expr =
  BEGIN
    RETURN NEW(X.StructureAccess, struct := struct, member := member)
  END StructureAccessExpr;

PROCEDURE BitRangeExpr(bits, minx, maxx : Expr) : Expr =
  BEGIN
    RETURN NEW(X.BitRange, bits := bits, minx := minx, maxx := maxx)
  END BitRangeExpr;

PROCEDURE RecvExpr(chan : Expr) : Expr =
  BEGIN
    RETURN NEW(X.Receive, chan := chan)
  END RecvExpr;

PROCEDURE PeekExpr(chan : Expr) : Expr =
  BEGIN
    RETURN NEW(X.Peek, chan := chan)
  END PeekExpr;

PROCEDURE ProbeExpr(chan : Expr) : Expr =
  BEGIN
    RETURN NEW(X.Probe, chan := chan)
  END ProbeExpr;

PROCEDURE FunctionCallExpr(f : Expr; args : ExprSeq) : Expr =
  BEGIN
    RETURN NEW(X.FunctionCall, f := f, args := args)
  END FunctionCallExpr;

(**********************************************************************)

PROCEDURE ArrayType(range : Range; elemntType : Type) : Type =
  BEGIN
    RETURN NEW(T.Array, range := range, elemntType := elemntType)
  END ArrayType;
  
PROCEDURE BooleanType() : Type =
  BEGIN
    RETURN NEW(T.Boolean)
  END BooleanType;
  
PROCEDURE ChannelStructureType(members : CspStructMemberSeq.T) : Type =
  BEGIN
    RETURN NEW(T.ChannelStructure, members := members)
  END ChannelStructureType;
  
PROCEDURE ChannelType(numValues : BigInt.T; dir : Direction) : Type =
  BEGIN
    RETURN NEW(T.Channel, numValues := numValues, dir := dir)
  END ChannelType;
  
PROCEDURE IntegerType(isConst, isSigned : BOOLEAN;
                      dw                : CARDINAL;
                      interval          : Interval) : Type =
  BEGIN
    RETURN NEW(T.Integer, isConst := isConst, isSigned := isSigned, dw := dw, interval := interval)
  END IntegerType;
  
PROCEDURE NodeType(arrayed   : BOOLEAN;
                   width     : [1..LAST(CARDINAL)];
                   direction : Direction) : Type =
  BEGIN
    RETURN NEW(T.Node, arrayed := arrayed, width := width, direction := direction)
  END NodeType;

PROCEDURE StringType() : Type =
  BEGIN
    RETURN NEW(T.String)
  END StringType;
  
PROCEDURE StructureType(isConst : BOOLEAN; name : TEXT) : Type =
  BEGIN
    RETURN NEW(T.Structure, isConst := isConst, name := name)
  END StructureType;

(**********************************************************************)

PROCEDURE FunctionDeclaration(funcName   : Atom.T;
                              formals    : CspDeclaratorSeq.T;
                              returnType : CspType.T;) : Decl =
  BEGIN
    RETURN NEW(D.Function, funcName := funcName, formals := formals, returnType := returnType)
  END FunctionDeclaration;

PROCEDURE StructureDeclaration(name  : Atom.T;
                               decls : CspDeclaratorSeq.T;) : Decl =
  BEGIN
    RETURN NEW(D.Structure, name := name, decls := decls)
  END StructureDeclaration;

BEGIN END CspAst.

