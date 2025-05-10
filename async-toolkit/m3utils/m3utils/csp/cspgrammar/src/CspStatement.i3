INTERFACE CspStatement;
IMPORT CspExpression;
IMPORT Pathname;
IMPORT CspDeclarator;
IMPORT CspSyntax;
IMPORT Atom;
IMPORT CspRange;
IMPORT CspStructDeclaratorSeq;

TYPE
  Expr = CspExpression.T;
  
  T <: Public;

  Public = CspSyntax.T;

  Assignment <: T OBJECT
    lhs, rhs : Expr;
  END;

  AssignOperate <: T OBJECT
    lhs, rhs : Expr;
    op       : CspExpression.BinaryOp;
  END;

  (**********************************************************************)

  Guarded <: T;

  Repetition = Guarded BRANDED Brand & " Repetition" OBJECT END;

  Selection = Guarded BRANDED Brand & " Selection" OBJECT END;

  DetRepetition <: Repetition;

  DetSelection <: Selection;

  NondetRepetition <: Repetition;

  NondetSelection <: Selection;
  
  (**********************************************************************)

  Error <: T OBJECT
    fn : Pathname.T;
    lno, cno : CARDINAL;
  END;

  (**********************************************************************)

  Compound <: T;

  Parallel <: Compound;

  Sequential <: Compound;

  (**********************************************************************)

  Loop = T OBJECT
    dummy : Atom.T;
    range : CspRange.T;
    stmt  : T;
  END;

  SequentialLoop <: Loop;

  ParallelLoop <: Loop;
  
  (**********************************************************************)

  Skip <: T;

  
  Send <: T OBJECT
    chan, val : Expr;
  END;

  Recv <: T OBJECT
    chan, val : Expr;
  END;

  Var <: T OBJECT
    decl : CspDeclarator.T;
  END;

  Expression <: T OBJECT
    expr : Expr;
  END;

  Comment <: T OBJECT
    string : TEXT;
  END;

  Structure <: T OBJECT 
    (* was in CspStructDeclarator, but is analogous to Var *)
    name  : Atom.T;
    decls : CspStructDeclaratorSeq.T;
  END;

CONST Brand = "CspStatement";

END CspStatement.
