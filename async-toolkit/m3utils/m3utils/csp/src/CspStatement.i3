INTERFACE CspStatement;
IMPORT CspExpression;
IMPORT Pathname;
IMPORT CspDeclaratorSeq;
IMPORT CspSyntax;

TYPE
  Expr = CspExpression.T;
  
  T <: Public;

  Public = CspSyntax.T;

  Assignment = T BRANDED Brand & " Assignment" OBJECT
    lhs, rhs : Expr;
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

  Skip <: T;

  
  Send <: T OBJECT
    chan, val : Expr;
  END;

  Recv <: T OBJECT
    chan, val : Expr;
  END;

  Var <: T OBJECT
    decls : CspDeclaratorSeq.T;
    stmt  : T;
  END;

  Expression <: T OBJECT
    expr : Expr;
  END;

CONST Brand = "CspStatement";

END CspStatement.
