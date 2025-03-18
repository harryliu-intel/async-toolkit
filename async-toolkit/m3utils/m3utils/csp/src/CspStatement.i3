INTERFACE CspStatement;
IMPORT CspExpression;
IMPORT Pathname;
IMPORT CspDeclaratorSeq;

TYPE
  Expr = CspExpression.T;
  
  T <: Public;

  Public = ROOT OBJECT
  END;

  Assignment = T BRANDED Brand & " Assignment" OBJECT
    lhs, rhs : Expr;
  END;

  (**********************************************************************)

  Guarded <: T;

  Repetition = Guarded BRANDED Brand & " Repetition" OBJECT END;

  Selection = Guarded BRANDED Brand & " Selection" OBJECT END;
  
  DetRepetition = Repetition BRANDED Brand & " DetRepetition" OBJECT
  END;

  DetSelection = Selection BRANDED Brand & " DetSelection" OBJECT
  END;

  NondetRepetition = Repetition BRANDED Brand & " NondetRepetition" OBJECT
  END;

  NondetSelection = Selection BRANDED Brand & " NondetSelection" OBJECT
  END;

  (**********************************************************************)

  Error = T BRANDED Brand & " Error" OBJECT
    fn : Pathname.T;
    lno, cno : CARDINAL;
  END;

  (**********************************************************************)

  Compound <: T;
  
  Parallel = Compound BRANDED Brand & " Parallel" OBJECT
  END;

  Sequential = Compound BRANDED Brand & " Sequential" OBJECT
  END;

  (**********************************************************************)
  
  Skip = T BRANDED Brand & " Skip" OBJECT
  END;

  Send = T BRANDED Brand & " Send" OBJECT
    chan, val : Expr;
  END;

  Recv = T BRANDED Brand & " Recv" OBJECT
    chan, val : Expr;
  END;

  Var = T BRANDED Brand & " Var" OBJECT
    decls : CspDeclaratorSeq.T;
    stmt  : T;
  END;

  Expression = T BRANDED Brand & " Expression" OBJECT
    expr : Expr;
  END;

CONST Brand = "CspStatement";

END CspStatement.
