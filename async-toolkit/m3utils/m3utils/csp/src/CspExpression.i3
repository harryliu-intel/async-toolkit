INTERFACE CspExpression;
IMPORT Atom;
IMPORT BigInt;
IMPORT CspSyntax;

TYPE
  T = CspSyntax.T BRANDED Brand OBJECT END;

  (* literals *)
  Boolean <: PublicBoolean;

  PublicBoolean = T OBJECT
    val : BOOLEAN;
  END;

  Integer <: PublicInteger;

  PublicInteger = T OBJECT
    val : BigInt.T;
  END;

  String <: PublicString;

  PublicString = T OBJECT
    val : TEXT;
  END;

  (* identifiers *)

  Identifier <: PublicIdentifier;

  PublicIdentifier = T OBJECT
    id : Atom.T;
  END;

  (* operations *)

  BinaryOp = { Add, Sub, Div, Rem, Mul,
               EQ, GE, GT, LT, LE, NE,
               And, Or, Xor,
               SHL, SHR,
               CondAnd, CondOr };

  UnaryOp = { Neg, Not };

  Binary <: PublicBinary;

  PublicBinary = T OBJECT
    op   : BinaryOp;
    l, r : T;
  END;

  Unary <: PublicUnary;

  PublicUnary = T OBJECT
    op : UnaryOp;
    x  : T;
  END;

  MemberAccess <: PublicAccess;
  StructureAccess <: PublicAccess;

  PublicAccess = T OBJECT
    struct : T;
    member : Atom.T;
  END;

  ArrayAccess <: PublicArrayAccess;

  PublicArrayAccess = T OBJECT
    arr, idx : T;
  END;

  BitRange <: PublicBitRange;

  PublicBitRange = T OBJECT
    bits, minx, maxx : T;
  END;

  ChanExpr = T OBJECT
    chan : T;
  END;
  
  Probe <: ChanExpr;

  Receive <: ChanExpr;

  Peek <: ChanExpr;
  
  FunctionCall <: T; (* see CspExpressionPublic *)
  
CONST Brand = "CspExpression";

CONST
  BinMap = ARRAY BinaryOp OF TEXT {
  "+", "-", "/", "%", "*",
  "=", ">=", ">", "<", "<=", "!=",
  "&", "|", "^",
  "<<", ">>",
  "&&", "||"
  };

  UnaMap = ARRAY UnaryOp OF TEXT {
  "-", "~"
  };

END CspExpression.
