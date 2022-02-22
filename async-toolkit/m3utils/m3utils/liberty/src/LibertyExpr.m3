MODULE LibertyExpr;
IMPORT LibertyComponent;
IMPORT LibertyNumber;

REVEAL
  T = LibertyComponent.T BRANDED Brand OBJECT
  OVERRIDES
    format := Format;
  END;

PROCEDURE Format(t : T) : TEXT =
  BEGIN
  END Format;
  
PROCEDURE Plus(a, b : T) : T =
  BEGIN
    RETURN NEW(Binary, op := Op.Plus, a := a, b := b)
  END Plus;
  
PROCEDURE Minus(a, b : T) : T =
  BEGIN
    RETURN NEW(Binary, op := Op.Minus, a := a, b := b)
  END Minus;
  
PROCEDURE Times(a, b : T) : T =
  BEGIN
    RETURN NEW(Binary, op := Op.Times, a := a, b := b)
  END Times;

PROCEDURE Div(a, b : T) : T =
  BEGIN
    RETURN NEW(Binary, op := Op.Div, a := a, b := b)
  END Div;

PROCEDURE Uminus(a : T) : T =
  BEGIN
    RETURN NEW(Unary, op := Op.Uminus, a := a)
  END Uminus;
  
PROCEDURE Uplus(a : T) : T =
  BEGIN
    RETURN NEW(Unary, op := Op.Uplus, a := a)
  END Uplus;
  
PROCEDURE Num(n : LibertyNumber.T) : T =
  BEGIN
    RETURN NEW(Const, type := Type.Num, val := n)
  END Num;

PROCEDURE Ident(n : TEXT) : T =
  BEGIN
    RETURN NEW(Const, type := Type.Ident, val := n)
  END Ident;

BEGIN END LibertyExpr.
