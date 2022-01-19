MODULE GateExpr;
FROM Fmt IMPORT F;

PROCEDURE And(a, b : T) : T =
  BEGIN
    RETURN NEW(Expr, op := Op.And, a := a, b := b, nm := F("(& %s %s)", a.nm, b.nm))
  END And;
  
PROCEDURE Or(a, b : T) : T =
  BEGIN
    RETURN NEW(Expr, op := Op.Or, a := a, b := b, nm := F("(| %s %s)", a.nm, b.nm))
  END Or;
  
PROCEDURE Not(a : T) : T =
  BEGIN RETURN NEW(Expr, op := Op.Not, a := a, b := NIL, nm := F("(~ %s)", a.nm))
  END Not;
  
PROCEDURE New(nm : TEXT) : T =
  BEGIN RETURN NEW(Named, nm := nm) END New;

PROCEDURE Format(a : T) : TEXT =
  BEGIN
    TYPECASE a OF
      Expr(x) =>
      CASE x.op OF
        Op.And => RETURN F("(%s & %s)", Format(x.a), Format(x.b))
      |
        Op.Or  => RETURN F("(%s | %s)", Format(x.a), Format(x.b))
      |
        Op.Not => RETURN F("!%s", Format(x.a))
      END
    |
      Named(n) => RETURN n.nm
    ELSE
      <*ASSERT FALSE*>
    END
  END Format;
  
BEGIN END GateExpr.
