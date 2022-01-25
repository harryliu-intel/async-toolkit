MODULE GateExpr;
FROM Fmt IMPORT F;
IMPORT TextSet, TextSetDef;

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

PROCEDURE Format(a : T; not : TEXT) : TEXT =
  BEGIN
    TYPECASE a OF
      Expr(x) =>
      CASE x.op OF
        Op.And => RETURN F("(%s & %s)", Format(x.a, not), Format(x.b, not))
      |
        Op.Or  => RETURN F("(%s | %s)", Format(x.a, not), Format(x.b, not))
      |
        Op.Not => RETURN F("%s%s", not, Format(x.a, not))
      END
    |
      Named(n) => RETURN n.nm
    ELSE
      <*ASSERT FALSE*>
    END
  END Format;

PROCEDURE Fanins(a : T) : TextSet.T =
  VAR
    res := NEW(TextSetDef.T).init();

  PROCEDURE Recurse(t : T) =
    BEGIN
      TYPECASE t OF
        Expr(x) =>
        CASE x.op OF
          Op.And, Op.Or => Recurse(x.a); Recurse(x.b)
        |
          Op.Not => Recurse(x.a)
        END
      |
        Named(n) => EVAL res.insert(n.nm)
      ELSE
        <*ASSERT FALSE*>
      END
    END Recurse;
    
  BEGIN
    Recurse(a);
    RETURN res
  END Fanins;
  
BEGIN END GateExpr.
