MODULE VarExpr;
IMPORT TextSet;
IMPORT Debug;
IMPORT Math;
IMPORT Text;
FROM Fmt IMPORT F;

CONST TE = Text.Equal;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    getDeps := GetDeps;
    eval := Calc;
  END;

  (* all the calls below, both GetDeps and Calc, should be turned into
     method calls *)
  
PROCEDURE GetDeps(t : T; into : TextSet.T; excluding : TextSet.T) =
  BEGIN
    TYPECASE t OF
      Func(f) =>
      <*ASSERT f.args # NIL*>
      FOR i := FIRST(f.args^) TO LAST(f.args^) DO
        GetDeps(f.args[i], into, excluding)
      END
    |
      Variable(v) =>
      IF excluding = NIL OR NOT excluding.member(v.nm) THEN
        EVAL into.insert(v.nm)
      END
    |
      Literal => (* skip *)
    |
      UnOp(u) =>
      GetDeps(u.a, into, excluding)
    |
      BinOp(b) =>
      GetDeps(b.a, into, excluding);
      GetDeps(b.b, into, excluding)
    |
      TernOp(t) =>
      GetDeps(t.a, into, excluding);
      GetDeps(t.b, into, excluding);
      GetDeps(t.c, into, excluding)
    ELSE
      Debug.Error("VarExpr.GetDeps : Unknown subtype of VarExpr.T")
    END
  END GetDeps;

PROCEDURE Calc(t : T; getter : Getter) : LONGREAL =
  BEGIN
    TYPECASE t OF
      Func(f) =>
      IF    TE(f.nm, "Get_P") OR TE(f.nm, "Get_O") OR TE(f.nm, "get_M") OR TE(f.nm, "Get_E") THEN
        (* a lookup of some kind *)
        RETURN Calc(f.args[0], getter)
      ELSIF TE(f.nm, "max") THEN
        VAR
          q := FIRST(LONGREAL);
        BEGIN
          FOR i := FIRST(f.args^) TO LAST(f.args^) DO
            q := MAX(q, Calc(f.args[i], getter))
          END;
          RETURN q
        END
      ELSIF TE(f.nm, "sqrt") THEN
        RETURN Math.sqrt(Calc(f.args[0], getter))
      ELSIF TE(f.nm, "exp") THEN
        RETURN Math.exp(Calc(f.args[0], getter))
      ELSIF TE(f.nm, "log") THEN
        RETURN Math.log(Calc(f.args[0], getter))
      ELSE
        Debug.Error(F("unknown func \"%s\"", f.nm));
        <*ASSERT FALSE*>
      END
    |
      Variable(v) =>
        RETURN getter.v(v.nm)
    |
      Literal(lit) =>
      RETURN lit.val
    |
      UMinus(u) =>
      RETURN -Calc(u.a, getter)
    |
      Plus(p) =>
      RETURN Calc(p.a, getter) + Calc(p.b, getter)
    |
      Minus(m) =>
      RETURN Calc(m.a, getter) - Calc(m.b, getter)
    |
      Times(r) =>
      RETURN Calc(r.a, getter) * Calc(r.b, getter)
    |
      Divide(q) =>
      RETURN Calc(q.a, getter) / Calc(q.b, getter)
    |
     Equal(e) =>
     IF      Calc(e.a, getter) = Calc(e.b, getter) THEN
       RETURN 1.0d0
     ELSE
       RETURN 0.0d0
     END
    |
      QMark(qm) =>
      IF      Calc(qm.a, getter) # 0.0d0 THEN
        RETURN Calc(qm.b, getter)
      ELSE
        RETURN Calc(qm.c, getter)
      END
    ELSE
      Debug.Error("VarExpr.Calc : Unknown subtype of VarExpr.T");
      <*ASSERT FALSE*>
    END
  END Calc;

BEGIN END VarExpr.
