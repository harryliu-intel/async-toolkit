MODULE GlitchExpr EXPORTS GlitchExpr, GlitchExprEval;
IMPORT BDD;
FROM Fmt IMPORT F;
IMPORT ZeroOneX;
IMPORT Text01XTbl;
IMPORT Debug;
IMPORT TextGlitchExprTbl;
IMPORT TextSet, TextSetDef;

PROCEDURE And(a, b : T) : T =
  BEGIN
    RETURN NEW(Expr, op := Op.And, a := a, b := b, nm := F("(& %s %s)", a.nm, b.nm), x := BDD.And(a.x, b.x))
  END And;
  
PROCEDURE Or(a, b : T) : T =
  BEGIN
    RETURN NEW(Expr, op := Op.Or, a := a, b := b, nm := F("(| %s %s)", a.nm, b.nm), x := BDD.Or(a.x, b.x))
  END Or;
  
PROCEDURE Not(a : T) : T =
  BEGIN RETURN NEW(Expr, op := Op.Not, a := a, b := NIL, nm := F("(~ %s)", a.nm), x := BDD.Not(a.x)) END Not;
  
PROCEDURE New(nm : TEXT) : T =
  BEGIN
    Debug.Out("Creating New literal GlitchExpr " & nm);
    RETURN NEW(Named, nm := nm, x := BDD.New())
  END New;

PROCEDURE Eval(x     : T;
               tab   : Text01XTbl.T;
               gates : TextGlitchExprTbl.T) : ZeroOneX.T =
  TYPE
    V = ZeroOneX.T;
  BEGIN
    TYPECASE x OF
      Expr(x) =>
      CASE x.op OF
        Op.And =>
        WITH av = Eval(x.a, tab, gates), bv = Eval(x.b, tab, gates) DO
          IF av = V.V0 OR bv = V.V0 THEN
            RETURN V.V0
          ELSIF av = V.VX OR bv = V.VX THEN
            RETURN V.VX
          ELSE
            RETURN V.V1
          END
        END
      |
        Op.Or =>
        WITH av = Eval(x.a, tab, gates), bv = Eval(x.b, tab, gates) DO
          IF av = V.V1 OR bv = V.V1 THEN
            RETURN V.V1
          ELSIF av = V.VX OR bv = V.VX THEN
            RETURN V.VX
          ELSE
            RETURN V.V0
          END
        END
      |
        Op.Not =>
        WITH av = Eval(x.a, tab, gates) DO
          IF av = V.VX THEN RETURN V.VX ELSE RETURN VAL(1-ORD(av),V) END
        END
      END
    |
      Named(n) =>
      VAR
        v : V;
        g : T;
      BEGIN
        IF tab.get(n.nm, v) THEN
          RETURN v
        ELSIF gates.get(n.nm, g) THEN
          RETURN Eval(g, tab, gates)
        ELSE
          Debug.Error(F("no value provided for %s", n.nm));
          <*ASSERT FALSE*>
        END
      END
    ELSE
      <*ASSERT FALSE*>
    END
  END Eval;

PROCEDURE Fanins(t : T) : TextSet.T =
  VAR
    res := NEW(TextSetDef.T).init();

  PROCEDURE Recurse(q : T) =
    BEGIN
      TYPECASE q OF
        Named(n) => EVAL res.insert(n.nm)
      |
        Expr(x) =>
        Recurse(x.a);
        IF x.op # Op.Not THEN Recurse(x.b) END
      ELSE
        <*ASSERT FALSE*>
      END
    END Recurse;
    
  BEGIN
    Recurse(t);
    RETURN res
  END Fanins;
    
BEGIN END GlitchExpr.
