MODULE Glitch;
IMPORT GlitchExpr;
IMPORT TextSet, TextSetDef;
IMPORT RefList;
IMPORT TextGlitchExprTbl AS TextExprTbl;
FROM Fmt IMPORT F, Int, Bool;
IMPORT Debug;
IMPORT BDDTextTbl;
IMPORT BDD;
IMPORT BDDDepends;
IMPORT BDDSet;


PROCEDURE Output(nm : TEXT) =
  BEGIN
    EVAL outputs.insert(nm);
  END Output;

PROCEDURE Async(nm : TEXT) =
  BEGIN
    EVAL asyncs.insert(nm);
  END Async;

PROCEDURE Gate(tgt : TEXT; expr : GlitchExpr.T) =
  BEGIN
    gateList := RefList.Cons(NEW(GateRec, tgt := tgt, expr := expr),
                             gateList);
    EVAL gates.put(tgt, expr);
    EVAL revbdds.put(expr.x, tgt)
  END Gate;

PROCEDURE Literal(nm : TEXT; expr : GlitchExpr.T) =
  BEGIN
    EVAL literals.put(nm, expr);
    EVAL revbdds.put(expr.x, nm);
  END Literal;

PROCEDURE GetLiteral(nm : TEXT) : GlitchExpr.T =
  VAR
    expr : GlitchExpr.T;
  BEGIN
    IF NOT literals.get(nm, expr) THEN
      expr := GlitchExpr.New(nm := nm);
      Literal(nm, expr)
    END;
    RETURN expr
  END GetLiteral;

TYPE
  GateRec = OBJECT
    tgt  : TEXT;
    expr : GlitchExpr.T;
  END;

VAR
  asyncs, outputs := NEW(TextSetDef.T).init();

  gateList : RefList.T := NIL;
  gates, literals      := NEW(TextExprTbl.Default).init();
  revbdds          := NEW(BDDTextTbl.Default).init();
  
PROCEDURE RunChecks() =
  VAR
    oIter := outputs.iterate();
    o : TEXT;
    x : GlitchExpr.T;
  BEGIN
    (* for each output, check its dependence on each async input *)
    WHILE oIter.next(o) DO
      Debug.Out("Output " & o);
      IF literals.get(o, x) THEN
        Debug.Out(F("output %s is literal", o))
      ELSIF gates.get(o, x) THEN
        Debug.Out(F("output %s is driven by gate %s", o, x.nm));
        DoOutput(o, x)
      ELSE
        Debug.Out(F("output %s not found", o))
      END;

      
    END
              
  END RunChecks;

PROCEDURE GetInsensitivity(of, wrt : BDD.T) : BDD.T =
  BEGIN
    RETURN
      BDD.Equivalent(BDD.MakeTrue(of, wrt), BDD.MakeFalse(of, wrt))
  END GetInsensitivity;
  
PROCEDURE DoOutput(o : TEXT; x : GlitchExpr.T) =
  VAR
    flatX := FlatExpression(x.x);
    deps := BDDDepends.Depends(flatX);

    asyncdeps : BDDSet.T;
    
  BEGIN
    Debug.Out(F("output %s depends on %s literals",
                o, Int(deps.size())));
    VAR
      iter := deps.iterate();
      b : BDD.T;
    BEGIN
      WHILE iter.next(b) DO
        WITH name   = GetName(b),
             isAsync = asyncs.member(name) DO
          Debug.Out(F("output %s <- input %s, async %s",
                      o, name, Bool(isAsync)));

          IF isAsync THEN
            (* ok input is async, do something... *)
            WITH insens = GetInsensitivity(flatX, b) DO
              Debug.Out(F("insensitivity expr %s",
                          BDD.Format(insens, NIL)))
            END
          END
          
        END
      END
    END
  END DoOutput;

PROCEDURE GetName(b : BDD.T) : TEXT =
  VAR
    nm : TEXT;
  BEGIN
    IF revbdds.get(b, nm) THEN RETURN nm ELSE <*ASSERT FALSE*> END
  END GetName;

PROCEDURE FlatExpression(b : BDD.T) : BDD.T =
  (* take a gate expression and convert it to being in the ultimate
     inputs *)
  VAR
    changed : BOOLEAN;
    c := b;
    d : BDD.T;
    g : GlitchExpr.T;
  BEGIN
    (* while there are pieces of the expression that haven't been 
       replaced, replace *)

    REPEAT
      changed := FALSE;
      WITH deps = BDDDepends.Depends(c),
           iter = deps.iterate() DO
        WHILE iter.next(d) DO
          IF gates.get(GetName(d), g) THEN
            (* do the Shannon cofactor expansion w.r.t. the actual 
               expression in g for the literal d *)
            c := BDD.Or(
                     BDD.And(g.x,
                             BDD.MakeTrue(c, d)),
                     BDD.And(BDD.Not(g.x),
                             BDD.MakeFalse(c, d)));
            changed := TRUE
          END
        END
      END
    UNTIL NOT changed;

    RETURN c
  END FlatExpression;
  
BEGIN END Glitch.
