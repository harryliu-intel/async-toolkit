MODULE Glitch;

(* glitch checking program

   Author : Mika Nystrom <mika.nystroem@intel.com>
   October 27, 2021
*)

(* the main entry point is in this file, it's called RunChecks() 

   The basic algorithm is as follows:

   for every output
     for every async_input in the logic fanin cone of such output
       compute insensitivity states (in terms of other inputs) of this output to the async_input

       traverse each insensitivity state separately, and perform a 0-1-X 
       simulation with the async_input set to X

       if the output is X, it is glitchy.
       if the output is still insensitive to the async_input's being X, it is
       not in fact glitchy

   if ANY output is glitchy in ANY async input, we fail and print a useful
   message.  The Main program will quit with Unix status non-zero in such
   cases.
*)

IMPORT GlitchExpr;
IMPORT TextSet, TextSetDef;
IMPORT TextGlitchExprTbl AS TextExprTbl;
FROM Fmt IMPORT F, FN, Int, Bool;
IMPORT Debug;
IMPORT BDDTextTbl;
IMPORT BDD;
IMPORT BDDDepends;
IMPORT BDDSet;
IMPORT ZeroOneX;
IMPORT Text01XTbl;
IMPORT GlitchExprEval;
IMPORT Wr;
IMPORT Stdio;
IMPORT GlitchGate;
IMPORT TextGlitchGateListTbl;
IMPORT GlitchGateList;
IMPORT Thread;
IMPORT TextBDDTbl;
IMPORT Text;
IMPORT TextSubsets;
IMPORT Wx;
IMPORT SopBDD;
IMPORT Word;
IMPORT BDDBDDTbl;

CONST TE = Text.Equal;

<*FATAL Thread.Alerted*>

VAR doDebug := Debug.GetLevel() >= 10;
    
PROCEDURE Output(nm : TEXT) =
  BEGIN
    EVAL outputs.insert(nm);
  END Output;

PROCEDURE Async(nm : TEXT) =
  BEGIN
    EVAL asyncs.insert(nm);
  END Async;

PROCEDURE Gate(tgt : TEXT; expr : GlitchExpr.T) =
  VAR
    tgtBdd  := BDD.New();
    newGate :=  NEW(GlitchGate.T,
                    tgt    := tgt, 
                    expr   := expr,
                    tgtBdd := tgtBdd);
  BEGIN
    gateList := GlitchGateList.Cons(newGate, gateList);

    WITH hadIt = gates.put(tgt, expr) DO
      IF hadIt THEN
        Debug.Error("duplicate gate for name " & tgt)
      END
    END;
    
    WITH hadIt = revbdds.put(tgtBdd, tgt) DO
      IF hadIt THEN
        Debug.Error("duplicate BDD for gate name " & tgt)
      END
    END;

    WITH set  = GlitchExpr.Fanins(expr),
         iter = set.iterate() DO
      VAR
        nn : TEXT;
        gl : GlitchGateList.T := NIL;
      BEGIN
        WHILE iter.next(nn) DO
          EVAL fanouts.get(nn, gl);
          gl := GlitchGateList.Cons(newGate, gl);
          EVAL fanouts.put(nn, gl)
        END
      END
    END
  END Gate;

PROCEDURE Literal(nm : TEXT; expr : GlitchExpr.T) =
  BEGIN
    WITH hadIt = literals.put(nm, expr) DO
      IF hadIt THEN
        Debug.Error("duplicate literal for name " & nm)
      END
    END;
    
    WITH hadIt = revbdds.put(expr.x, nm) DO
      IF hadIt THEN
        Debug.Error("duplicate BDD for literal name " & nm)
      END
    END
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

VAR
  asyncs, outputs             := NEW(TextSetDef.T).init();

  gateList : GlitchGateList.T := NIL;
  gates, literals             := NEW(TextExprTbl.Default).init();
  revbdds                     := NEW(BDDTextTbl.Default).init();
  fanouts                     := NEW(TextGlitchGateListTbl.Default).init();
  
PROCEDURE RunChecks(asyncLimit : CARDINAL) : BOOLEAN =
  VAR
    oIter := outputs.iterate();
    o : TEXT;
    x : GlitchExpr.T;
  BEGIN
    IF asyncs.size() > asyncLimit THEN
      Debug.Error(F("More than %s async inputs, giving up!", Int(asyncLimit)))
    END;
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
    END;
    Wr.PutText(Stdio.stderr, Int(runs) & " sync states examined\n");
    RETURN ok
  END RunChecks;

PROCEDURE GetInsensitivity(of, wrt : BDD.T) : BDD.T =
  BEGIN
    RETURN
      BDD.Equivalent(BDD.MakeTrue(of, wrt), BDD.MakeFalse(of, wrt))
  END GetInsensitivity;
  
PROCEDURE DoOutput(o : TEXT; x : GlitchExpr.T) =
  VAR
    flatX := FlatExpression(x.x);
    deps  := BDDDepends.Depends(flatX);

    asyncdeps : BDDSet.T;
    
  BEGIN
    Debug.Out(F("output %s depends on %s literals",
                o, Int(deps.size())));
    VAR
      iter := deps.iterate();
      b : BDD.T;
    BEGIN
      WHILE iter.next(b) DO
        WITH name    = GetName(b),
             isAsync = asyncs.member(name) DO
          Debug.Out(F("output %s <- input %s, async %s",
                      o, name, Bool(isAsync)));

          IF isAsync THEN

            DoAsync2(o, name)
          END
          
        END
      END
    END
  END DoOutput;

PROCEDURE DoAsync2(output, async : TEXT) =
  BEGIN
    Debug.Out(F("DoAsync2(%s, %s)", output, async));
    WITH chainedS = ChainedSensitivity(output, async),
         globalS  = GlobalSensitivity(output, async),
         delta    = BDD.Not(BDD.Equivalent(chainedS, globalS)) DO
      IF chainedS # globalS THEN
        ok := FALSE;
          Wr.PutText(Stdio.stderr,
                     F("FOUND GLITCH: output %s <- async input %s\n",
                       output,
                       async));
          Wr.PutText(Stdio.stderr,
                     "BEGIN STATE DUMP >>>>>>>>>>>>>>>>>>>>>>>\n");
          Wr.PutText(Stdio.stderr, F("chainedS = %s, globalS = %s, delta %s\n",
                                     PrettyBdd(chainedS),
                                     PrettyBdd(globalS),
                                     PrettyBdd(delta)));
          Wr.PutText(Stdio.stderr,
                     "END < STATE DUMP <<<<<<<<<<<<<<<<<<<<<<<\n");
        
      END
    END
  END DoAsync2;

PROCEDURE ComputeSensitivities(set        : TextSet.T;
                               wrt        : TEXT;
                               (*OUT*)map : TextBDDTbl.T) =
  VAR
    iter     := set.iterate();
    n        : TEXT;
    dummy    : BDD.T;
  BEGIN
    WHILE iter.next(n) DO
      IF NOT map.get(n, dummy) THEN
        EVAL map.put(n, ChainedSensitivity(n, wrt))
      END
    END
  END ComputeSensitivities;

PROCEDURE GlobalSensitivity(of, wrt : TEXT) : BDD.T =
  VAR
    x   : GlitchExpr.T;
  BEGIN
    Debug.Out(F("GlobalSensitivity(of = %s, wrt = %s)", of, wrt));

    IF TE(of, wrt) THEN
      (* wrt always depends on itself *)
      RETURN True
    ELSIF NOT gates.get(of, x) THEN
      (* other ultimate inputs do not depend on wrt *)
      RETURN False
    ELSE
      (* x is the glitchexpr that we care about *)
      VAR
        wrtB    : BDD.T;
        hadIt   := LookupBdd(wrt, wrtB);
        flatOf  := FlatExpression(x.x);

        flatOfF, flatOfT, globalSens : BDD.T;
      BEGIN
        <*ASSERT hadIt*>

        flatOfF := BDD.MakeFalse(flatOf, wrtB);
        flatOfT := BDD.MakeTrue(flatOf, wrtB);

        globalSens := BDD.Not(BDD.Equivalent(flatOfF, flatOfT));

        IF doDebug THEN
          Debug.Out(F("Global sensitivity of %s wrt %s : %s",
                      of, wrt, 
                      PrettyBdd(globalSens)))
        END;
        RETURN globalSens
      END
    END
  END GlobalSensitivity;

PROCEDURE ChainedSensitivity(of, wrt : TEXT) : BDD.T =
  VAR
    x   : GlitchExpr.T;
    res : BDD.T;
  BEGIN
    Debug.Out(F("ChainedSensitivity(of = %s, wrt = %s)", of, wrt));
    
    IF TE(of, wrt) THEN
      (* wrt always depends on itself *)
      res := True
    ELSIF NOT gates.get(of, x) THEN
      (* other ultimate inputs do not depend on wrt *)
      res := False
    ELSE
      (* x is the glitchexpr that we care about *)
      res := False;
      VAR
        fanins  := GlitchExpr.Fanins(x);
        sens    := NEW(TextBDDTbl.Default).init();
        wrtB    : BDD.T;
        hadIt   := LookupBdd(wrt, wrtB);
        flatOf  := FlatExpression(x.x);

        flatOfF, flatOfT, globalSens : BDD.T;
        
      BEGIN
        <*ASSERT hadIt*>

        flatOfF := BDD.MakeFalse(flatOf, wrtB);
        flatOfT := BDD.MakeTrue(flatOf, wrtB);

        globalSens := BDD.Not(BDD.Equivalent(flatOfF, flatOfT));

        IF doDebug THEN
          Debug.Out(F("Global sensitivity of %s wrt %s : %s",
                      of, wrt, 
                      PrettyBdd(globalSens)));
          Debug.Out(F("ChainedSensitivity of %s : fanins %s", of, FmtTextSet(fanins)));
          Debug.Out(F("ChainedSensitivity expr %s <- %s", of, PrettyBdd(x.x)))
        END;
        
        ComputeSensitivities(fanins, wrt, sens);

        VAR
          iter := fanins.iterate();
          fi : TEXT;
          fiSens : BDD.T;
        BEGIN
          WHILE iter.next(fi) DO
            WITH fiB    = MustLookupBdd(fi),
                 fT     = BDD.MakeTrue(x.x, fiB),
                 ffT    = FlatExpression(fT),
                 fF     = BDD.MakeFalse(x.x, fiB),
                 ffF    = FlatExpression(fF),
                 localS = BDD.Not(BDD.Equivalent(ffF, ffT)),
                 hadIt  = sens.get(fi, fiSens),
                 chainS = BDD.And(localS, fiSens) DO
              <*ASSERT hadIt*>
              IF doDebug THEN
                Debug.Out(FN("Fanin of %s|%s<-T = %s = %s, %s|%s<-F = %s = %s, localS = %s",
                             ARRAY OF TEXT{ of, fi, PrettyBdd(fT), PrettyBdd(ffT),
                                            of, fi, PrettyBdd(fF), PrettyBdd(ffF),
                                            PrettyBdd(localS) }));
                Debug.Out(F("sens of %s / %s = fiSens %s; localS & fiSens = %s ",
                            fi,
                            wrt,
                            PrettyBdd(fiSens),
                            PrettyBdd(chainS)
                ))
              END;

              res := BDD.Or(res, chainS)
            END
          END
        END
      END
    END;
    IF doDebug THEN
      Debug.Out(F("Done! Chained sensitivity(of = %s, wrt = %s) = %s",
                  of,
                  wrt,
                  PrettyBdd(res)))
    END;
    RETURN res
  END ChainedSensitivity;

PROCEDURE FmtTextSet(s : TextSet.T) : TEXT =
  VAR
    wx := Wx.New();
    t : TEXT;
    iter := s.iterate();
    first := TRUE;
  BEGIN
    Wx.PutText(wx, "{");
    WHILE iter.next(t) DO
      IF first THEN
        first := FALSE;
        Wx.PutText(wx, " ")
      ELSE
        Wx.PutText(wx, ", ")
      END;
      Wx.PutText(wx, t);
    END;
    Wx.PutText(wx, " }");
    RETURN Wx.ToText(wx)
  END FmtTextSet;
    
TYPE
  Binding = RECORD
    b : BDD.T;
    v : BOOLEAN;
  END;

VAR ok := TRUE;
    runs : CARDINAL := 0;

PROCEDURE DoAsync(o                : TEXT;
                  x                : GlitchExpr.T;
                  flatX, b, insens : BDD.T;
                  deps             : BDDSet.T) =
  (* do async checks on o/x w.r.t. b *)

  CONST
    PrintInterval = 100000;
    
  PROCEDURE RunOne() =
    BEGIN
      INC(runs);

      IF runs MOD PrintInterval = 0 THEN
        TRY
          Wr.PutText(Stdio.stderr, Int(runs));
          Wr.PutChar(Stdio.stderr, '\n')
        EXCEPT ELSE
        END
      END;
      
      FOR i := FIRST(bindings^) TO LAST(bindings^) DO
        EVAL tab.put(GetName(bindings[i].b),
                     ZeroOneX.FromBool(bindings[i].v))
      END;

      (* set up environment *)
      WITH val = GlitchExprEval.Eval(x, tab, gates) DO
        Debug.Out(F("RunOne : result is %s", ZeroOneX.Format(val)));

        IF val = V.VX THEN
          ok := FALSE;
          Wr.PutText(Stdio.stderr,
                     F("FOUND GLITCH: output %s <- async input %s\n",
                       o,
                       GetName(b)));
          Wr.PutText(Stdio.stderr,
                     "BEGIN STATE DUMP >>>>>>>>>>>>>>>>>>>>>>>\n");
          FOR i := FIRST(bindings^) TO LAST(bindings^) DO
            Wr.PutText(Stdio.stderr, F("var %s val %s\n",
                                       GetName(bindings[i].b),
                                       Bool(bindings[i].v)))
          END;
          Wr.PutText(Stdio.stderr,
                     "END < STATE DUMP <<<<<<<<<<<<<<<<<<<<<<<\n");
        END
      END
    END RunOne;

  TYPE
    V = ZeroOneX.T;
  VAR
    indeps   := deps.copy();
    bindings := NEW(REF ARRAY OF Binding, deps.size() - 1);
    tab      := NEW(Text01XTbl.Default).init();
  BEGIN
    WITH hadIt = indeps.delete(b) DO <*ASSERT hadIt*> END;
    
    Debug.Out(F("DoAsync %s <- %s", o, BDD.Format(b, revbdds)));
    
    Debug.Out(F("insensitivity expr %s",
                BDD.Format(insens, NIL)));

    TRY
      Wr.PutText(Stdio.stderr, F("%s bindings\n", Int(NUMBER(bindings^))));
    EXCEPT ELSE END;

    (* now build indeps array *)

    EVAL tab.put(GetName(b), V.VX);
    
    WITH iter     = indeps.iterate() DO
      FOR i := FIRST(bindings^) TO LAST(bindings^) DO
        EVAL iter.next(bindings[i].b)
      END;
      MakeBindings(insens, bindings^, 0, RunOne)
    END
  END DoAsync;

PROCEDURE PrettyBdd(b : BDD.T) : TEXT =
  BEGIN
    RETURN SopBDD.ConvertBool(b).invariantSimplify(True, True, True)
             .format(revbdds)
  END PrettyBdd;
  
PROCEDURE MakeBindings(insens       : BDD.T;
                       VAR bindings : ARRAY OF Binding;
                       ptr          : CARDINAL;
                       p            : PROCEDURE() ) =
  BEGIN
    IF ptr = NUMBER(bindings) THEN
      (* base case *)
      IF doDebug THEN
        Debug.Out("Hit base case in MakeBindings");
        IF doDebug THEN
          FOR i := FIRST(bindings) TO LAST(bindings) DO
            Debug.Out(F("XXX %s (%s) <- %s",
                        BDD.Format(bindings[i].b),
                        PrettyBdd(bindings[i].b),
                        Bool(bindings[i].v)))
          END
        END
      END;
      p()
    ELSE
      FOR v := FIRST(BOOLEAN) TO LAST(BOOLEAN) DO
        WITH nxt = BDDMake(insens, bindings[ptr].b, v) DO
          IF nxt # False THEN
            bindings[ptr].v := v;
            MakeBindings(nxt, bindings, ptr + 1, p)
          END
        END
      END
    END
  END MakeBindings;

PROCEDURE BDDMake(b : BDD.T; lit : BDD.T; val : BOOLEAN) : BDD.T =
  BEGIN
    CASE val OF
      FALSE => RETURN BDD.MakeFalse(b, lit)
    |
      TRUE  => RETURN BDD.MakeTrue(b, lit)
    END
  END BDDMake;

PROCEDURE GetName(b : BDD.T) : TEXT =
  VAR
    nm : TEXT;
  BEGIN
    IF revbdds.get(b, nm) THEN RETURN nm ELSE <*ASSERT FALSE*> END
  END GetName;

PROCEDURE DbgExpr(x : BDD.T) : TEXT =
  VAR
    nm : TEXT;
  BEGIN
    WITH hadIt = revbdds.get(x, nm) DO
      IF hadIt THEN
        RETURN nm
      ELSE
        RETURN "*anon*"
      END
    END
  END DbgExpr;

VAR flatTbl := NEW(BDDBDDTbl.Default).init(); (* memoization table *)

PROCEDURE FlatExpression(b : BDD.T) : BDD.T =
  (* take a gate expression and convert it to being in the ultimate
     inputs *)
  VAR
    changed : BOOLEAN;
    c := b;
    d, f : BDD.T;
    g : GlitchExpr.T;
  BEGIN
    (* while there are pieces of the expression that haven't been 
       replaced, replace *)

    IF flatTbl.get(b, f) THEN
      RETURN f
    ELSE
      Debug.Out(F("flattening"));
      
      REPEAT
        changed := FALSE;
        WITH deps = BDDDepends.Depends(c),
             iter = deps.iterate() DO
          
          WHILE iter.next(d) DO
            WITH nam = GetName(d) DO
              IF gates.get(nam, g) THEN
                (* do the Shannon cofactor expansion w.r.t. the actual 
                   expression in g for the literal d *)
                WITH flatGx = FlatExpression(g.x) DO
                  IF doDebug THEN
                    Debug.Out(F("replacing %s with gate expression %s (%s)",
                                nam,
                                PrettyBdd(g.x),
                                PrettyBdd(flatGx)))
                  END;
                  WITH new = BDD.Or(
                                 BDD.And(flatGx,
                                         BDD.MakeTrue(c, d)),
                                 BDD.And(BDD.Not(flatGx),
                                         BDD.MakeFalse(c, d))) DO
                    <*ASSERT new # c*>
                    c := new
                  END
                END;
                changed := TRUE;
                EXIT
              END
            END
          END
        END
      UNTIL NOT changed;

      EVAL flatTbl.put(b, c);
      
      RETURN c
    END
  END FlatExpression;

VAR False := BDD.False();
    True  := BDD.True();

PROCEDURE MustLookupBdd(nm : TEXT) : BDD.T =
  VAR
    b : BDD.T;
    hadIt := LookupBdd(nm, b);
  BEGIN
    <*ASSERT hadIt*>
    RETURN b
  END MustLookupBdd;
  
PROCEDURE LookupBdd(nm : TEXT; VAR res : BDD.T) : BOOLEAN =
  VAR
    gx : GlitchExpr.T;
  BEGIN
    IF literals.get(nm, gx) OR gates.get(nm, gx) THEN
      res := gx.x;
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END LookupBdd;

PROCEDURE TextSetToBddArr(s : TextSet.T) : REF ARRAY OF BDD.T =
  VAR
    res := NEW(REF ARRAY OF BDD.T, s.size());
    iter := s.iterate();
    j := 0;
    nm : TEXT;
  BEGIN
    WHILE iter.next(nm) DO
      WITH hadIt = LookupBdd(nm, res[j]) DO
        <*ASSERT hadIt*>
      END;
      INC(j)
    END;
    <*ASSERT j = NUMBER(res^)*>
    RETURN res
  END TextSetToBddArr;

BEGIN
END Glitch.
