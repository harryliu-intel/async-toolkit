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
IMPORT Wx;
IMPORT SopBDD;
IMPORT BDDBDDTbl;
IMPORT Time;
IMPORT TextIntTbl;
IMPORT TextIntPair;
IMPORT TextIntPairArraySort;
IMPORT TextList;
IMPORT TextRefTbl;

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
    Debug.Out(F("Glitch.Literal(%s)", nm));
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
  
PROCEDURE RunChecks(asyncLimit, alg1MaxDepth : CARDINAL) : BOOLEAN
  RAISES { Timeout } =
  VAR
    oIter := outputs.iterate();
    o : TEXT;
    x : GlitchExpr.T;
  BEGIN
    (* for each output, check its dependence on each async input *)
    count := CountZero;
    WHILE oIter.next(o) DO
      Debug.Out("Output " & o);
      IF gates.get(o, x) THEN
        Debug.Out(F("output %s is driven by gate %s", o, x.nm));
        DoOutput(o, x, asyncLimit, alg1MaxDepth)
      ELSIF literals.get(o, x) THEN
        Debug.Out(F("output %s is literal", o))
      ELSE
        Debug.Out(F("output %s not found", o))
      END;
    END;
    FOR i := FIRST(count) TO LAST(count) DO
      Wr.PutText(Stdio.stderr, F("Glitch.RunChecks method %s count %s\n",
                                 Int(i), Int(count[i])))
    END;
    RETURN ok
  END RunChecks;

<*UNUSED*>
PROCEDURE GetInsensitivity(of, wrt : BDD.T) : BDD.T =
  BEGIN
    RETURN
      BDD.Equivalent(BDD.MakeTrue(of, wrt), BDD.MakeFalse(of, wrt))
  END GetInsensitivity;

<*UNUSED*>
PROCEDURE DoOutputOld(o : TEXT; x : GlitchExpr.T; asyncLimit : CARDINAL)
  RAISES { Timeout } =
  VAR
    flatX := FlatExpression(x.x);
    deps  := BDDDepends.Depends(flatX);

  BEGIN
    (* this is not quite right

       what we should do is run the output against every async input provided,
       instead of flattening and figuring out what it seemingly depends on.
    *)
    
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
  END DoOutputOld;

TYPE Method = [1 .. 2];
VAR count : ARRAY Method OF CARDINAL;
CONST CountZero = ARRAY Method OF CARDINAL { 0, .. };
      
PROCEDURE DoOutput(o : TEXT;
                   x : GlitchExpr.T;
                   asyncLimit, alg1MaxDepth : CARDINAL)
  RAISES { Timeout } =
  VAR
    iter := asyncs.iterate();
    wrt : TEXT;
  BEGIN
    WHILE iter.next(wrt) DO
      WITH quick = Quick(o, wrt, alg1MaxDepth, FALSE) DO
        IF quick = 0 OR quick = 1 THEN
          INC(count[1])
        ELSE
          EVAL Quick(o, wrt, alg1MaxDepth, TRUE); (* debug output *)
          IF asyncs.size() > asyncLimit THEN
            Debug.Error(F("Glitch.DoOutput: Algorithm 1 failed (%s <- %s), and more than %s async inputs, giving up!",
                          o,
                          wrt,
                          Int(asyncLimit)))
          END;
          DoAsync2(o, wrt);
          INC(count[2])
        END
      END
    END
  END DoOutput;

TYPE
  QuickResult = [ -1 .. LAST(CARDINAL) ];

VAR
  sf := NEW(TextSetDef.T).init();

PROCEDURE FmtList(lst : TextList.T) : TEXT =
  VAR
    wx := Wx.New();
    p := lst;
  BEGIN
    Wx.PutText(wx, "{ ");
    WHILE p # NIL DO
      Wx.PutText(wx, p.head);
      Wx.PutChar(wx, ' ');
      p := p.tail
    END;
    Wx.PutChar(wx, '}');
    RETURN Wx.ToText(wx)
  END FmtList;
  
PROCEDURE Quick(ultimate, async : TEXT;
                MaxDepth        : CARDINAL;
                trackFanins     : BOOLEAN) : QuickResult =

  PROCEDURE Recurse(output : TEXT; depth : CARDINAL) : QuickResult =
    (* return # of paths from async to output *)
    VAR
      x : GlitchExpr.T;
    BEGIN
      IF    depth > MaxDepth THEN
        Debug.Warning(F("Quick.Recurse : hit max depth (>%s) for %s <- %s",
                        Int(MaxDepth), output, async));
        RETURN -1
      ELSIF TE(output, async) THEN
        Debug.Out(F("Quick (%s, %s) = 1", output, async));
        RETURN 1
      ELSIF gates.get(output, x) THEN
        VAR
          fSet := GlitchExpr.Fanins(x);
          iter := fSet.iterate();
          fi   : TEXT;
          sum  := 0;
        BEGIN
          WHILE iter.next(fi) DO
            <*ASSERT output # NIL*>
            <*ASSERT async # NIL*>
            <*ASSERT fi # NIL*>
            Debug.Out(F("Quick (%s, %s), looking at fanin %s",
                        output, async, fi));
            IF TE(output, fi) THEN
              IF NOT sf.insert(output) THEN
                Debug.Warning("Algorithm 1 skipping self-fanin: " & output)
              END
            ELSE
              IF trackFanins THEN
                VAR
                  fiDepth := 0;
                  lst : REFANY;
                BEGIN
                  WITH hadIt = depths.get(fi, fiDepth) DO
                    IF hadIt THEN
                      EVAL repeats.insert(fi)
                    ELSE
                      fiDepth := depth
                    END
                  END;
                  fiDepth := MIN(fiDepth, depth);
                  EVAL depths.put(fi, fiDepth);
                  EVAL fanouts.get(fi, lst);
                  IF NOT TextList.Member(lst, output) THEN
                    lst := TextList.Cons(output, lst);
                    EVAL fanouts.put(fi, lst)
                  END
                END
              END;
              WITH this = Recurse(fi , depth + 1) DO
                IF this = -1 THEN
                  Debug.Warning("path through " & output);
                  RETURN -1
                ELSE
                  sum := sum + this
                END
              END
            END
          END;
          Debug.Out(F("Quick /sum/ (%s, %s) = %s", output, async, Int(sum)));
          RETURN sum
        END
      ELSIF literals.get(output, x) THEN
        Debug.Out(F("Quick (%s, %s) = 0", output, async));
        RETURN 0
      ELSE
        <*ASSERT FALSE*>
      END
    END Recurse;

  VAR
    depths  : TextIntTbl.T;
    repeats : TextSet.T;
    fanouts : TextRefTbl.T;
  BEGIN
    IF trackFanins THEN
      depths  := NEW(TextIntTbl.Default).init();
      repeats := NEW(TextSetDef.T).init();
      fanouts := NEW(TextRefTbl.Default).init();
      EVAL depths.put(ultimate, 0)
    END;
    
    Debug.Out(F("Quick(%s <- %s)", ultimate, async), 1);
    WITH res = Recurse(ultimate, 0) DO
      Debug.Out("Quick result " & Int(res), 1);
      IF trackFanins AND repeats.size() # 0 THEN
        VAR
          iter := repeats.iterate();
          rn : TEXT;
          dp : INTEGER;
          arr := NEW(REF ARRAY OF TextIntPair.T, repeats.size());
          j := 0;
          lst : REFANY;
        BEGIN
          WHILE iter.next(rn) DO
            WITH hadIt = depths.get(rn, dp) DO
              <*ASSERT hadIt*>
              arr[j] := TextIntPair.T { rn, dp };
              INC(j)
            END
          END;
          <*ASSERT j = NUMBER(arr^)*>
          TextIntPairArraySort.Sort(arr^, TextIntPair.CompareK2K1);
          FOR i := FIRST(arr^) TO LAST(arr^) DO
            WITH s = arr[i] DO
              EVAL fanouts.get(s.k1, lst);
              Debug.Out(F("Repeated node %s -> %s depth %s",
                          s.k1,
                          FmtList(lst),
                          Int(s.k2)), 1)
            END
          END
        END
      END;
      RETURN res
    END
  END Quick;
  
PROCEDURE DoAsync2(output, async : TEXT)
  RAISES { Timeout } =
  <*FATAL Wr.Failure*>
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
                               (*OUT*)map : TextBDDTbl.T)
  RAISES { Timeout } =
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

PROCEDURE GlobalSensitivity(of, wrt : TEXT) : BDD.T
  RAISES { Timeout } =
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

PROCEDURE ChainedSensitivity(of, wrt : TEXT) : BDD.T
  RAISES { Timeout } =
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

<*UNUSED*>    
PROCEDURE DoAsync(o                : TEXT;
                  x                : GlitchExpr.T;
                  flatX, b, insens : BDD.T;
                  deps             : BDDSet.T)
  RAISES { Timeout } =
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

PROCEDURE PrettyBdd(b : BDD.T; timeo : Time.T := 5.0d0) : TEXT
  RAISES { Timeout } =
  BEGIN
    IF prettyTimeout THEN
      RAISE Timeout 
    END;
    
    WITH cl = NEW(PrettyClosure,
                  mu := NEW(MUTEX),
                  b  := b) DO
      IF timeo = 0.0d0 THEN
        EVAL cl.apply();
        RETURN cl.res
      ELSE
        VAR
          start := Time.Now();
          thr   := Thread.Fork(cl);
        BEGIN
          LOOP
            LOCK cl.mu DO
              IF cl.res # NIL THEN
                RETURN Thread.Join(thr)
              END
            END;
            IF Time.Now() - start > timeo THEN
              prettyTimeout := TRUE;
              RAISE Timeout
            END;
            Thread.Pause(0.1d0)
          END
        END
      END
    END
  END PrettyBdd;

VAR prettyTimeout := FALSE;
    
TYPE
  PrettyClosure = Thread.Closure OBJECT
    mu  : MUTEX;
    b   : BDD.T;
    res : TEXT := NIL;
  OVERRIDES
    apply := PCApply;
  END;

PROCEDURE PCApply(cl : PrettyClosure) : REFANY =
  BEGIN
    WITH temp = SopBDD.ConvertBool(cl.b).invariantSimplify(True, True, True)
      .format(revbdds) DO
      LOCK cl.mu DO
        cl.res := temp
      END;
      RETURN temp
    END
  END PCApply;
  
PROCEDURE MakeBindings(insens       : BDD.T;
                       VAR bindings : ARRAY OF Binding;
                       ptr          : CARDINAL;
                       p            : PROCEDURE() )
  RAISES { Timeout } =
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

PROCEDURE FlatExpression(b : BDD.T) : BDD.T
  RAISES { Timeout } =
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
