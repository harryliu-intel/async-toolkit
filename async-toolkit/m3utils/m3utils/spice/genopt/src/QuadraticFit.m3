MODULE QuadraticFit;
IMPORT LRVector;
IMPORT WLRVector;
IMPORT WLRVectorSeq;
IMPORT Debug;
FROM Fmt IMPORT F, LongReal, Int, Bool;
IMPORT LRMatrix2 AS M;
IMPORT NewUOA_M3;
IMPORT NewUOAs;
FROM GenOpt IMPORT FmtP;
IMPORT LRScalarField;

CONST LR = LongReal;

VAR doDebug := Debug.DebugThis("QuadraticFit");
      
REVEAL
  T = Public BRANDED Brand OBJECT
    n      : CARDINAL;
    rho    : LONGREAL;
    clean  : BOOLEAN;
    state  : LRVector.T;
    points : WLRVectorSeq.T;
    LT     : REF M.M;
    Lambda : REF M.M; (* Lambda = L LT *)
    p0     : LRVector.T;
    W      : REF M.M;
    C      : LONGREAL;

    x,xx   : REF M.V; (* scratch *)
    
  METHODS
    doFit() := DoFit;
    evalState(state : LRVector.T (* tentative new state *)) : LONGREAL
          := EvalState;
    evalPt(at : LRVector.T   (* point in space *)   ) : LONGREAL := EvalPt;
    setState(to : LRVector.T) := SetState;
  OVERRIDES
    init       := Init;
    addPoint   := AddPoint;
    pred       := Pred;
    getParams  := GetParams;
    getMinimum := GetMinimum;
  END;

PROCEDURE Qdofs(n : CARDINAL) : CARDINAL =
  BEGIN
    RETURN (n * n + 3 * n + 2) DIV 2
  END Qdofs;

PROCEDURE Init(t : T; n : CARDINAL; rho : LONGREAL) : T =
  BEGIN
    t.n       := n;
    t.rho     := rho;
    t.points  := NEW(WLRVectorSeq.T).init();
    t.state   := NEW(LRVector.T, Qdofs(n));

    (* it's important that the initial state NOT be zero, because that
       leads to degenerate L- and Lambda-matrices *)
    M.SetV(t.state^, 1.0d0); 
    
    t.clean   := FALSE;

    (* allocate the scratch spaces *)
    t.x  := NEW(REF M.V, n);
    t.xx := NEW(REF M.V, n);
    
    RETURN t
  END Init;

PROCEDURE AddPoint(t : T; pa : LRVector.T; y, w : LONGREAL) =
  VAR
    p := LRVector.Copy(pa);
  BEGIN
    <*ASSERT NUMBER(p^) = t.n*>
    t.clean := FALSE;

    WITH wvec = WLRVector.T { v := p, y := y, w := w } DO
      IF doDebug THEN
        Debug.Out("QuadraticFit.AddPoint : " & WLRVector.Format(wvec))
      END;
    
      t.points.addhi(wvec)
    END
  END AddPoint;

TYPE
  StateField = LRScalarField.Default OBJECT
    t : T;
  OVERRIDES
    eval := SFEval;
  END;

PROCEDURE SFEval(sf : StateField; state : LRVector.T) : LONGREAL =
  BEGIN
    WITH res = sf.t.evalState(state) DO
      IF doDebug THEN
        Debug.Out(F("QuadraticFit.SFEval(%s) = %s",
                    FmtP(state), LR(res)))
      END;
      RETURN res
    END
  END SFEval;
  
PROCEDURE DoFit(t : T) =
  VAR
    np := t.points.size();

  BEGIN
    IF doDebug THEN
      Debug.Out("QuadraticFit.DoFit : t.points.size()=" & Int(np))
    END;
    
    t.LT      := NEW(REF M.M, t.n, t.n);
    M.Zero(t.LT^);
    t.Lambda := NEW(REF M.M, t.n, t.n);
    
    (* build V *)
    t.p0 := NEW(LRVector.T, t.n);

    t.W := NEW(REF M.M, np, np);
    M.Zero(t.W^);

    WITH sf   = NEW(StateField, t := t),
         best = NewUOA_M3.Minimize(t.state,
                                   sf,
                                   npt := NUMBER(t.state^) + 2,
                                   rhobeg := 2.0d0   * t.rho,
                                   rhoend := 0.001d0 * t.rho,
                                   maxfun := 1000)
(*
         best = NewUOAs.Minimize(t.state,
                                 sf,
                                 rhobeg := 2.0d0   * t.rho,
                                 rhoend := 0.001d0 * t.rho).f
      
*)
     DO
      IF doDebug THEN
        Debug.Out(F("QuadraticFit.DoFit : best = %s state = %s",
                    LR(best), FmtP(t.state)))
      END;
(*      t.setState(best.x) *)
    END;

    t.clean := TRUE
  END DoFit;

PROCEDURE SetState(t : T; to : LRVector.T) =
  VAR
    j : CARDINAL;
  BEGIN
    IF doDebug THEN
      Debug.Out("QuadraticFit.SetState to=" & FmtP(to));
    END;
    
    <*ASSERT NUMBER(t.state^) = NUMBER(to^)*>
    t.p0^ := SUBARRAY(to^, 0, t.n);
    j := t.n;
    FOR row := 0 TO t.n - 1 DO
      FOR col := 0 TO row DO
        (* note that we're building up the transpose of L = LT here; 
           this is why we index [col, row] *)
        IF col = row THEN
          t.LT[col, row] := to[j] * to[j] (* diagonals are squared *)
        ELSE
          t.LT[col, row] := to[j]
        END;
        INC(j)
      END
    END;
    <*ASSERT j = LAST(to^)*>
    t.C := to[j];

    M.MulTransposeMM(t.LT^, t.LT^, t.Lambda^);

    IF doDebug THEN
      Debug.Out("t.p0 = " & FmtP(t.p0));
      Debug.Out("t.C  = " & LR(t.C));
      Debug.Out("t.LT = \n" & M.FormatM(t.LT^));
      Debug.Out("t.Lambda = \n" & M.FormatM(t.Lambda^))
    END;
    
    t.state := to
  END SetState;
  
PROCEDURE EvalState(t : T; at : LRVector.T) : LONGREAL =
  VAR
    sumSq := 0.0d0;
  BEGIN
    t.setState(at);

    (* the sum of all the squared deviations *)
    
    FOR i := 0 TO t.points.size() - 1 DO
      WITH rec   = t.points.get(i),
           yhat  = t.evalPt(rec.v),
           delta = rec.y - yhat,
           sq    = delta * delta,
           wsq   = rec.w * sq DO
        IF doDebug THEN
          Debug.Out(F("EvalState : v=%s y=%s yhat=%s wsq=%s",
                      FmtP(rec.v), LR(rec.y), LR(yhat), LR(wsq)))
        END;
        sumSq := sumSq + wsq
      END
    END;
    RETURN sumSq
  END EvalState;

PROCEDURE EvalPt(t : T; p : LRVector.T) : LONGREAL =
  BEGIN
    (* (p - p0)T L LT(p - p0) + C *)
    M.SubV(p^, t.p0^, t.x^);
    M.MulMV(t.Lambda^, t.x^, t.xx^);
    WITH dot = M.Dot(t.x^, t.xx^),
         res = dot + t.C DO
      RETURN res
    END
  END EvalPt;
  
PROCEDURE Pred(t : T; p : LRVector.T) : LONGREAL =
  BEGIN
    IF NOT t.clean THEN
      t.doFit()
    END;
    RETURN t.evalPt(p)
  END Pred;

PROCEDURE GetParams(t : T) : LRVector.T =
  BEGIN
    IF doDebug THEN
      Debug.Out("QuadraticFit.GetParams : t.clean=" & Bool(t.clean))
    END;
    IF NOT t.clean THEN
      t.doFit()
    END;
    RETURN LRVector.Copy(t.state)
  END GetParams;

PROCEDURE GetMinimum(t : T) : LRVector.T =
  VAR
    res := NEW(LRVector.T, t.n);
  BEGIN
    IF doDebug THEN
      Debug.Out("QuadraticFit.GetMinimum : t.clean=" & Bool(t.clean))
    END;
    IF NOT t.clean THEN
      t.doFit()
    END;
    res^ := SUBARRAY(t.state^, 0, t.n);
    RETURN res
  END GetMinimum;

BEGIN END QuadraticFit.
