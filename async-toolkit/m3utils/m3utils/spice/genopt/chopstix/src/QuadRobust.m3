MODULE QuadRobust;

(* Robust parallel minimization.

   We maintain a point p where we search.

   On each iteration, we perform line searches in various directions.

   The best result so far is accepted at the next point.

   Author : mika.nystroem@intel.com
   September, 2024
*)

FROM NewUOAs IMPORT Output;
IMPORT LineMinimizer;
IMPORT LRVector, LRScalarField;
IMPORT LRVectorField;
IMPORT RandomVector;
IMPORT Random;
IMPORT LRMatrix2 AS M;
IMPORT Math;
IMPORT Debug;
FROM Fmt IMPORT LongReal, F, Int, Bool, FN;
IMPORT LineProblem;
IMPORT LineProblemArraySort;
IMPORT LRScalarFieldPll;
IMPORT LRVectorFieldPll;
IMPORT LongRealSeq AS LRSeq;
FROM GenOpt IMPORT rho, iter, ResultWriter;
FROM GenOptUtils IMPORT FmtP;
IMPORT MultiEvalLRVector;
IMPORT LRVectorSet, LRVectorSetDef;
IMPORT LRVectorMRVTbl;

IMPORT PointEvaluatorLR AS PointEvaluator;
IMPORT PointEvaluatorLRSeq AS PointEvaluatorSeq;

IMPORT PointMetricLRVector AS PointMetric;
IMPORT PointMetricLRVectorArraySort AS PointMetricArraySort;
IMPORT PointMetricLRVectorSeq AS PointMetricSeq;

IMPORT Wx;
IMPORT LRRegression AS Regression;
IMPORT ConjGradient;
IMPORT Thread;
IMPORT PointResult;
IMPORT PointResultSeq;
IMPORT NormalDeviate;
IMPORT Process;
FROM SurfaceRep IMPORT Qdofs, Ldofs, ComputeIndepsL, ComputeIndepsQ,
                       FmtQ, BiggestQuadratic, ComputeG,
                       ComputeQ;
IMPORT StatFits;
IMPORT LongrealPQ;
IMPORT Matrix;
IMPORT SchemeSymbol;
IMPORT ResponseModel;
IMPORT QuadResponse;
IMPORT ModelVar;
IMPORT ModelVarSeq;
IMPORT SchemeObject;
IMPORT Scheme;
IMPORT SchemeUtils;
IMPORT SchemeLongReal;
IMPORT StatObject;

CONST LR  = LongReal;
      T2S = SchemeSymbol.FromText;
      L2S = SchemeLongReal.FromLR;

TYPE TA = ARRAY OF TEXT;

VAR doDebug := Debug.DebugThis("QuadRobust");

VAR doNominal := FALSE;

PROCEDURE SetDoNominal(to : BOOLEAN) =
  BEGIN doNominal := to END SetDoNominal;

PROCEDURE GetDoNominal() : BOOLEAN =
  BEGIN RETURN doNominal END GetDoNominal;

VAR selectByAll := FALSE;
  
PROCEDURE SetSelectByAll(to : BOOLEAN) =
  BEGIN selectByAll := to END SetSelectByAll;

PROCEDURE GetSelectByAll() : BOOLEAN =
  BEGIN RETURN selectByAll END GetSelectByAll;

PROCEDURE GetFHist(seq : PointResultSeq.T) : LRSeq.T =
  (* type converter *)
  VAR
    res := NEW(LRSeq.T).init();
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      res.addhi(seq.get(i).metric)
    END;
    RETURN res
  END GetFHist;

PROCEDURE FindBest(seq         : PointResultSeq.T;
                   VAR bestval : LONGREAL;
                   VAR bestp   : LRVector.T) =
  (* find the best answer of all the evaluations *)
  VAR
    min  := LAST(LONGREAL);
    minQ := FALSE;
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      WITH e = seq.get(i) DO
        (* quadratic beats non-quadratic *)
        IF e.metric < min AND (e.quadratic OR NOT minQ) THEN
          bestp   := LRVector.Copy(e.p);
          bestval := e.metric;
          min     := e.metric;
          minQ    := e.quadratic
        END
      END
    END          
  END FindBest;

(* parallel evaluation code follows *)

TYPE
  MultiEvaluator = LRScalarField.Default OBJECT
    (* using Default instead of T turns on evalHint *)
    base    : MultiEvalLRVector.T;
    samples : CARDINAL;
    values  : LRVectorMRVTbl.T;
    thisCyc : LRVectorSet.T;
    toEval  : SchemeObject.T;
  METHODS
    init(func    : MultiEvalLRVector.T;
         samples : CARDINAL;
         values  : LRVectorMRVTbl.T;
         toEval  : SchemeObject.T;
         thisCyc : LRVectorSet.T       ) : MultiEvaluator := InitME;
  OVERRIDES
    eval     := MEEval;
  END;

VAR valueMu := NEW(MUTEX);

TYPE
  NomClosure = Thread.Closure OBJECT
    me  : MultiEvalLRVector.T;
    at  : LRVector.T;
    res : LRVector.T;
  OVERRIDES
    apply := NCApply;
  END;

PROCEDURE NCApply(nc : NomClosure) : REFANY =
  BEGIN
    nc.res := nc.me.nominalEval(at := nc.at);
    RETURN nc
  END NCApply;
    
PROCEDURE MEEval(me      : MultiEvaluator;
                 p       : LRVector.T) : LONGREAL =
  CONST
    Poison : LRVector.T = NIL;
  VAR
    res, oldres : MultiEvalLRVector.Result;
    isNew, haveOld : BOOLEAN;

    thr : Thread.T := NIL;
  BEGIN

    (* 
       this nominal business is tricky.

       The first eval at a point always includes a nominal eval IF
       we are doing nominal evals at all.
    *)

    LOCK valueMu DO
      isNew   := NOT me.thisCyc.insert(p);
      haveOld := me.values.get(p, oldres);
    END;

    IF doNominal AND NOT haveOld THEN
      Debug.Out("QuadRobust.MEEval : Launching nominal eval at " & FmtP(p));
      thr := Thread.Fork(NEW(NomClosure, me := me.base, at := p))
    END;
    
    res := me.base.multiEval(p, me.samples);

    IF thr # NIL THEN
      VAR
        nc : NomClosure := Thread.Join(thr);
      BEGIN
        res.nominal := nc.res;
      END
    ELSIF doNominal THEN
      res.nominal := Poison (* poison it to look for bugs *)
    END;
    
    IF isNew THEN
      IF haveOld THEN
         <*ASSERT oldres.nominal # Poison*>
        res.nominal := oldres.nominal;
        res := MultiEvalLRVector.Combine(res, oldres);
      END;
      IF doDebug THEN
        Debug.Out("QuadRobust.MEEval : adding new entry to me.values")
      END;
      LOCK valueMu DO
        <*ASSERT res.nominal # Poison*>
        EVAL me.values.put(p, res)
      END
    ELSIF haveOld THEN
      (* use the old data if it's got more samples *)
      <*ASSERT oldres.nominal # Poison*>
      IF oldres.n > res.n THEN
        res := oldres
      ELSE
        res.nominal := oldres.nominal
      END;
      <*ASSERT res.nominal # Poison*>
    END;

    <*ASSERT res.nominal # Poison*>

    (* here we have the best possible estimate in res , and we have updated
       the database *)

    WITH nominal = MultiEvalLRVector.Nominal(res),
         mean    = MultiEvalLRVector.Mean   (res),
         sdev    = MultiEvalLRVector.Sdev   (res) DO 
      <*ASSERT nominal # Poison*>
      IF doDebug THEN
        Debug.Out(F("QuadRobust.MEEval : nominal=%s mean=%s sdev=%s",
                    FmtP(nominal), FmtP(mean), FmtP(sdev)))
      END;

      WITH retval = SchemeFinish(res, me.toEval) DO
        IF doDebug THEN
          Debug.Out(F("QuadRobust.MEEval : reval=%s",
                      LR(retval)))
        END;
        RETURN retval
      END
    END
  END MEEval;

PROCEDURE ResMetric(READONLY r : MultiEvalLRVector.Result) : LONGREAL =
  (* metric as observed from direct observations *)
  BEGIN
    (* FILL THIS IN *)
    RETURN 0.0d0
  END ResMetric;
  
PROCEDURE InitME(self    : MultiEvaluator;
                 func    : MultiEvalLRVector.T;
                 samples : CARDINAL;
                 values  : LRVectorMRVTbl.T;
                 toEval  : SchemeObject.T;
                 thisCyc : LRVectorSet.T) : MultiEvaluator =
  BEGIN
    self.hintsByForking := TRUE;
    self.base           := func;
    self.samples        := samples;
    self.values         := values;
    self.toEval         := toEval;
    self.thisCyc        := thisCyc;
    RETURN self
  END InitME;

(* 
   we maintain a database of evaluations, both of means and 
   variances (so really of counts, sums, and sum-of-squares)

   Since there is memoization within a single iteration (or "pass" as it
   is named below) we only update this database ONCE for each point
   per iteration. 

   If a given point has been updated on an iteration, we in other words,
   do not update that point again on this iteration.

   When we ask for the value at a given point, we return the value
   from the database, not from the most recent evaluation.
*)

PROCEDURE Vdist(a, b : LRVector.T) : LONGREAL =
  BEGIN
    RETURN Math.sqrt(M.SumDiffSqV(a^, b^))
  END Vdist;

PROCEDURE DoLeaderBoard(READONLY pr   : PointResult.T; (* current [old] point *)
                        values        : LRVectorMRVTbl.T;
                        newpts(*OUT*) : LRVectorSet.T) : PointResult.T =
  VAR
    parr := NEW(REF ARRAY OF PointMetric.T, values.size());
    iter := values.iterate();
    q : LRVector.T;
    r : MultiEvalLRVector.Result;
    i := 0;
    n := NUMBER(pr.p^);
  BEGIN
    (* populate the array *)

    Debug.Out("**** values.size() = " & Int(values.size()));

    WHILE iter.next(q, r) DO
      WITH metric = ResMetric(r) DO
        IF i > LAST(parr^) THEN
          Debug.Warning(F("i [%s] > LAST(parr^) [%s]",
                          Int(i), Int(LAST(parr^))))
        END;
        parr[i] := PointMetric.T { metric, q, r };
        INC(i)
      END
    END;

    PointMetricArraySort.Sort(parr^);
    
    WITH wx = Wx.New() DO
      Wx.PutText(wx, "==== QuadRobust leaderboard ====\n");
      Wx.PutText(wx, F("rho = %s ; p = %s\n", LR(rho), FmtP(pr.p)));
      FOR i := FIRST(parr^) TO LAST(parr^) DO
        WITH rec  = parr[i],
             dist = Vdist(rec.p, pr.p) DO

          IF newpts.size() < 4 * n * n THEN
            (* insert first n^2 points in interesting set *)
            Debug.Out("Adding to newpts : rec.p = " & FmtP(rec.p));
            EVAL newpts.insert(rec.p)
          END;
          (*  *)
          Wx.PutText(wx, F("metric %s ; dist %s; result %s ; rec.p %s\n",
                           LR(rec.metric),
                           LR(dist),
                           MultiEvalLRVector.Format(rec.result),
                           FmtP(rec.p)))
        END
      END;
      Debug.Out(Wx.ToText(wx))
    END;

    WITH dofs = Qdofs(n) DO
      IF NUMBER(parr^) >= dofs THEN
        Debug.Out(F("DoLeaderBoard: enough points (%s >= %s) to attempt a surface fit.", Int(NUMBER(parr^)), Int(dofs)));
        TRY
          RETURN pr (* junk *)
        EXCEPT
          Matrix.Singular =>
          Debug.Warning("AttemptSurfaceFit raised Matrix.Singular!")
        END
      ELSE
        Debug.Out(F("DoLeaderBoard: NOT enough points (%s < %s) to attempt a surface fit.", Int(NUMBER(parr^)), Int(dofs)));
      END;
      RETURN pr (* on failure, just return the old point *)
    END
  END DoLeaderBoard;

PROCEDURE InsertClosestPoints(n             : CARDINAL;
                              bestQ         : LRVector.T;
                              READONLY parr : ARRAY OF PointMetric.T;
                              newpts        : LRVectorSet.T) =
  VAR
    darr := NEW(REF ARRAY OF PointMetric.T, NUMBER(parr));
  BEGIN
    FOR i := FIRST(parr) TO LAST(parr) DO
      WITH rec  = parr[i],
           dist = Vdist(rec.p, bestQ) DO
        darr[i] := PointMetric.T { dist, rec.p, rec.result }
      END
    END;
    
    PointMetricArraySort.Sort(darr^);
    
    WITH wx = Wx.New() DO
      Wx.PutText(wx, "==== Closest neighbors to quad. min. ====\n");
      FOR i := FIRST(darr^) TO LAST(darr^) DO
        WITH rec = darr[i] DO
          Wx.PutText(wx, F("dist %s ; result %s ; rec.p %s\n",
                           LR(rec.metric),
                           MultiEvalLRVector.Format(rec.result),
                           FmtP(rec.p)));
        END
      END;
      
      Debug.Out(Wx.ToText(wx));
    END;
    FOR i := 0 TO MIN(NUMBER(darr^) - 1, 4 * n * n) DO
      Debug.Out("Adding to newpts : rec.p = " & FmtP(darr[i].p));
      EVAL newpts.insert(darr[i].p)
    END
  END InsertClosestPoints;

PROCEDURE GetConstantTerm(b : REF M.M) : LONGREAL =
  BEGIN
    WITH idx = LAST(b^) DO
      RETURN b[idx, 0]
    END
  END GetConstantTerm;

PROCEDURE DoSimpleFit(p                          : LRVector.T;
                      bnom, bmu, bsigma          : REF M.M;
                      tol                        : LONGREAL;
                      newpts                     : LRVectorSet.T
                      ) =
  VAR
    b := NEW(B,
             bnom   := bnom,
             bmu    := bmu,
             bsigma := bsigma,
             alpha  := 0.0d0,
             p      := p);
    qf      : QuadraticF;
    qg      : QuadraticG;
    mp      := LRVector.Copy(p);
    
    bdims   := M.GetDim(bmu^);
    bb      := M.NewM(bdims);
    beta    := 0.0d0;
    success := FALSE;
    
    best    : LONGREAL;
    biggest : LONGREAL;
    n := NUMBER(p^);
  BEGIN
    IF doNominal THEN
      qf := NEW(QuadraticF, b := b, eval := EvalQFN);
      qg := NEW(QuadraticG, b := b, eval := EvalQGN);
      M.LinearCombination3(1.0d0 , bnom^,
                           1.0d0 , bmu^,
                           0.0d0, bsigma^,
                           bb^)
    ELSE
      qf := NEW(QuadraticF, b := b, eval := EvalQF);
      qg := NEW(QuadraticG, b := b, eval := EvalQG);
      M.LinearCombination(1.0d0 , bmu^,
                          0.0d0, bsigma^,
                          bb^)
    END;
    
    Debug.Out("bb    = " & FmtQ(n, bb));
    biggest := BiggestQuadratic(p, bb);
    
    (* here biggest is the most negative quadratic coefficient *)
    Debug.Out("quadratic biggest = " & LR(biggest));
    
    LOOP
      mp^ := p^;

      TRY
        best := ConjGradient.Minimize(mp, tol, qf, qg);
      EXCEPT
        ConjGradient.TooManyIterations =>
        Debug.Warning("QuadRobust.DoSimpleFit : caught ConjGradient.TooManyIterations");
        RETURN
      END;
      
      Debug.Out(F("Minimized quadratic %s @ %s [tol=%s]",
                  LR(best), FmtP(mp), LR(tol)));
      
      (* insert this point too *)
      
      IF best = best THEN
        Debug.Out(F("minimization succeeded beta=%s, inserting : %s ",
                    LR(beta) , FmtP(mp)));
        EVAL newpts.insert(mp);
        success := TRUE;
        EXIT
      ELSIF beta > 4.0d0 THEN
        (* give up *)
        Debug.Out("minimization failed with beta exceeding 4, giving up!");
        
        EXIT
      ELSE
        (* failed.. must be a saddle point *)
        
        beta    := beta * 2.0d0 + 0.1d0;
        b.alpha := beta * ABS(biggest);
        
        Debug.Out(F("Minimization failed, adjusting : beta=%s alpha=%s", LR(beta), LR(b.alpha)))
      END
    END(* POOL *);
  END DoSimpleFit;
  
  (* 
     the problem here is that because of noise, the matrix we 
     investigate may not be positive definite (we may generate a saddle
     point).
     
     So we add a quadratic 
     
     alpha * (x - p) ^2 
     
     as "spice."
  *)
  
TYPE
  B = OBJECT
    bnom, bmu, bsigma : REF M.M;
    p                 : LRVector.T;
    alpha             : LONGREAL;
  END;
  
  QuadraticF = LRScalarField.Default OBJECT (* quad function wrapper *)
    b : B;
  OVERRIDES
    eval := EvalQF;
  END;

  QuadraticG = LRVectorField.Default OBJECT (* quad gradient wrapper *)
    b : B;
  OVERRIDES
    eval := EvalQG;
  END;

PROCEDURE SchemeFinish(resS    : MultiEvalLRVector.Result;
                       toEval  : SchemeObject.T) : LONGREAL =
  BEGIN    
    <*ASSERT resS.sum # NIL*>
    <*ASSERT resS.sumsq # NIL*>
    Debug.Out("SchemeFinish : resS = " & MultiEvalLRVector.Format(resS));

    (* resS contains all the data *)
    WITH pso     = NEW(StatObject.DefaultPoint).init(),
         optvars = GetOptVars(),
         nom     = MultiEvalLRVector.Nominal(resS),
         mu      = MultiEvalLRVector.Mean   (resS),
         sigma   = MultiEvalLRVector.Sdev   (resS)
     DO
      FOR i := 0 TO optvars.size() - 1  DO
        WITH v = optvars.get(i) DO
          pso.define(v.nm, nom[i], mu[i], sigma[i])
        END
      END;

      WITH scm = NARROW(resS.extra,Scheme.T),
           e = SchemeUtils.List3(T2S("eval-in-env"),
                                 L2S(0.0d0),
                                 SchemeUtils.List2(T2S("quote"), toEval)) DO
        scm.bind(T2S("*the-stat-object*"), pso);
        Debug.Out("SchemeFinish : toEval = " & SchemeUtils.Stringify(e));

        TRY
          WITH res = scm.evalInGlobalEnv(e) DO
            Debug.Out("SchemeFinish : final res = " & SchemeUtils.Stringify(res));
            RETURN SchemeLongReal.FromO(res)
          END
        EXCEPT
          Scheme.E(x) =>
          Debug.Error(F("EXCEPTION! SchemeFinish : Scheme.E \"%s\" when evaluating toEval", x));
          <*ASSERT FALSE*>
        END
      END
    END
  END SchemeFinish;

PROCEDURE EvalQF(qf : QuadraticF; p : LRVector.T) : LONGREAL =
  BEGIN
    WITH mu    = ComputeQ(p, qf.b.bmu),
         sigma = ComputeQ(p, qf.b.bsigma),
         spice = qf.b.alpha * M.SumDiffSqV(p^, qf.b.p^) DO
      <*ASSERT NOT doNominal*>
      RETURN mu + 666.666d0 * sigma + spice
    END
  END EvalQF;

PROCEDURE EvalQG(qg : QuadraticG; p : LRVector.T) : LRVector.T =
  BEGIN
    WITH muG    = ComputeG(p, qg.b.bmu),
         sigmaG = ComputeG(p, qg.b.bsigma),
         res    = NEW(LRVector.T, NUMBER(p^)) DO
      <*ASSERT NOT doNominal*>
      M.LinearCombinationV(1.0d0, muG^, 666.666d0, sigmaG^, res^);

      FOR i := FIRST(res^) TO LAST(res^) DO
        res[i] := res[i] + 2.0d0 * qg.b.alpha * (p[i] - qg.b.p[i])
      END;
      
      RETURN res
    END
  END EvalQG;
  
PROCEDURE EvalQFN(qf : QuadraticF; p : LRVector.T) : LONGREAL =
  BEGIN
    WITH nom   = ComputeQ(p, qf.b.bnom),
         mu    = ComputeQ(p, qf.b.bmu),
         sigma = ComputeQ(p, qf.b.bsigma),
         spice = qf.b.alpha * M.SumDiffSqV(p^, qf.b.p^) DO
      <*ASSERT doNominal*>
      RETURN nom + mu + 666.666d0 * sigma + spice
    END
  END EvalQFN;

PROCEDURE EvalQGN(qg : QuadraticG; p : LRVector.T) : LRVector.T =
  BEGIN
    WITH muG    = ComputeG(p, qg.b.bmu),
         nomG   = ComputeG(p, qg.b.bnom),
         sigmaG = ComputeG(p, qg.b.bsigma),
         res    = NEW(LRVector.T, NUMBER(p^)) DO
      <*ASSERT doNominal*>
      M.LinearCombinationV3(1.0d0, nomG^,
                            1.0d0, muG^,
                            666.666d0, sigmaG^,
                            res^);

      FOR i := FIRST(res^) TO LAST(res^) DO
        res[i] := res[i] + 2.0d0 * qg.b.alpha * (p[i] - qg.b.p[i])
      END;
      
      RETURN res
    END
  END EvalQGN;
  
VAR ridgeCoeff := 0.0d0;
    
PROCEDURE Minimize(pa             : LRVector.T;
                   func           : MultiEvalLRVector.T;
                   toEval         : SchemeObject.T;
                   rhobeg, rhoend : LONGREAL;
                   progressWriter : ResultWriter) : Output =
  VAR
    n        := NUMBER(pa^);
    nv       := 2 * n;
    da       := NEW(REF ARRAY OF LRVector.T, nv);
    pp       := NEW(REF ARRAY OF LRVector.T, nv);
    lps      := NEW(REF ARRAY OF LineProblem.T, nv);
    rand     := NEW(Random.Default).init();
    mins     := NEW(PointResultSeq.T).init();
    allMins  := NEW(PointResultSeq.T).init();
    lm       := NEW(REF ARRAY OF LineMinimizer.T, nv);

    message  : TEXT;
    thisCyc  : LRVectorSet.T;
    values   := NEW(LRVectorMRVTbl.Default).init();
    peSeq    := NEW(PointEvaluatorSeq.T).init();

    newPts   := NEW(LRVectorSetDef.T).init(); (* interesting points to eval *)

    samples : CARDINAL;

    pr       := PointResult.T { pa, LAST(LONGREAL), FALSE, LAST(LONGREAL) };

  BEGIN
    rho   := rhobeg;
    iter  := 0;
    
    FOR i := 0 TO 2 * n - 1 DO
      lm[i] := NEW(LineMinimizer.T).init()
    END;
    
    (* allocate some random vectors *)
    FOR i := FIRST(da^) TO LAST(da^) DO
      da[i] := RandomVector.GetDirV(rand, n, 1.0d0)
    END;

    (* orthogonalize the first n vectors with Gram-Schmidt *)

    Orthogonalize(SUBARRAY(da^, 0, n));  (* first ortho. block *)
    Orthogonalize(SUBARRAY(da^, n, n));  (* second ortho. block *)

    (* setup complete *)
    
    FOR pass := 0 TO 100 * n - 1 DO

      (*******************  MAIN MINIMIZATION ITERATION  *******************)

      IF pass = 0 THEN
        samples := 5
      ELSIF pass < 4 THEN
        samples := 10
      ELSIF pass < 8 THEN
        samples := 20
      ELSE
        samples := 40
      END;

      Debug.Out(F("QuadRobust.Minimize : pass %s : newPts.size()=%s NUMBER(da^)=%s",
                  Int(pass), Int(newPts.size()), Int(NUMBER(da^))));
      
      (* spin up enough threads to handle newPts *)
      WHILE peSeq.size() < newPts.size() DO
        WITH newPe = NEW(PointEvaluator.T).init() DO
          peSeq.addhi(newPe)
        END
      END;

      (* launch evaluations *)
      VAR
        iter := newPts.iterate();
        p : LRVector.T;
        i := 0;
      BEGIN
        WHILE iter.next(p) DO
          WITH q  = LRVector.Copy(p),
               pe = peSeq.get(i),
               ff = NEW(MultiEvaluator).init(func,
                                             2 * samples,
                                             values,
                                             toEval,
                                             thisCyc) DO
            pe.start(q, ff);
            INC(i)
          END
        END
      END(*RAV*);

      LOCK valueMu DO
        thisCyc := NEW(LRVectorSetDef.T).init()
      END;
      
      Debug.Out(F("QuadRobust.Minimize : pass %s; database has %s points",
                  Int(pass), Int(values.size())));
      Debug.Out(F("QuadRobust.Minimize : rho=%s [rhoend=%s]", LR(rho), LR(rhoend)));

      <*ASSERT LineMinimizer.Running() = 0*>
        
      FOR i := FIRST(da^) TO LAST(da^) DO
        
        pp[i] := LRVector.Copy(pr.p);
        WITH ff = NEW(MultiEvaluator).init(func, samples, values, toEval, thisCyc) DO
          lm[i].start(pp[i],
                      LRVector.Copy(da[i]),
                      ff,
                      rho)
        END
      END;
      
      IF FALSE THEN
        Debug.Out("Robust.m3 : running = " & Int(LineMinimizer.Running())) 
      END;

      Debug.Out("QuadRobust : Waiting for points to evaluate ... ");
      FOR i := 0 TO newPts.size() - 1 DO
        EVAL peSeq.get(i).wait()
      END;

      newPts := NEW(LRVectorSetDef.T).init(); (* empty the set *)

      Debug.Out("QuadRobust : Waiting for line searches to evaluate ...");
      FOR i := FIRST(da^) TO LAST(da^) DO
        lps[i] := lm[i].wait()
      END;
      Debug.Out("All tasks are done");
      (* all tasks are done *)
      <*ASSERT LineMinimizer.Running() = 0*>
      
      (* at this point we have the minima in all directions 
         in two orthonormal bases 0..n-1, and n..2*n-1 *)

      LineProblemArraySort.Sort(lps^);

      (* next point should be the best of the line minimizations
         
         Because we are using two orthonormal bases, we get two
         opt points.
      *)

      VAR
        newp := PointResult.T { lps[0].minp, lps[0].minval, FALSE, rho };
        (* tentatively set to best line-search point *)
        
        opt0 := Predict(pr.p, SUBARRAY(pp^, 0, n));
        opt1 := Predict(pr.p, SUBARRAY(pp^, n, n));
      BEGIN
        Debug.Out("About to call DoLeaderBoard.  values.size() = " & Int(values.size()));
        LOCK valueMu DO
          WITH bestq = DoLeaderBoard(pr, values, newPts) DO
            newp := bestq
          END
        END;
        Debug.Out(F("Robust.m3 : opt0 (%s) ; opt1 (%s)",
                    M.FormatV(opt0^),
                    M.FormatV(opt1^)));
        
        Debug.Out(F("Robust.m3 : updating p (%s) -> (%s)",
                    PointResult.Format(pr),
                    PointResult.Format(newp)));


        IF newp.p^ = pr.p^ THEN
          (* if we're not going anywhere, just tighten up a little bit *)
          rho := 0.9d0 * rho
        ELSE
          WITH dp = LRVector.Copy(pr.p) DO
            M.SubV(newp.p^, pr.p^, dp^);
            (* we blend in new rho estimator *)
            
            rho := 0.50d0 * rho + 0.50d0 * M.Norm(dp^);
            
            Debug.Out(F("Robust.m3 : new rho = %s", LR(rho)));
            IF rho < rhoend THEN
              message := "stopping because rho < rhoend";
              EXIT
            END
          END
        END;


        FOR i := FIRST(da^) TO LAST(da^) DO
          da[i] := RandomVector.GetDirV(rand, n, 1.0d0)
        END;

        (* 
           set the two anchor vectors to point to the "opt point" 
           Maybe we should try swapping the opt points between the
           basis blocks?  Or average them?
        *)
        
        M.SubV(opt0^, newp.p^, da[0]^);
        M.SubV(opt1^, newp.p^, da[n]^);

        pr := newp;
        mins.addhi   (newp);
        allMins.addhi(newp);

        IF progressWriter # NIL THEN
          WITH output = Output { iterations := iter,
                                 funcCount  := 0,
                                 fhist      := GetFHist(allMins),
                                 message    := message,
                                 f          := pr.metric,
                                 x          := LRVector.Copy(pr.p),
                                 stoprho    := rho } DO
            progressWriter.write(output)
          END
        END;
            
        WITH Lookback = 5 DO

          (* 
             if we haven't improved in five straight iterations,
             call it a day 
          *)
          
          IF mins.size() > Lookback THEN
            WITH old = mins.get(mins.size() - Lookback) DO
              IF old.metric <= newp.metric AND (NOT newp.quadratic OR old.quadratic) AND old.rho <= newp.rho THEN
                message := "stopping because no more improvement";
                EXIT
              END
            END
          END
        END
      END;

      (* forget really old (unreliable) values *)
      WITH Lookback = 7 DO
        IF mins.size() > Lookback THEN
          EVAL mins.remlo()
        END
      END;
      
      (* 
         Finally, maintain the loop invariant that the search vectors are
         ready on loop entry.

         We do this here ... because... ?  Not sure, we could probably
         do it at the top of the loop, but on the first iteration,
         the primary directions are random, whereas here, they are pointing
         to opt.
      *)
      Orthogonalize(SUBARRAY(da^, 0, n));  (* first ortho. block *)
      Orthogonalize(SUBARRAY(da^, n, n));  (* second ortho. block *)

      (* 
         clear cache so we don't get fooled by noise 
         Note that we expect the function evaluation to use memoization,
         so this defeats memoization (that's the point!)
      *)
      TYPECASE func.base OF
        LRVectorFieldPll.T(pll) => pll.clearTbls()
      ELSE
        (* we don't know how it works so we don't know how to clear it *)
      END;

      INC(iter);

      message := "stopping because of out of iterations"
      (* if we fall through, that's the last message we set! *)
      
    END;

    (******************  SEARCH IS COMPLETE  ******************)
    
    VAR
      bestval : LONGREAL;
      bestv   : LRVector.T;
    BEGIN
      (* 
         Report the best point we saw.  

         Warning: this best point might not be entirely real! 
         It could be noisy!  How do we fix this?
      *)

      IF FALSE THEN
        FindBest(mins, bestval, bestv)
      ELSE
        WITH e = mins.get(mins.size() - 1) DO
          bestval := e.metric;
          bestv   := LRVector.Copy(e.p)
        END
      END;

      
      Debug.Out("Robust.m3 : " & message);
      
      RETURN Output { iterations := iter,
                      funcCount  := 0,
                      fhist      := GetFHist(allMins),
                      message    := message,
                      f          := bestval,
                      x          := bestv,
                      stoprho    := rho }
    END
  END Minimize;

PROCEDURE Predict(s          : LRVector.T;
                  READONLY d : ARRAY OF LRVector.T) : LRVector.T =
  VAR
    n := NUMBER(s^);
    diff, sum := NEW(LRVector.T, n);

  BEGIN
    M.ZeroV(sum^);
    FOR i := FIRST(d) TO LAST(d) DO
      <*ASSERT d[i] # NIL*>
      <*ASSERT s    # NIL*>
      <*ASSERT diff # NIL*>
      M.SubV(d[i]^, s^, diff^);
      M.AddV(diff^, sum^, sum^)
    END;
    M.AddV(s^, sum^, sum^);
    RETURN sum
  END Predict;

PROCEDURE RemoveComponent(READONLY ik : LRVector.S; VAR v : LRVector.S) =
  BEGIN
    WITH dot = M.Dot(ik, v) DO
      M.LinearCombinationV(-dot, ik, 1.0d0, v, v)
    END
  END RemoveComponent;

PROCEDURE Orthogonalize(READONLY da : ARRAY OF LRVector.T) =
  (* orthogonalizes (orthonormalizes) the first N elements of da;
     doesnt touch da[0] *)
  VAR
    n := NUMBER(da[0]^);
  BEGIN
    FOR i := 1 TO n - 1 DO
      FOR j := 0 TO i - 1 DO
        RemoveComponent(da[j]^, da[i]^) (* remove da[j] from da[i] *)
      END;
      WITH inorm = Math.sqrt(M.Dot(da[i]^, da[i]^)),
           mult  = 1.0d0 / inorm DO
        M.MulSV(mult, da[i]^, da[i]^)
      END
    END;
  END Orthogonalize;

PROCEDURE DoModel(varname : SchemeSymbol.T;
                  models  : ARRAY QuadResponse.T OF ResponseModel.Type) =
  BEGIN
    optvars.addhi(ModelVar.T { varname, models })
  END DoModel;

VAR optvars : ModelVarSeq.T;
  
PROCEDURE OptInit() =
  BEGIN
    optvars := NEW(ModelVarSeq.T).init();
  END OptInit;

PROCEDURE GetOptVars() : ModelVarSeq.T = BEGIN RETURN optvars END GetOptVars;
  
BEGIN
  OptInit()
END QuadRobust.
