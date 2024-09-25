MODULE StocRobust;

(* Robust parallel minimization.

   We maintain a point p where we search.

   On each iteration, we perform line searches in various directions.

   The best result so far is accepted at the next point.

   Author : mika.nystroem@intel.com
   September, 2024
*)

FROM NewUOAs IMPORT Output;
IMPORT LRVector, LRScalarField;
IMPORT LRVectorField;
IMPORT RandomVector;
IMPORT Random;
IMPORT LRMatrix2 AS M;
IMPORT Math;
IMPORT Debug;
FROM Fmt IMPORT LongReal, F, Int, Bool, FN;
IMPORT LineProblem;
IMPORT LineProblemSeq;
IMPORT LineProblemArraySort;
IMPORT LRScalarFieldPll;
IMPORT LongRealSeq AS LRSeq;
FROM GenOpt IMPORT rho, iter, FmtP;
IMPORT LineMinimizer;
IMPORT MultiEval, MultiEvalClass;
IMPORT LRVectorSet, LRVectorSetDef;
IMPORT LRVectorMRTbl;
IMPORT PointEvaluatorSeq;
IMPORT PointMetric;
IMPORT PointMetricArraySort;
IMPORT Wx;
IMPORT PointMetricSeq;
IMPORT LRRegression AS Regression;
IMPORT ConjGradient;
IMPORT PointEvaluator;

CONST LR = LongReal;

TYPE TA = ARRAY OF TEXT;

VAR doDebug := Debug.DebugThis("StocRobust");

VAR sigmaK := 0.0d0;
    
PROCEDURE SetSigmaK(k : LONGREAL) =
  BEGIN sigmaK := k END SetSigmaK;

PROCEDURE GetSigmaK() : LONGREAL =
  BEGIN RETURN sigmaK END GetSigmaK;

PROCEDURE GetFHist(seq : LineProblemSeq.T) : LRSeq.T =
  (* type converter *)
  VAR
    res := NEW(LRSeq.T).init();
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      res.addhi(seq.get(i).minval)
    END;
    RETURN res
  END GetFHist;

PROCEDURE FindBest(seq         : LineProblemSeq.T;
                   VAR bestval : LONGREAL;
                   VAR bestp   : LRVector.T) =
  (* find the best answer of all the evaluations *)
  VAR
    min := LAST(LONGREAL);
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      WITH e = seq.get(i) DO
        IF e.minval < min THEN
          bestp := LRVector.Copy(e.minp);
          bestval := e.minval;
          min := e.minval
        END
      END
    END          
  END FindBest;

(* parallel evaluation code follows *)

TYPE
  MultiEvaluator = LRScalarField.Default OBJECT
    (* using Default instead of T turns on evalHint *)
    base    : MultiEval.T;
    samples : CARDINAL;
    sigmaK  : LONGREAL;
    values  : LRVectorMRTbl.T;
    thisCyc : LRVectorSet.T;
  OVERRIDES
    eval     := MEEval;
  END;

VAR valueMu := NEW(MUTEX);
    
PROCEDURE MEEval(me      : MultiEvaluator;
                 p       : LRVector.T) : LONGREAL =
  VAR
    res := me.base.multiEval(p, me.samples);
    oldres : MultiEval.Result;
  BEGIN

    WITH new     = NOT me.thisCyc.member(p),
         haveOld = me.values.get(p, oldres) DO

      IF new THEN
        IF haveOld THEN
          res := MultiEval.Combine(res, oldres)
        END;
        IF doDebug THEN
          Debug.Out("adding new entry to me.values")
        END;
        LOCK valueMu DO
          EVAL me.values.put(p, res)
        END
      ELSIF haveOld THEN
        (* use the old data if it's got more samples *)
        IF oldres.n > res.n THEN res := oldres END
      END;
    END;

    (* here we have the best possible estimate in res , and we have updated
       the database *)

    WITH mean   = MultiEval.Mean(res),
         sdev   = MultiEval.Sdev(res),
         retval = mean + me.sigmaK * sdev DO
      IF doDebug THEN
        Debug.Out(F("StocRobust.MEEval : mean=%s sigmaK=%s sdev=%s -> retval=%s",
                    LR(mean), LR(me.sigmaK), LR(sdev), LR(retval)))
      END;
      RETURN retval
    END
  END MEEval;

PROCEDURE ResMetric(READONLY r : MultiEval.Result; sigmaK : LONGREAL) : LONGREAL =
  BEGIN
    WITH mean   = MultiEval.Mean(r),
         sdev   = MultiEval.Sdev(r),
         retval = mean + sigmaK * sdev DO
      RETURN retval
    END
  END ResMetric;
  
PROCEDURE MultiEvaluate(func    : MultiEval.T;
                        samples : CARDINAL;
                        sigmaK  : LONGREAL;
                        values  : LRVectorMRTbl.T;
                        thisCyc : LRVectorSet.T) : LRScalarField.T =
  BEGIN
    WITH me = NEW(MultiEvaluator,
                  hintsByForking := TRUE,
                  base           := func,
                  samples        := samples,
                  sigmaK         := sigmaK,
                  values         := values,
                  thisCyc        := thisCyc) DO
      RETURN me
    END
  END MultiEvaluate;

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

PROCEDURE DoLeaderBoard(p             : LRVector.T; (* current [old] point *)
                        newp          : LRVector.T; (* next [new] point *)
                        values        : LRVectorMRTbl.T;
                        newpts(*OUT*) : LRVectorSet.T) : LRVector.T =
  VAR
    parr := NEW(REF ARRAY OF PointMetric.T, values.size());
    iter := values.iterate();
    q : LRVector.T;
    r : MultiEval.Result;
    i := 0;
    n := NUMBER(p^);
    dofs := (n * n + 3 * n + 2) DIV 2;
    bestQ : LRVector.T;
  BEGIN
    (* populate the array *)

    Debug.Out("**** values.size() = " & Int(values.size()));

    WHILE iter.next(q, r) DO
      WITH metric = ResMetric(r, sigmaK) DO
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
      Wx.PutText(wx, "==== StocRobust leaderboard ====\n");
      Wx.PutText(wx, F("rho = %s ; p = %s\n", LR(rho), FmtP(p)));
      FOR i := FIRST(parr^) TO LAST(parr^) DO
        WITH rec  = parr[i],
             dist = Vdist(rec.p, p) DO

          IF newpts.size() < 4 * n * n THEN
            (* insert first n^2 points in interesting set *)
            Debug.Out("Adding to newpts : rec.p = " & FmtP(rec.p));
            EVAL newpts.insert(rec.p)
          END;
          
          Wx.PutText(wx, F("metric %s ; dist %s; result %s ; rec.p %s\n",
                           LR(rec.metric),
                           LR(dist),
                           MultiEval.Format(rec.result),
                           FmtP(rec.p)))
        END
      END;
      Debug.Out(Wx.ToText(wx))
    END;

    IF NUMBER(parr^) >= dofs THEN
      (* attempt a surface fit *)
      Debug.Out(F("DoLeaderBoard: enough points (%s >= %s) to attempt a surface fit.", Int(NUMBER(parr^)), Int(dofs)));
      
      VAR
        tryrho := rho;
        seq : PointMetricSeq.T;
        success := FALSE;
      BEGIN
        LOOP
          seq := NEW(PointMetricSeq.T).init();
          FOR i := FIRST(parr^) TO LAST(parr^) DO
            WITH rec  = parr[i],
                 dist = Vdist(rec.p, p) DO
              IF dist < tryrho THEN
                seq.addhi(rec)
              END
            END
          END;
          IF seq.size() >= dofs THEN
            success := TRUE;
            EXIT
          ELSE
            tryrho := tryrho * 2.0d0
          END
        END;

        Debug.Out(F("DoLeaderBoard: seq.size()=%s rho=%s tryrho=%s success=%s",
                    Int(seq.size()), LR(rho), LR(tryrho), Bool(success)));

        IF success THEN
          VAR
            x := NEW(REF M.M, seq.size(), dofs);
            ysigma := NEW(REF M.M, seq.size(), 1);
            ymu    := NEW(REF M.M, seq.size(), 1);
            w := NEW(REF M.M, seq.size(), seq.size());
            ymuHat, ysigmaHat : REF M.M;
            rmu := NEW(Regression.T);
            rsigma := NEW(Regression.T);
          BEGIN
            M.Zero(w^);
            FOR i := 0 TO seq.size() - 1 DO
              WITH rec = seq.get(i) DO
                ComputeIndeps(rec.p, i, x^);
                ymu[i, 0]    := MultiEval.Mean(rec.result);
                ysigma[i, 0] := MultiEval.Sdev(rec.result);
                w[i, i] := FLOAT(rec.result.n, LONGREAL)
              END
            END(*ROF*);

            Regression.Run(x, ymu   , ymuHat   , FALSE, rmu, h := ridgeCoeff, W := w);
            Regression.Run(x, ysigma, ysigmaHat, FALSE, rsigma, h := ridgeCoeff, W := w);

            WITH wx    = Wx.New(),
                 pparr = NEW(REF ARRAY OF PointMetric.T, seq.size()) DO
              Wx.PutText(wx, "=====  QUADRATIC PREDICTION  =====\n");
              FOR i := 0 TO seq.size() - 1 DO
                WITH rec = seq.get(i),
                     pred = ymuHat[i,0] + sigmaK * ysigmaHat[i,0],
                     p2mu = ComputeQ(rec.p, rmu.b),
                     p2sigma = ComputeQ(rec.p, rsigma.b),
                     pred2 = p2mu + sigmaK * p2sigma DO
                  
                  Wx.PutText(wx, FN("metric %s : res %s, pred { mu %s ; sig %s } : predmetric %s =? %s; rec.p %s\n", TA{
                                   LR(rec.metric),
                                   MultiEval.Format(rec.result),
                                   LR(ymuHat[i,0]),
                                   LR(ysigmaHat[i,0]),
                                   LR(pred),
                                   LR(pred2),
                                   FmtP(rec.p)}));
                  pparr[i] := PointMetric.T { pred, rec.p, rec.result }
                END
              END(* ROF *);

              (* pparr contains these points tagged by their predicted
                 metric *)
              PointMetricArraySort.Sort(pparr^);
              Wx.PutText(wx, "=====  SORTED QUADRATIC PREDICTION  =====\n");

              bestQ := LRVector.Copy(pparr[0].p);
              
              FOR i := FIRST(pparr^) TO LAST(pparr^) DO
                WITH pm = pparr[i] DO
                  Wx.PutText(wx, F("pred metric %s : %s : %s\n",
                                   LR(pm.metric),
                                   FmtP(pm.p),
                                   MultiEval.Format(pm.result)))

                END
              END;

              Debug.Out( Wx.ToText(wx) );

              FOR i := 0 TO MIN(NUMBER(pparr^) - 1, 4 * n * n) DO
                Debug.Out("Adding to newpts : rec.p = " & FmtP(pparr[i].p));
                EVAL newpts.insert(pparr[i].p)
              END
              
            END(*WITH*);

            Debug.Out("sigma = " & FmtQ(p, rsigma.b));
            Debug.Out("mu    = " & FmtQ(p, rmu.b));


            
            (* let's have some fun *)

            VAR
              tol := ysigmaHat[0, 0] / 10.0d0;
              best : LONGREAL;
              b := NEW(B,
                       bmu := rmu.b,
                       bsigma := rsigma.b,
                       sigmaK := sigmaK,
                       alpha := 0.0d0,
                       p := p);
              qf := NEW(QuadraticF, b := b);
              qg := NEW(QuadraticG, b := b);
              mp := LRVector.Copy(p);

              bdims := M.GetDim(rmu.b^);
              bb := M.NewM(bdims);
              mostNeg : LONGREAL;
              beta := 0.0d0;
              success := FALSE;
            BEGIN
              M.LinearCombination(1.0d0, rmu.b^, sigmaK, rsigma.b^, bb^);
              Debug.Out("bb    = " & FmtQ(p, bb));
              mostNeg := MostNegQuadratic(p, bb);

              (* here mostNeg is the most negative quadratic coefficient *)
              Debug.Out("quadratic mostNeg = " & LR(mostNeg));
              
              LOOP
                mp^ := p^;
                
                best := ConjGradient.Minimize(mp, tol, qf, qg);
                
                Debug.Out(F("Minimized quadratic %s @ %s",
                            LR(best), FmtP(mp)));
                
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

                  beta := beta * 2.0d0 + 0.1d0;
                  b.alpha := beta * ABS(mostNeg);

                  Debug.Out(F("Minimization failed, adjusting : beta=%s alpha=%s", LR(beta), LR(b.alpha)))
                END
              END(* POOL *);

              IF TRUE THEN
                (* bestQ is the best point we know on the fitted curve *)
                (* grab the nearest points from the seq and re-sample them *)

                VAR
                  darr := NEW(REF ARRAY OF PointMetric.T, NUMBER(parr^));
                BEGIN
                  FOR i := FIRST(parr^) TO LAST(parr^) DO
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
                                         MultiEval.Format(rec.result),
                                         FmtP(rec.p)));
                      END
                    END;

                    Debug.Out(Wx.ToText(wx));
                  END;
                  FOR i := 0 TO MIN(NUMBER(darr^) - 1, 4 * n * n) DO
                    Debug.Out("Adding to newpts : rec.p = " & FmtP(darr[i].p));
                    EVAL newpts.insert(darr[i].p)
                  END
                END
              END;
              RETURN bestQ
            END
          END
        END
      END
      
    END
  END DoLeaderBoard;

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
    bmu, bsigma : REF M.M;
    sigmaK      : LONGREAL;
    p           : LRVector.T;
    alpha       : LONGREAL;
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

PROCEDURE EvalQF(qf : QuadraticF; p : LRVector.T) : LONGREAL =
  BEGIN
    WITH mu    = ComputeQ(p, qf.b.bmu),
         sigma = ComputeQ(p, qf.b.bsigma),
         spice = qf.b.alpha * M.SumDiffSqV(p^, qf.b.p^) DO
      RETURN mu + qf.b.sigmaK * sigma + spice
    END
  END EvalQF;

PROCEDURE EvalQG(qg : QuadraticG; p : LRVector.T) : LRVector.T =
  BEGIN
    WITH muG    = ComputeG(p, qg.b.bmu),
         sigmaG = ComputeG(p, qg.b.bsigma),
         res    = NEW(LRVector.T, NUMBER(p^)) DO
      M.LinearCombinationV(1.0d0, muG^, qg.b.sigmaK, sigmaG^, res^);

      FOR i := FIRST(res^) TO LAST(res^) DO
        res[i] := res[i] + 2.0d0 * qg.b.alpha * (p[i] - qg.b.p[i])
      END;
      
      RETURN res
    END
  END EvalQG;
  
PROCEDURE ComputeIndeps(p : LRVector.T; row : CARDINAL; VAR x : M.M) =
  VAR
    k := 0;
    q : LONGREAL;
    f0, f1 : LONGREAL;
  BEGIN
    FOR i := 0 TO NUMBER(p^) DO
      IF i = NUMBER(p^) THEN
        f0 := 1.0d0
      ELSE
        f0 := p[i]
      END;
      FOR j := i TO NUMBER(p^) DO
        IF j = NUMBER(p^) THEN
          f1 := 1.0d0
        ELSE
          f1 := p[j]
        END;
        q := f0 * f1;
        x[row, k] := q;
        INC(k)
      END
    END
  END ComputeIndeps;

PROCEDURE ComputeQ(p : LRVector.T; b : REF M.M) : LONGREAL =
  (* value of the quadratic *)
  VAR
    k := 0;
    q : LONGREAL;
    f0, f1 : LONGREAL;
    term : LONGREAL;
    sum := 0.0d0;
  BEGIN
    FOR i := 0 TO NUMBER(p^) DO
      IF i = NUMBER(p^) THEN
        f0 := 1.0d0
      ELSE
        f0 := p[i]
      END;
      FOR j := i TO NUMBER(p^) DO
        IF j = NUMBER(p^) THEN
          f1 := 1.0d0
        ELSE
          f1 := p[j]
        END;
        q := f0 * f1;
        term := q * b[k, 0];
        sum := sum + term;
        INC(k)
      END
    END;
    RETURN sum
  END ComputeQ;

PROCEDURE FmtQ(p : LRVector.T; b : REF M.M) : TEXT =
  VAR
    k := 0;
    sum := "";
    f0, f1 : TEXT;
    q : TEXT;
    term : TEXT;
  BEGIN
    FOR i := 0 TO NUMBER(p^) DO
      IF i = NUMBER(p^) THEN
        f0 := "1"
      ELSE
        f0 := F("p[%s]", Int(i))
      END;
      FOR j := i TO NUMBER(p^) DO
        IF j = NUMBER(p^) THEN
          f1 := "1"
        ELSE
          f1 := F("p[%s]", Int(j))
        END;
        q := f0 & " * " & f1;
        term := LR(b[k, 0]) & " * " & q;
        sum := sum & " + " &  term;
        INC(k)
      END
    END;
    RETURN sum
  END FmtQ;

PROCEDURE MostNegQuadratic(p : LRVector.T; b : REF M.M) : LONGREAL =
  VAR
    k := 0;
    mostNeg := LAST(LONGREAL);
  BEGIN
    FOR i := 0 TO NUMBER(p^) DO
      FOR j := i TO NUMBER(p^) DO
        IF    i = j AND i = NUMBER(p^) THEN
          (* constant term *)
          (* skip *)
        ELSIF i = j THEN
          (* squared term *)
          WITH pi = 2.0d0 * b[k, 0] DO
            mostNeg := MIN(mostNeg, pi)
          END
        ELSIF j = NUMBER(p^) THEN
          (* linear term *)
        ELSE
          (* cross term *)
          WITH pi = b[k, 0] DO
            mostNeg := MIN(mostNeg, pi)
          END;
          WITH pj = b[k, 0] DO
            mostNeg := MIN(mostNeg, pj)
          END
        END;
        INC(k)
      END
    END;
    RETURN mostNeg
  END MostNegQuadratic;
  
PROCEDURE ComputeG(p : LRVector.T; b : REF M.M) : LRVector.T =
  (* gradient of the quadratic *)
  VAR
    res := NEW(LRVector.T, NUMBER(p^));
    k := 0;
  BEGIN
    FOR i := FIRST(p^) TO LAST(p^) DO
      res[i] := 0.0d0
    END;
    
    FOR i := 0 TO NUMBER(p^) DO
      FOR j := i TO NUMBER(p^) DO
        IF    i = j AND i = NUMBER(p^) THEN
          (* constant term *)
          (* skip *)
        ELSIF i = j THEN
          (* squared term *)
          WITH pi = 2.0d0 * p[i] * b[k, 0] DO
            res[i] := res[i] + pi
          END
        ELSIF j = NUMBER(p^) THEN
          (* linear term *)
          WITH pi = b[k, 0] DO
            res[i] := res[i] + pi
          END
        ELSE
          (* cross term *)
          WITH pi = p[j] * b[k, 0] DO
            res[i] := res[i] + pi
          END;
          WITH pj = p[i] * b[k, 0] DO
            res[j] := res[j] + pj
          END
        END;
        INC(k)
      END
    END;
    RETURN res
  END ComputeG;
  
VAR ridgeCoeff := 0.0d0;
    
PROCEDURE Minimize(p              : LRVector.T;
                   func           : MultiEval.T;
                   rhobeg, rhoend : LONGREAL) : Output =
  VAR
    n        := NUMBER(p^);
    nv       := 2 * n;
    da       := NEW(REF ARRAY OF LRVector.T, nv);
    pp       := NEW(REF ARRAY OF LRVector.T, nv);
    lps      := NEW(REF ARRAY OF LineProblem.T, nv);
    rand     := NEW(Random.Default).init();
    mins     := NEW(LineProblemSeq.T).init();
    allMins  := NEW(LineProblemSeq.T).init();
    cl       := NEW(REF ARRAY OF LineMinimizer.T, nv);
    
    message  : TEXT;
    thisCyc  : LRVectorSet.T;
    values   := NEW(LRVectorMRTbl.Default).init();
    peSeq    := NEW(PointEvaluatorSeq.T).init();

    newPts   := NEW(LRVectorSetDef.T).init(); (* interesting points to eval *)

    samples : CARDINAL;
    
  BEGIN
    rho   := rhobeg;
    iter  := 0;
    
    FOR i := 0 TO 2 * n - 1 DO
      cl[i] := NEW(LineMinimizer.T).init()
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
               func = MultiEvaluate(func,
                                    2 * samples,
                                    sigmaK,
                                    values,
                                    thisCyc) DO
            pe.start(q, func);
            INC(i)
          END
        END
      END;

      thisCyc := NEW(LRVectorSetDef.T).init();
      
      Debug.Out(F("StocRobust.Minimize : pass %s; database has %s points",
                  Int(pass), Int(values.size())));
      Debug.Out(F("StocRobust.Minimize : rho=%s [rhoend=%s]", LR(rho), LR(rhoend)));

      <*ASSERT LineMinimizer.Running() = 0*>
        
      FOR i := FIRST(da^) TO LAST(da^) DO
        
        pp[i] := LRVector.Copy(p);
        cl[i].start(pp[i],
                    LRVector.Copy(da[i]),
                    
                    MultiEvaluate(func, samples, sigmaK, values, thisCyc),
                    
                    (* how the HECK do we get the samples to Main? *)
                    (* I think we need to expose extra fields in 
                       PllEvaluator in Main and share it with this
                       module, then we can pass samples that way ... *)
                    
                    rho)
      END;
      
      IF FALSE THEN
        Debug.Out("Robust.m3 : running = " & Int(LineMinimizer.Running())) 
      END;

      Debug.Out("StocRobust : Waiting for points to evaluate ... ");
      FOR i := 0 TO newPts.size() - 1 DO
        EVAL peSeq.get(i).wait()
      END;

      newPts := NEW(LRVectorSetDef.T).init(); (* empty the set *)

      Debug.Out("StocRobust : Waiting for line searches to evaluate ...");
      FOR i := FIRST(da^) TO LAST(da^) DO
        lps[i] := cl[i].wait()
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
        newp := lps[0].minp; (* tentatively set to best line-search point *)
        opt0 := Predict(p, SUBARRAY(pp^, 0, n));
        opt1 := Predict(p, SUBARRAY(pp^, n, n));
      BEGIN
        Debug.Out("About to call DoLeaderBoard.  values.size() = " & Int(values.size()));
        LOCK valueMu DO
          WITH bestq = DoLeaderBoard(p, lps[0].minp, values, newPts) DO
            IF bestq # NIL THEN
              newp := bestq
            END
          END
        END;
        Debug.Out(F("Robust.m3 : opt0 (%s) ; opt1 (%s)",
                    M.FormatV(opt0^),
                    M.FormatV(opt1^)));
        
        Debug.Out(F("Robust.m3 : updating p (%s) -> (%s)",
                    M.FormatV(p^),
                    M.FormatV(newp^)));

        WITH dp = LRVector.Copy(p) DO
          M.SubV(newp^, p^, dp^);
          rho := M.Norm(dp^);
          Debug.Out(F("Robust.m3 : new rho = %s", LR(rho)));
          IF rho < rhoend THEN
            message := "stopping because rho < rhoend";
            EXIT
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
        
        M.SubV(opt0^, newp^, da[0]^);
        M.SubV(opt1^, newp^, da[n]^);

        p^ := newp^;
        mins.addhi(lps[0]);
        allMins.addhi(lps[0]);

        WITH Lookback = 5 DO

          (* 
             if we haven't improved in five straight iterations,
             call it a day 
          *)
          
          IF mins.size() > Lookback THEN
            WITH old = mins.get(mins.size() - Lookback) DO
              IF old.minval <= lps[0].minval THEN
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
        LRScalarFieldPll.T(pll) => pll.clearTbls()
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
      
      FindBest(mins, bestval, bestv);

      Debug.Out("Robust.m3 : " & message);
      
      RETURN Output { iterations := iter,
                      funcCount  := 0,
                      fhist      := GetFHist(allMins),
                      message    := message,
                      f          := bestval,
                      x          := bestv }
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

BEGIN END StocRobust.
