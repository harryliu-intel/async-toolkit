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
IMPORT Thread;
IMPORT QuadraticFit;

CONST LR = LongReal;

TYPE TA = ARRAY OF TEXT;

VAR doDebug := Debug.DebugThis("StocRobust");

VAR sigmaK := 0.0d0;
    
PROCEDURE SetSigmaK(k : LONGREAL) =
  BEGIN sigmaK := k END SetSigmaK;

PROCEDURE GetSigmaK() : LONGREAL =
  BEGIN RETURN sigmaK END GetSigmaK;

VAR doNominal := FALSE;

PROCEDURE SetDoNominal(to : BOOLEAN) =
  BEGIN doNominal := to END SetDoNominal;

PROCEDURE GetDoNominal() : BOOLEAN =
  BEGIN RETURN doNominal END GetDoNominal;

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

TYPE
  NomClosure = Thread.Closure OBJECT
    me  : MultiEval.T;
    at  : LRVector.T;
    res : LONGREAL;
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
    Poison = FIRST(LONGREAL);
  VAR
    res, oldres : MultiEval.Result;
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
      Debug.Out("Launching nominal eval at " & FmtP(p));
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
        res := MultiEval.Combine(res, oldres);
      END;
      IF doDebug THEN
        Debug.Out("adding new entry to me.values")
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

    WITH nominal = MultiEval.Nominal(res),
         mean    = MultiEval.Mean(res),
         sdev    = MultiEval.Sdev(res),
         retval  = nominal + mean + me.sigmaK * sdev DO
      <*ASSERT nominal # Poison*>
      IF doDebug THEN
        Debug.Out(F("StocRobust.MEEval : nominal=%s mean=%s sigmaK=%s sdev=%s -> retval=%s",
                    LR(nominal), LR(mean), LR(me.sigmaK), LR(sdev), LR(retval)))
      END;
      RETURN retval
    END
  END MEEval;

PROCEDURE ResMetric(READONLY r : MultiEval.Result; sigmaK : LONGREAL) : LONGREAL =
  BEGIN
    WITH nom    = MultiEval.Nominal(r),
         mean   = MultiEval.Mean(r),
         sdev   = MultiEval.Sdev(r),
         retval = nom + mean + sigmaK * sdev DO
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
                        values        : LRVectorMRTbl.T;
                        newpts(*OUT*) : LRVectorSet.T) : LRVector.T =
  VAR
    parr := NEW(REF ARRAY OF PointMetric.T, values.size());
    iter := values.iterate();
    q : LRVector.T;
    r : MultiEval.Result;
    i := 0;
    n := NUMBER(p^);
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
          (*  *)
          Wx.PutText(wx, F("metric %s ; dist %s; result %s ; rec.p %s\n",
                           LR(rec.metric),
                           LR(dist),
                           MultiEval.Format(rec.result),
                           FmtP(rec.p)))
        END
      END;
      Debug.Out(Wx.ToText(wx))
    END;

    WITH dofs = Qdofs(n) DO
      IF NUMBER(parr^) >= dofs THEN
        Debug.Out(F("DoLeaderBoard: enough points (%s >= %s) to attempt a surface fit.", Int(NUMBER(parr^)), Int(dofs)));
        RETURN AttemptSurfaceFit(p, parr, newpts)
      ELSE
        Debug.Out(F("DoLeaderBoard: NOT enough points (%s < %s) to attempt a surface fit.", Int(NUMBER(parr^)), Int(dofs)));
        RETURN NIL
      END
    END
  END DoLeaderBoard;

PROCEDURE Ldofs(n : CARDINAL) : CARDINAL =
  BEGIN
    RETURN n + 1
  END Ldofs;
  
PROCEDURE Qdofs(n : CARDINAL) : CARDINAL =
  BEGIN
    RETURN (n * n + 3 * n + 2) DIV 2
  END Qdofs;

PROCEDURE AttemptSurfaceFit(p             : LRVector.T;
                            parr          : REF ARRAY OF PointMetric.T;
                            newpts(*OUT*) : LRVectorSet.T) : LRVector.T =
  (* attempt a surface fit *)
  
  VAR
    n       := NUMBER(p^);

    tryrho  := rho;
    success := FALSE;
    qdofs   := Qdofs(n);
    ldofs   := Ldofs(n);
    
    seq   : PointMetricSeq.T;
    bestQ : LRVector.T;
  BEGIN
    (* first, pick enough points for the fit *)
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
      IF seq.size() >= qdofs THEN
        success := TRUE;
        EXIT
      ELSE
        tryrho := tryrho * 2.0d0
      END
    END;

    (* we have chosen enough points to attempt a fit *)
    
    (***************************************************************)
    
    (* we implement two prediction methods:

       1. mu + sigma  [doNominal = FALSE]
         
            we use a quadratic fit for mu
            we use a linear fit for sigma

       2. nominal + mu + sigma [doNominal = TRUE]
         
            we use a quadratic (forced concave) fit for nominal
            we use a linear fit for mu
            we use a linear fit for sigma
      *)
          

    Debug.Out(F("DoLeaderBoard: seq.size()=%s rho=%s tryrho=%s success=%s",
                Int(seq.size()), LR(rho), LR(tryrho), Bool(success)));

    PROCEDURE PredNominal(pp : LRVector.T) : LONGREAL =
      BEGIN
        IF doNominal THEN
          RETURN qf.pred(pp)
        ELSE
          RETURN 0.0d0
        END
      END PredNominal;
      
    VAR
      xquad  := NEW(REF M.M, seq.size(), qdofs);
      xlin   := NEW(REF M.M, seq.size(), ldofs);
      ysigma := NEW(REF M.M, seq.size(), 1);
      ymu    := NEW(REF M.M, seq.size(), 1);
      w      := NEW(REF M.M, seq.size(), seq.size());
      ymuHat, ysigmaHat : REF M.M;
      rmu    := NEW(Regression.T);
      rsigma := NEW(Regression.T);

      qf     : QuadraticFit.T;
    BEGIN
      IF doNominal THEN
        qf := NEW(QuadraticFit.T).init(n, rho);
      END;
      
      M.Zero(w^);
      FOR i := 0 TO seq.size() - 1 DO
        WITH rec = seq.get(i) DO
          (* 
             compute two forms of independents:
             xquad for quadratic fit
             xlin  for linear fit 
          *)
          ComputeIndepsQ(rec.p, i, xquad^);
          ComputeIndepsL(rec.p, i, xlin^);

          IF doNominal THEN
            qf.addPoint(rec.p,
                        MultiEval.Nominal(rec.result),
                        1.0d0) (* nominal isnt weighted *)
          END;
          
          ymu   [i, 0] := MultiEval.Mean(rec.result);
          ysigma[i, 0] := MultiEval.Sdev(rec.result);
          w[i, i]      := FLOAT(rec.result.n, LONGREAL)
        END
      END(*ROF*);

      IF doNominal THEN
        Debug.Out("StocRobust.AttemptSurfaceFit : doNominal is true, attempting to do a nominal fit");
        
        WITH minNom    = qf.getMinimum(),
             minNomVal = qf.pred(minNom) DO
          Debug.Out(F("Nominal QuadraticFit min %s @ %s",
                      LR(minNomVal), FmtP(minNom)))
        END;
        
        Regression.Run(xlin,
                       ymu   , ymuHat  , FALSE, rmu, h := ridgeCoeff, W := w);
        rmu.b := L2Q(n, rmu.b^)
      ELSE
        Regression.Run(xquad,
                       ymu   , ymuHat  , FALSE, rmu, h := ridgeCoeff, W := w);
      END;
      
      Regression.Run(xlin,
                     ysigma, ysigmaHat, FALSE, rsigma, h := ridgeCoeff, W := w);

      rsigma.b := L2Q(n, rsigma.b^);

      WITH wx    = Wx.New(),
           pparr = NEW(REF ARRAY OF PointMetric.T, seq.size()) DO
        Wx.PutText(wx, "=====  QUADRATIC PREDICTION  =====\n");
        FOR i := 0 TO seq.size() - 1 DO
          WITH rec     = seq.get(i),
               nom     = PredNominal(rec.p),
               pred    = nom + ymuHat[i,0] + sigmaK * ysigmaHat[i,0],
               p2mu    = ComputeQ(rec.p, rmu.b),
               p2sigma = ComputeQ(rec.p, rsigma.b),
               pred2   = p2mu + sigmaK * p2sigma DO
            
            Wx.PutText(wx, FN("metric %s : res %s, pred { nom %s; mu %s ; sig %s } : predmetric %s =? %s; rec.p %s\n", TA{
            LR(rec.metric),
            MultiEval.Format(rec.result),
            LR(nom),
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
      IF doNominal THEN
      ELSE
        DoSimpleFit(p,
                    rmu,
                    rsigma,
                    tol    := ysigmaHat[0, 0] / 10.0d0,
                    newpts := newpts)
      END;

      (* bestQ is the best point we know on the fitted curve *)
      (* grab the nearest points from the seq and re-sample them *)

      InsertClosestPoints(n, bestQ, parr^, newpts);
      
      RETURN bestQ
    END
  END AttemptSurfaceFit;


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
  END InsertClosestPoints;

PROCEDURE DoSimpleFit(p           : LRVector.T;
                      rmu, rsigma : Regression.T;
                      tol         : LONGREAL;
                      newpts      : LRVectorSet.T
                      ) =
  VAR
    b := NEW(B,
             bmu    := rmu.b,
             bsigma := rsigma.b,
             sigmaK := sigmaK,
             alpha  := 0.0d0,
             p      := p);
    qf      := NEW(QuadraticF, b := b);
    qg      := NEW(QuadraticG, b := b);
    mp      := LRVector.Copy(p);
    
    bdims   := M.GetDim(rmu.b^);
    bb      := M.NewM(bdims);
    beta    := 0.0d0;
    success := FALSE;
    
    best    : LONGREAL;
    biggest : LONGREAL;
  BEGIN
    M.LinearCombination(1.0d0, rmu.b^, sigmaK, rsigma.b^, bb^);
    Debug.Out("bb    = " & FmtQ(p, bb));
    biggest := BiggestQuadratic(p, bb);
    
    (* here biggest is the most negative quadratic coefficient *)
    Debug.Out("quadratic biggest = " & LR(biggest));
    
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
      <*ASSERT NOT doNominal*>
      RETURN mu + qf.b.sigmaK * sigma + spice
    END
  END EvalQF;

PROCEDURE EvalQG(qg : QuadraticG; p : LRVector.T) : LRVector.T =
  BEGIN
    WITH muG    = ComputeG(p, qg.b.bmu),
         sigmaG = ComputeG(p, qg.b.bsigma),
         res    = NEW(LRVector.T, NUMBER(p^)) DO
      <*ASSERT NOT doNominal*>
      M.LinearCombinationV(1.0d0, muG^, qg.b.sigmaK, sigmaG^, res^);

      FOR i := FIRST(res^) TO LAST(res^) DO
        res[i] := res[i] + 2.0d0 * qg.b.alpha * (p[i] - qg.b.p[i])
      END;
      
      RETURN res
    END
  END EvalQG;
  
PROCEDURE ComputeIndepsQ(p      : LRVector.T;
                         row    : CARDINAL;
                         VAR x  : M.M) =
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
        
        q := f0 * f1; (* this is the value we want *)
        
        x[row, k] := q;
        
        INC(k)
      END
    END
  END ComputeIndepsQ;

PROCEDURE ComputeIndepsL(p      : LRVector.T;
                         row    : CARDINAL;
                         VAR x  : M.M) =
  VAR
    k  := 0;
    f0 : LONGREAL;
  BEGIN
    FOR i := 0 TO NUMBER(p^) DO
      IF i = NUMBER(p^) THEN
        f0 := 1.0d0
      ELSE
        f0 := p[i]
      END;
        
      x[row, k] := f0;
      
      INC(k)
    END
  END ComputeIndepsL;

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

PROCEDURE L2Q(n : CARDINAL; READONLY b : M.M) : REF M.M =
  VAR
    ldofs := Ldofs(n);
    qdofs := Qdofs(n);
    res := NEW(REF M.M, qdofs, 1);
    f0, f1, q : LONGREAL;
    k := 0;
  BEGIN
    <*ASSERT NUMBER(b) = ldofs*>
    <*ASSERT NUMBER(b[0]) = 1*>
    FOR i := 0 TO n DO
      IF i = n THEN
        f0 := 1.0d0
      ELSE
        f0 := b[ i, 0 ]
      END;
      FOR j := i TO n DO
        IF j = n THEN
          f1 := 1.0d0
        ELSE
          f1 := b[ j, 0 ]
        END;
        
        q := f0 * f1; (* this is the value we want *)

        IF i = n OR j = n THEN
          res[ k, 0 ] := q
        ELSE
          res[ k, 0 ] := 0.0d0
        END;
        
        INC(k)
      END
    END;
    RETURN res
  END L2Q;

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

PROCEDURE BiggestQuadratic(p : LRVector.T; b : REF M.M) : LONGREAL =
  VAR
    k := 0;
    mostNeg := LAST(LONGREAL);
  BEGIN
    (* we care about negative squared terms and any cross term *)
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
            mostNeg := MIN(-ABS(mostNeg), pi)
          END;
          WITH pj = b[k, 0] DO
            mostNeg := MIN(-ABS(mostNeg), pj)
          END
        END;
        INC(k)
      END
    END;
    RETURN -mostNeg
  END BiggestQuadratic;
  
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

      LOCK valueMu DO
        thisCyc := NEW(LRVectorSetDef.T).init()
      END;
      
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
          WITH bestq = DoLeaderBoard(p, values, newPts) DO
            (* XXX should fix this *)
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
          (* rho can only decrease by 4 *)
          rho := 0.25d0 * rho + 0.75d0 * M.Norm(dp^);
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
