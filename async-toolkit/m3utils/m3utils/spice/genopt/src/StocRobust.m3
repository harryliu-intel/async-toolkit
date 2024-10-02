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
IMPORT PointResult;
IMPORT PointResultSeq;

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

PROCEDURE ResMetric(READONLY r : MultiEval.Result;
                    sigmaK     : LONGREAL) : LONGREAL =
  (* metric as observed from direct observations *)
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

PROCEDURE DoLeaderBoard(READONLY pr   : PointResult.T; (* current [old] point *)
                        values        : LRVectorMRTbl.T;
                        newpts(*OUT*) : LRVectorSet.T) : PointResult.T =
  VAR
    parr := NEW(REF ARRAY OF PointMetric.T, values.size());
    iter := values.iterate();
    q : LRVector.T;
    r : MultiEval.Result;
    i := 0;
    n := NUMBER(pr.p^);
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
                           MultiEval.Format(rec.result),
                           FmtP(rec.p)))
        END
      END;
      Debug.Out(Wx.ToText(wx))
    END;

    WITH dofs = Qdofs(n) DO
      IF NUMBER(parr^) >= dofs THEN
        Debug.Out(F("DoLeaderBoard: enough points (%s >= %s) to attempt a surface fit.", Int(NUMBER(parr^)), Int(dofs)));
        RETURN AttemptSurfaceFit(pr, parr, newpts)
      ELSE
        Debug.Out(F("DoLeaderBoard: NOT enough points (%s < %s) to attempt a surface fit.", Int(NUMBER(parr^)), Int(dofs)));
        RETURN pr
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

PROCEDURE AttemptSurfaceFit(pr            : PointResult.T;
                            parr          : REF ARRAY OF PointMetric.T;
                            newpts(*OUT*) : LRVectorSet.T) : PointResult.T =
  (* attempt a surface fit *)
  
  VAR
    n       := NUMBER(pr.p^);

    tryrho  := rho;
    success := FALSE;
    qdofs   := Qdofs(n);
    ldofs   := Ldofs(n);
    
    seq   : PointMetricSeq.T;
    bestQ : PointResult.T;
  BEGIN
    (* first, pick enough points for the fit *)
    LOOP
      Debug.Out(F("AttemptSurfaceFit : tryrho=%s ; starting from %s",
                  LR(tryrho), PointResult.Format(pr)));
      
      seq := NEW(PointMetricSeq.T).init();
      FOR i := FIRST(parr^) TO LAST(parr^) DO
        WITH rec  = parr[i],
             dist = Vdist(rec.p, pr.p) DO
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

    AttemptStatFits (parr);

    Debug.Out(F("AttemptSurfaceFit: seq.size()=%s rho=%s tryrho=%s success=%s p=%s",
                Int(seq.size()), LR(rho), LR(tryrho), Bool(success),
                PointResult.Format(pr)));

    VAR
      xquad    := NEW(REF M.M, seq.size(), qdofs);
      xlin     := NEW(REF M.M, seq.size(), ldofs);

      ynom     := NEW(REF M.M, seq.size(), 1);
      ymu      := NEW(REF M.M, seq.size(), 1);
      w        := NEW(REF M.M, seq.size(), seq.size());

      ynomHat,
      ymuHatXX   : REF M.M;

      rnom     := NEW(Regression.T);
      rmu      := NEW(Regression.T);

      bestfits := AttemptStatFits2(parr); (* linear fit, in quadratic format,
                                             to sigma and mu *)

    BEGIN
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

          ynom  [i, 0] := MultiEval.Nominal(rec.result);
          ymu   [i, 0] := MultiEval.Mean   (rec.result);
          
          w[i, i]      := FLOAT(rec.result.n, LONGREAL)
        END
      END(*ROF*);

      IF doNominal THEN
        Debug.Out("StocRobust.AttemptSurfaceFit : doNominal is true, attempting to do a nominal fit");

        Regression.Run(xquad,
                       ynom  , ynomHat , FALSE, rnom, h := ridgeCoeff);
        rmu.b := bestfits.bmu;
      ELSE
        Regression.Run(xquad,
                       ymu   , ymuHatXX, FALSE, rmu, h := ridgeCoeff, W := w);
      END;
      
      IF doNominal THEN
        FOR i := 0 TO Qdofs(n) - 1 DO
          Debug.Out(F("rnom.b[%s,0] = %s",
                      Int(i), LR(rnom.b[i,0])))
        END;
      
        Debug.Out("nom   = " & FmtQ(n, rnom.b));
      END;
      Debug.Out  ("mu    = " & FmtQ(n, rmu.b));
      Debug.Out  ("sigma = " & FmtQ(n, bestfits.bsigma));

      IF doNominal THEN
        PROCEDURE Do(pp : LRVector.T) =
          BEGIN
            WITH ppnorm  = LRVector.Norm(pp),
                 ppnorm2 = ppnorm * ppnorm,
                 q       = ComputeQ(pp, rnom.b) DO
              Debug.Out(F("pp %s ; ppnorm2 %s ; q %s",
                          FmtP(pp), LR(ppnorm2), LR(q)));
            END
          END Do;
          
        VAR
          pp      := LRVector.Copy(pr.p);
        BEGIN
          Do(pp);   
          M.ZeroV(pp^);
          pp[0] := 1.0d0;
          Do(pp)
        END
      END;

      WITH wx    = Wx.New(),
           pparr = NEW(REF ARRAY OF PointMetric.T, seq.size()) DO
        Wx.PutText(wx, "=====  QUADRATIC PREDICTION  =====\n");
        FOR i := 0 TO seq.size() - 1 DO
          WITH rec     = seq.get(i),

               pnorm   = LRVector.Norm(rec.p),
               pnorm2  = pnorm * pnorm,
               
               nom     = ynomHat[i,0],

               p2nom   = ComputeQ(rec.p, rnom.b (*, wx *) ),
               p2mu    = ComputeQ(rec.p, rmu.b),
               p2sigma = ComputeQ(rec.p, bestfits.bsigma),
               pred2   = nom + p2mu + sigmaK * p2sigma DO


            Wx.PutText(wx, FN("metric %s : res %s : predmetric (%s = %s + %s + %s * %s); rec.p %s ; pnorm2 %s\n", TA{
            LR(rec.metric),
            MultiEval.Format(rec.result),
            LR(pred2),
            LR(p2nom), LR(p2mu), LR(sigmaK), LR(p2sigma),
            FmtP(rec.p),
            LR(pnorm2)}));

            pparr[i] := PointMetric.T { pred2, rec.p, rec.result }
          END
        END(* ROF *);

        (* pparr contains these points tagged by their predicted
           metric *)
        PointMetricArraySort.Sort(pparr^);
        Wx.PutText(wx, "=====  SORTED QUADRATIC PREDICTION  =====\n");

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
        END;

        (* 
           at this point, 
           
           parr[0] is the best point we have evaluated.
           
           pparr[0] is the best point we have seen according to its
           quadratic prediction
        *)

        Debug.Out("parr[0]  = " & PointMetric.Format( parr[0]));
        Debug.Out("pparr[0] = " & PointMetric.Format(pparr[0]));
        

        PROCEDURE SigVal(READONLY res : MultiEval.Result;
                         k            : LONGREAL) : LONGREAL =
          BEGIN
            WITH pow    = Math.pow,

                 nf     = FLOAT(res.n, LONGREAL),
                 
                 nom    = MultiEval.Nominal(res),
                 mu     = MultiEval.Mean(res),
                 sig    = MultiEval.Sdev(res),
                 
                 muStd  = sig / Math.sqrt(nf),
                 (* std error of the mean *)

                 sigStd = pow(2.0d0 * pow(sig, 4.0d0) / (nf - 1.0d0), 0.25d0), 
                 (* estimator of std error of sdev *)

                 totStd = muStd + sigStd,

                 metric = nom + mu + sigmaK * sig,
                 (* implied metric *)
                 
                 res = metric + k * totStd
             DO
              RETURN res
            END
          END SigVal;
                 
        BEGIN
          WITH ppmet = pparr[0].metric,
               ppmin = SigVal(pparr[0].result, -3.0d0),
               ppmax = SigVal(pparr[0].result, +3.0d0),
               pmax  = SigVal( parr[0].result, +6.0d0),
               pmet  = parr[0].metric
           DO
            Debug.Out(F("pp %s ppmet %s ppmin %s ppmax %s",
                        FmtP(pparr[0].p),
                        LR(ppmet),
                        LR(ppmin),
                        LR(ppmax)));
            Debug.Out(F("p  %s pmax %s",
                        FmtP(parr[0].p),
                        LR(pmax)));
            IF ppmet < ppmin OR ppmet > ppmax THEN
              Debug.Out("pparr[0] untrustworthy");
              bestQ :=
                  PointResult.T { LRVector.Copy(parr[0].p), pmet, FALSE, rho }
            ELSIF ppmet > pmax THEN
              Debug.Out("pparr[0] bad");
              bestQ :=
                  PointResult.T { LRVector.Copy(parr[0].p), pmet, FALSE, rho }
            ELSE
              bestQ :=
                  PointResult.T { LRVector.Copy(pparr[0].p), ppmet, TRUE, rho }
            END
          END
        END
      END(*WITH*);

      DoSimpleFit(pr.p,
                  rnom.b,
                  bestfits.bmu,
                  bestfits.bsigma,
                  tol    := GetConstantTerm(bestfits.bsigma) / 10.0d0,
                  newpts := newpts);

      (* bestQ is the best point we know on the fitted curve *)
      (* grab the nearest points from the seq and re-sample them *)

      InsertClosestPoints(n, bestQ.p, parr^, newpts);
      
      RETURN bestQ
    END
  END AttemptSurfaceFit;

PROCEDURE Rss(a, b : REF M.M) : LONGREAL =
  BEGIN
    RETURN M.SumDiffSqM(a^, b^)
  END Rss;

PROCEDURE VarM(a : REF M.M) : LONGREAL =
  BEGIN
    RETURN M.DevSqM(a^)
  END VarM;
  
PROCEDURE AttemptStatFits(parr : REF ARRAY OF PointMetric.T) =

  PROCEDURE DoFit(READONLY parr : ARRAY OF PointMetric.T) =
    VAR
      m := NUMBER(parr);

      n := NUMBER(parr[FIRST(parr)].p^);
      
      qdofs  := Qdofs(n);
      ldofs  := Ldofs(n);
      
      xquad  := NEW(REF M.M, m, qdofs);
      xlin   := NEW(REF M.M, m, ldofs);

      ymu    := NEW(REF M.M, m, 1);
      ysigma := NEW(REF M.M, m, 1);
      w      := NEW(REF M.M, m, m);

      ymuHat1,
      ysigmaHat1 : REF M.M;

      ymuHat2,
      ysigmaHat2 : REF M.M;

      rmu1    := NEW(Regression.T);
      rsigma1 := NEW(Regression.T);
      rmu2    := NEW(Regression.T);
      rsigma2 := NEW(Regression.T);

    BEGIN
      M.Zero(w^);
      FOR i := 0 TO m - 1 DO
        WITH rec = parr[i] DO
          ComputeIndepsQ(rec.p, i, xquad^);
          ComputeIndepsL(rec.p, i, xlin^);

          ymu   [i, 0] := MultiEval.Mean   (rec.result);
          ysigma[i, 0] := MultiEval.Sdev   (rec.result);
          
          w[i, i]      := FLOAT(rec.result.n, LONGREAL)
        END
      END(*ROF*);
 
      Regression.Run(xlin,
                     ymu   , ymuHat1   , FALSE, rmu1   , 0.0d0, W := w);
      Regression.Run(xlin,
                     ysigma, ysigmaHat1, FALSE, rsigma1, 0.0d0, W := w);
      Regression.Run(xquad,
                     ymu   , ymuHat2   , FALSE, rmu2   , 0.0d0, W := w);
      Regression.Run(xquad,
                     ysigma, ysigmaHat2, FALSE, rsigma2, 0.0d0, W := w);

      WITH rssMu0  = VarM(ymu),
           rssMu1  = Rss(ymu   , ymuHat1),
           rssMu2  = Rss(ymu   , ymuHat2),
           rssSig0 = VarM(ysigma),
           rssSig1 = Rss(ysigma, ysigmaHat1),
           rssSig2 = Rss(ysigma, ysigmaHat2) DO
        Debug.Out(FN("AttemptStatFits mu  : m=%s rss0=%s rss1=%s rss2=%s",
                     TA{Int(m), LR(rssMu0), LR(rssMu1), LR(rssMu2) }));
        Debug.Out(FN("AttemptStatFits sig : m=%s rss0=%s rss1=%s rss2=%s",
                     TA{Int(m), LR(rssSig0), LR(rssSig1), LR(rssSig2) }));
      END
      
    END DoFit;

  VAR
    m := 1;
  BEGIN
    REPEAT
      DoFit(SUBARRAY(parr^, 0, m));
      m := m * 2
    UNTIL m > NUMBER(parr^);
  END AttemptStatFits;

TYPE
  StatFits = RECORD
    l           : LONGREAL; (* log likelihood of fit *)
    bmu, bsigma : REF M.M;  (* these are quadratic fits *)
  END;
  
PROCEDURE AttemptStatFits2(parr : REF ARRAY OF PointMetric.T) : StatFits =

  PROCEDURE DoFit(READONLY parr, varr : ARRAY OF PointMetric.T) =
    VAR
      m  := NUMBER(parr);
      mf := FLOAT(m, LONGREAL);
      

      n := NUMBER(parr[FIRST(parr)].p^);
      
      qdofs  := Qdofs(n);
      ldofs  := Ldofs(n);
      
      xquad  := NEW(REF M.M, m, qdofs);
      xlin   := NEW(REF M.M, m, ldofs);

      ymu    := NEW(REF M.M, m, 1);
      ysigma := NEW(REF M.M, m, 1);
      w      := NEW(REF M.M, m, m);

      ymuHat,
      ysigmaHat : REF M.M;


      rmu    := NEW(Regression.T);
      rsigma := NEW(Regression.T);

      sumlMu, sumlSig := 0.0d0;

    BEGIN
      M.Zero(w^);
      FOR i := 0 TO m - 1 DO
        WITH rec = parr[i] DO
          ComputeIndepsQ(rec.p, i, xquad^);
          ComputeIndepsL(rec.p, i, xlin^);

          ymu   [i, 0] := MultiEval.Mean   (rec.result);
          ysigma[i, 0] := MultiEval.Sdev   (rec.result);
          
          w[i, i]      := FLOAT(rec.result.n, LONGREAL)
        END
      END(*ROF*);
 
      Regression.Run(xlin,
                     ymu   , ymuHat   , FALSE, rmu   , 0.0d0, W := w);
      Regression.Run(xlin,
                     ysigma, ysigmaHat, FALSE, rsigma, 0.0d0, W := w);

      WITH rssMu  = Rss(ymu   , ymuHat),
           sMu    = Math.sqrt(rssMu / mf),
           rssSig = Rss(ysigma, ysigmaHat),
           sSig   = Math.sqrt(rssSig / mf) DO
        Debug.Out(FN("AttemptStatFits2 m=%s rssMu=%s rssSig=%s ; sMu=%s sSig=%s",
                     TA{Int(m), LR(rssMu), LR(rssSig), LR(sMu), LR(sSig)}));
        
        Debug.Out  ("mu    = " & FmtL(n, rmu.b));
        Debug.Out  ("sigma = " & FmtL(n, rsigma.b));

        FOR i := FIRST(varr) TO LAST(varr) DO
          WITH v    = varr[i],

               (* data *)
               dmu  = MultiEval.Mean(varr[i].result), 
               dsig = MultiEval.Sdev(varr[i].result),

               (* predicted *)
               pmu  = ComputeL(v.p, rmu.b),
               psig = ComputeL(v.p, rsigma.b),

               (* likelihoods *)
               lmu  = LogLikelihood(dmu, pmu, sMu),
               lsig = LogLikelihood(dsig, psig, sSig) DO

            sumlMu := sumlMu + lmu;
            sumlSig := sumlSig + lsig
          END
        END;

        WITH l = sumlMu + sumlSig DO
          Debug.Out("log mu  likelihood = " & LR(sumlMu));
          Debug.Out("log sig likelihood = " & LR(sumlSig));
          Debug.Out("log likelihood     = " & LR(l));
          IF l > bestl THEN
            bestfits := StatFits { l, L2Q(n, rmu.b^), L2Q(n, rsigma.b^) }
          END
        END;
      END
      
    END DoFit;

  CONST
    LeaveOut = 16;
  VAR
    m := 2;
    max := NUMBER(parr^) - LeaveOut;

    bestl  := FIRST(LONGREAL); (* best fit *)
    bestfits : StatFits;
  BEGIN
    LOOP
      DoFit(SUBARRAY(parr^,           0, MIN(max, m)),
            SUBARRAY(parr^, MIN(max, m), LeaveOut));
      IF m >= max THEN
        EXIT
      END;
      m := m * 2
    END;
    RETURN bestfits
  END AttemptStatFits2;

VAR Pi     := 4.0d0 * Math.atan(1.0d0);
    Log2Pi := Math.log(2.0d0 * Pi);
    
PROCEDURE LogLikelihood(x, mu, sigma : LONGREAL) : LONGREAL =
  (* likelihood (density) of x given it is ~N(mu, sigma) *)
  CONST
    log = Math.log;
  BEGIN
    WITH t1 = -0.5d0 * Log2Pi,
         t2 = -log(sigma),
         dev = x - mu,
         t3 = -0.5d0 / sigma / sigma * dev * dev DO
      RETURN t1 + t2 + t3
    END
  END LogLikelihood;
  
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
             sigmaK := sigmaK,
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
                           sigmaK, bsigma^,
                           bb^)
    ELSE
      qf := NEW(QuadraticF, b := b, eval := EvalQF);
      qg := NEW(QuadraticG, b := b, eval := EvalQG);
      M.LinearCombination(1.0d0 , bmu^,
                          sigmaK, bsigma^,
                          bb^)
    END;
    
    Debug.Out("bb    = " & FmtQ(n, bb));
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
    bnom, bmu, bsigma : REF M.M;
    sigmaK            : LONGREAL;
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
  
PROCEDURE EvalQFN(qf : QuadraticF; p : LRVector.T) : LONGREAL =
  BEGIN
    WITH nom   = ComputeQ(p, qf.b.bnom),
         mu    = ComputeQ(p, qf.b.bmu),
         sigma = ComputeQ(p, qf.b.bsigma),
         spice = qf.b.alpha * M.SumDiffSqV(p^, qf.b.p^) DO
      <*ASSERT doNominal*>
      RETURN nom + mu + qf.b.sigmaK * sigma + spice
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
                            qg.b.sigmaK, sigmaG^,
                            res^);

      FOR i := FIRST(res^) TO LAST(res^) DO
        res[i] := res[i] + 2.0d0 * qg.b.alpha * (p[i] - qg.b.p[i])
      END;
      
      RETURN res
    END
  END EvalQGN;
  
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

PROCEDURE ComputeQ(p : LRVector.T; b : REF M.M; wx : Wx.T := NIL) : LONGREAL =
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
        IF wx # NIL THEN
          Wx.PutText(wx, FN("ComputeQ f0=%s f1=%s q=%s b[%s,0]=%s term=%s sum=%s\n",
                            TA{LR(f0), LR(f1), LR(q), Int(k), LR(b[k,0]),
                               LR(term), LR(sum)}))
        END;
        INC(k)
      END
    END;
    IF wx # NIL THEN
      Wx.PutText(wx,"===\n")
    END;
    RETURN sum
  END ComputeQ;

PROCEDURE ComputeL(p : LRVector.T; b : REF M.M) : LONGREAL =
  VAR
    sum := 0.0d0;
    f0 : LONGREAL;
  BEGIN
    FOR i := 0 TO NUMBER(p^) DO
      IF i = NUMBER(p^) THEN
        f0 := 1.0d0
      ELSE
        f0 := p[i]
      END;
      WITH term = f0 * b[i, 0] DO
        sum := sum + term
      END
    END;
    RETURN sum
  END ComputeL;

PROCEDURE L2Q(n : CARDINAL; READONLY b : M.M) : REF M.M =
  VAR
    ldofs := Ldofs(n);
    qdofs := Qdofs(n);
    res := NEW(REF M.M, qdofs, 1);
    k := 0;
  BEGIN
    <*ASSERT NUMBER(b) = ldofs*>
    <*ASSERT NUMBER(b[0]) = 1*>
    FOR i := 0 TO n DO
      FOR j := i TO n DO
        IF    i = n THEN
          res[ k, 0 ] := b[ j, 0 ]
        ELSIF j = n THEN
          res[ k, 0 ] := b[ i, 0 ]
        ELSE
          res[ k, 0 ] := 0.0d0
        END;
        
        INC(k)
      END
    END;
    RETURN res
  END L2Q;

PROCEDURE FmtL(n : CARDINAL; b : REF M.M) : TEXT =
  VAR
    ldofs := Ldofs(n);
    term : TEXT;
    sum := "";
  BEGIN
    <*ASSERT NUMBER(b^) = ldofs*>
    <*ASSERT NUMBER(b[0]) = 1*>
    FOR i := 0 TO n DO
      IF i = n THEN
        term := LR(b[i, 0])
      ELSE
        term := LR(b[i, 0]) & F(" * p[%s]", Int(i))
      END;
      sum := sum & " + " & term
    END;
    RETURN sum
  END FmtL;
  
PROCEDURE FmtQ(n : CARDINAL; b : REF M.M) : TEXT =
  VAR
    k := 0;
    sum := "";
    f0, f1 : TEXT;
    q : TEXT;
    term : TEXT;
  BEGIN
    FOR i := 0 TO n DO
      IF i = n THEN
        f0 := "1"
      ELSE
        f0 := F("p[%s]", Int(i))
      END;
      FOR j := i TO n DO
        IF j = n THEN
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
    
PROCEDURE Minimize(pa             : LRVector.T;
                   func           : MultiEval.T;
                   rhobeg, rhoend : LONGREAL) : Output =
  VAR
    n        := NUMBER(pa^);
    nv       := 2 * n;
    da       := NEW(REF ARRAY OF LRVector.T, nv);
    pp       := NEW(REF ARRAY OF LRVector.T, nv);
    lps      := NEW(REF ARRAY OF LineProblem.T, nv);
    rand     := NEW(Random.Default).init();
    mins     := NEW(PointResultSeq.T).init();
    allMins  := NEW(PointResultSeq.T).init();
    cl       := NEW(REF ARRAY OF LineMinimizer.T, nv);
    
    message  : TEXT;
    thisCyc  : LRVectorSet.T;
    values   := NEW(LRVectorMRTbl.Default).init();
    peSeq    := NEW(PointEvaluatorSeq.T).init();

    newPts   := NEW(LRVectorSetDef.T).init(); (* interesting points to eval *)

    samples : CARDINAL;

    pr       := PointResult.T { pa, LAST(LONGREAL), FALSE, LAST(LONGREAL) };
    
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
      END(*RAV*);

      LOCK valueMu DO
        thisCyc := NEW(LRVectorSetDef.T).init()
      END;
      
      Debug.Out(F("StocRobust.Minimize : pass %s; database has %s points",
                  Int(pass), Int(values.size())));
      Debug.Out(F("StocRobust.Minimize : rho=%s [rhoend=%s]", LR(rho), LR(rhoend)));

      <*ASSERT LineMinimizer.Running() = 0*>
        
      FOR i := FIRST(da^) TO LAST(da^) DO
        
        pp[i] := LRVector.Copy(pr.p);
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

        WITH dp = LRVector.Copy(pr.p) DO
          M.SubV(newp.p^, pr.p^, dp^);
          (* rho can only decrease by 4 *)
          rho := 0.50d0 * rho + 0.50d0 * M.Norm(dp^);
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
        
        M.SubV(opt0^, newp.p^, da[0]^);
        M.SubV(opt1^, newp.p^, da[n]^);

        pr := newp;
        mins.addhi   (newp);
        allMins.addhi(newp);

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
