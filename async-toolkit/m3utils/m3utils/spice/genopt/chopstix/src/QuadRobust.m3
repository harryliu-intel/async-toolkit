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
FROM Fmt IMPORT LongReal, F, Int, Bool, FN, Pad;
IMPORT LineProblem;
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

IMPORT PointMetricLR; (* for StatFits *)

IMPORT Wx;
IMPORT ConjGradient;
IMPORT Thread;
IMPORT PointResult;
IMPORT PointResultSeq;
IMPORT NormalDeviate;
IMPORT Process;
FROM SurfaceRep IMPORT Qdofs, Ldofs, 
                       FmtQ, BiggestQuadratic, ComputeG,
                       ComputeQ;
IMPORT StatFits;
IMPORT Matrix;
IMPORT SchemeSymbol;
IMPORT ResponseModel;
IMPORT StatComponent;
IMPORT ModelVar;
IMPORT ModelVarSeq;
IMPORT SchemeObject;
IMPORT Scheme;
IMPORT SchemeUtils;
IMPORT SchemeLongReal;
IMPORT StatObject;
IMPORT SchemeEnvironment;
IMPORT LRVectorLRTbl;
IMPORT Scatter;
IMPORT LRVectorLRPair;
IMPORT NewUOA_M3;
IMPORT Pickle AS Pickle;
IMPORT FileWr;
IMPORT Wr;
IMPORT QuadCheckpoint;
IMPORT FS;
IMPORT MultiEvalResultLRVector;
IMPORT Rd;
IMPORT AL;
IMPORT Powell;

<*FATAL Thread.Alerted*>

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
    fvalues : LRVectorLRTbl.T;
    thisCyc : LRVectorSet.T;
    toEval  : SchemeObject.T;
    recorder : ResultRecorder;
  METHODS
    init(func    : MultiEvalLRVector.T;
         samples : CARDINAL;
         values  : LRVectorMRVTbl.T;
         fvalues : LRVectorLRTbl.T;
         toEval  : SchemeObject.T;
         thisCyc : LRVectorSet.T;
         recorder : ResultRecorder) : MultiEvaluator := InitME;
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
    doPut := FALSE;
  BEGIN

    (* 
       this nominal business is tricky.

       The first eval at a point always includes a nominal eval IF
       we are doing nominal evals at all.
    *)

    LOCK valueMu DO
      isNew   := NOT me.thisCyc.insert(p);
      haveOld := me.values.get(p, oldres);
      IF haveOld THEN
       (* <*ASSERT oldres.extra # NIL*> *)
      END
    END;

    IF doNominal AND NOT haveOld THEN
      Debug.Out("QuadRobust.MEEval : Launching nominal eval at " & FmtP(p));
      thr := Thread.Fork(NEW(NomClosure, me := me.base, at := p))
    END;
    
    res := me.base.multiEval(p, me.samples);
    <*ASSERT res.extra # NIL*>

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
        VAR
          newres := MultiEvalLRVector.Combine(res, oldres);
        BEGIN
          newres.subdirPath := res.subdirPath;
          res := newres
        END;
        <*ASSERT res.extra # NIL*>
      END;
      IF doDebug THEN
        Debug.Out("QuadRobust.MEEval : adding new entry to me.values")
      END;
      doPut := TRUE;
      <*ASSERT res.nominal # Poison*>
    ELSIF haveOld THEN
      (* use the old data if it's got more samples *)
      (* is this really right? *)
      <*ASSERT oldres.nominal # Poison*>

      oldres.extra := res.extra;
      
      IF oldres.n > res.n THEN
        res := oldres;
        <*ASSERT res.extra # NIL*>
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
        Debug.Out(F("QuadRobust.MEEval : n=%s, nominal=%s mean=%s sdev=%s",
                    Int(res.n), FmtP(nominal), FmtP(mean), FmtP(sdev)))
      END;

      <*ASSERT res.extra # NIL*>
      <*ASSERT me.toEval # NIL*>
      WITH retval = SchemeFinish(res, me.toEval) DO
        IF doDebug THEN
          Debug.Out(F("QuadRobust.MEEval : reval=%s",
                      LR(retval)))
        END;

        (* at this point, I think retval contains THE MOST UP-TO-DATE 
           estimate of the function value at the point p *)

        (* so store it *)

        LOCK valueMu DO
          IF doPut THEN
            EVAL me.values.put(p, res)
          END;
          EVAL me.fvalues.put(p, retval)
        END;

        (* and memorize it *)

        LOCK me.recorder.resultsMu DO
          EVAL me.recorder.results.put(LRVectorLRPair.T { LRVector.Copy(p),
                                                       retval } ,
                                    res.subdirPath )
  
        END;
        
        RETURN retval
      END
    END
  END MEEval;

PROCEDURE ResMetric(q : LRVector.T; fvalues : LRVectorLRTbl.T) : LONGREAL =
  (* metric as observed from direct observations *)
  VAR
    f : LONGREAL;
    haveIt := fvalues.get(q, f);
  BEGIN
    <*ASSERT haveIt*>
    RETURN f
  END ResMetric;
  
PROCEDURE InitME(self    : MultiEvaluator;
                 func    : MultiEvalLRVector.T;
                 samples : CARDINAL;
                 values  : LRVectorMRVTbl.T;
                 fvalues : LRVectorLRTbl.T;
                 toEval  : SchemeObject.T;
                 thisCyc : LRVectorSet.T;
                 recorder : ResultRecorder) : MultiEvaluator =
  BEGIN
    self.hintsByForking := TRUE;
    self.base           := func;
    self.samples        := samples;
    self.values         := values;
    self.fvalues        := fvalues;
    self.toEval         := toEval;
    self.thisCyc        := thisCyc;
    self.recorder       := recorder;
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
                        fvalues       : LRVectorLRTbl.T;
                        newScheme     : SchemeMaker;
                        toEval        : SchemeObject.T;
                        lambdaMult    : LONGREAL;
                        newpts(*OUT*) : LRVectorSet.T;
                        VAR newPr     : PointResult.T) : BOOLEAN =
  (* call with valueMu LOCKed *)

  PROCEDURE WalkParr(tag : TEXT) =
    BEGIN
      
      WITH wx = Wx.New() DO
        Wx.PutText(wx, F("==== QuadRobust (%s) leaderboard ====\n", tag));
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
            Wx.PutText(wx, FN("%s metric %s (meas. %s); dist %s; result %s ; rec.p %s\n",
                              TA {tag,
                                  LR(rec.metric),
                                  LR(ResMetric(rec.p, fvalues)),
                                  LR(dist),
                                  MultiEvalLRVector.Format(rec.result),
                                  FmtP(rec.p)}))
          END
        END;
        Debug.Out(Wx.ToText(wx))
      END
    END WalkParr;

  PROCEDURE InitMeasured() =
    VAR
      iter := values.iterate();
      i    := 0;
      q : LRVector.T;
      r : MultiEvalLRVector.Result;
    BEGIN
      WHILE iter.next(q, r) DO
        WITH metric = ResMetric(q, fvalues) DO
          IF i > LAST(parr^) THEN
            Debug.Warning(F("i [%s] > LAST(parr^) [%s]",
                            Int(i), Int(LAST(parr^))))
          END;
          parr[i] := PointMetric.T { metric, q, r };
          INC(i)
        END
      END
    END InitMeasured;

  PROCEDURE InitFitted(me : LRScalarField.T) =
    VAR
      iter := values.iterate();
      i    := 0;
      q : LRVector.T;
      r : MultiEvalLRVector.Result;
    BEGIN
      WHILE iter.next(q, r) DO
        WITH metric = me.eval(q) DO
          parr[i] := PointMetric.T { metric, q, r };
          INC(i)
        END
      END;
    END InitFitted;

  PROCEDURE AttemptMinimize(p : LRVector.T; me : LRScalarField.T) =
    VAR
      pp := LRVector.Copy(p);
    BEGIN
      Debug.Out("QuadRobust.DoLeaderBoard.AttemptMinimize");
      IF NUMBER(p^) >= 3 THEN
        WITH min = NewUOA_M3.Minimize(pp,
                                      me,
                                      n*(n - 1) + 2,
                                      rho,
                                      rho * 0.001d0,
                                      200) DO
          Debug.Out(F("QuadRobust.DoLeaderBoard.AttemptMinimize (NewUOA) min %s @ %s",
                      LR(min), FmtP(pp)));
          EVAL newpts.insert(pp)
        END
      ELSE
        VAR xi  := Matrix.Unit(Matrix.Dim{NUMBER(p^), NUMBER(p^)});
            min := Powell.Minimize(pp, xi, rho * 0.001d0, me);
        BEGIN
          Debug.Out(F("QuadRobust.DoLeaderBoard.AttemptMinimize (Powell) min %s @ %s",
                      LR(min), FmtP(pp)));
          EVAL newpts.insert(pp)
        END
      END
    END AttemptMinimize;
    
  VAR
    parr := NEW(REF ARRAY OF PointMetric.T, values.size());
    n := NUMBER(pr.p^);
  BEGIN
    (* populate the array *)

    Debug.Out("**** values.size() = " & Int(values.size()));

    InitMeasured();

    PointMetricArraySort.Sort(parr^);

    WalkParr("measured");

    Debug.Out(F("DoLeaderBoard : parr[0].p = %s", FmtP(parr[0].p)));

    WITH dofs = Qdofs(n) DO
      IF NUMBER(parr^) >= dofs + StatFits.LeaveOut THEN
        Debug.Out(F("DoLeaderBoard: enough points (%s >= %s + %s) to attempt a surface fit.",
                    Int(NUMBER(parr^)),
                    Int(dofs),
                    Int(StatFits.LeaveOut)));
        TRY

                    
          VAR
            pmm := AttemptSurfaceFit(pr, parr^, values, lambdaMult, newpts);
            me  := NEW(ModelEvaluator,
                       models    := pmm,
                       newScheme := newScheme,
                       toEval    := toEval);
            pm  := parr[0];
          BEGIN

           
            (* walk through values one more time, this time rank 
               against fitted function *)

            InitFitted(me);

            PointMetricArraySort.Sort(parr^);

            WalkParr("fitted");

            AttemptMinimize(pr.p, me);
            
            newPr := PointResult.T { pm.p, pm.metric, FALSE, rho };
            RETURN TRUE (* new point chosen *)
          END
        EXCEPT
          Matrix.Singular =>
          Debug.Warning("AttemptSurfaceFit raised Matrix.Singular!")
        END
      ELSE
        Debug.Out(F("DoLeaderBoard: NOT enough points (%s < %s + %s) to attempt a surface fit.", Int(NUMBER(parr^)), Int(dofs),
                    Int(StatFits.LeaveOut)));
      END;
      Debug.Out(F("DoLeaderBoard: returning old point."));
      
      RETURN FALSE (* failed to choose new point *)
    END
  END DoLeaderBoard;

TYPE
  (* a powerful model evaluator based on fitted models *)
  
  ModelEvaluator = LRScalarField.Default OBJECT
    models    : REF ARRAY OF StatFits.T;
    newScheme : SchemeMaker;
    toEval    : SchemeObject.T;
  OVERRIDES
    eval := ModelEval;
  END;

PROCEDURE ModelEval(me : ModelEvaluator; p : LRVector.T) : LONGREAL =
  VAR
    scm := me.newScheme(p);
    pso := NEW(StatObject.DefaultPoint).init();
  BEGIN
    (* compare to SchemeFinish code *)
    
    FOR i := FIRST(me.models^) TO LAST(me.models^) DO
      WITH nom = ComputeQ(p, me.models[i].bnom  ),
           mu  = ComputeQ(p, me.models[i].bmu   ),
           sig = ComputeQ(p, me.models[i].bsigma),
           v   = modelvars.get(i) DO

        Debug.Out(F("ModelEval : defining %s(%s) : nom=%s mu=%s sig=%s",
                    SchemeSymbol.ToText(v.nm),
                    FmtP(p),
                    LR(nom), LR(mu), LR(sig)));
        
        pso.define(v.nm, nom, mu, sig)
      END
    END;
    WITH e = SchemeUtils.List3(T2S("eval-in-env"),
                               L2S(0.0d0),
                               SchemeUtils.List2(T2S("quote"), me.toEval)),
         ee = NARROW(scm.getGlobalEnvironment(), SchemeEnvironment.T) DO

      TRY
      scm.bind(T2S("*the-stat-object*"), pso);
      <*ASSERT ee.lookup(T2S("*the-stat-object*")) # NIL *>

      WITH blah = scm.evalInGlobalEnv(T2S("*the-stat-object*")) DO
        Debug.Out("ModelEval : blah = " & SchemeUtils.Stringify(blah));
      END;
      
      Debug.Out("ModelEval : toEval = " & SchemeUtils.Stringify(e));
      
        WITH res = scm.evalInGlobalEnv(e) DO
          Debug.Out("ModelEval : final res = " & SchemeUtils.Stringify(res));
          RETURN SchemeLongReal.FromO(res)
        END
      EXCEPT
        Scheme.E(x) =>
        Debug.Error(F("EXCEPTION! ModelEval : Scheme.E \"%s\" when evaluating toEval", x));
        <*ASSERT FALSE*>
      END
    END
  END ModelEval;

PROCEDURE AttemptSurfaceFit(pr              : PointResult.T;
                            READONLY parr   : ARRAY OF PointMetric.T;
                            values          : LRVectorMRVTbl.T;
                            lambdaMult      : LONGREAL;
                            newpts(*OUT*)   : LRVectorSet.T) : REF ARRAY OF StatFits.T
  RAISES { Matrix.Singular } =
  (* attempt a surface fit *)

  PROCEDURE PickClosestPoints() : PointMetricSeq.T =
    VAR
      success := FALSE;
      tryrho  := rho;
      seq   : PointMetricSeq.T;
    BEGIN
      LOOP
        Debug.Out(F("AttemptSurfaceFit : tryrho=%s ; starting from %s",
                    LR(tryrho), PointResult.Format(pr)));
        
        seq := NEW(PointMetricSeq.T).init();
        FOR i := FIRST(parr) TO LAST(parr) DO
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
      Debug.Out(F("AttemptSurfaceFit: seq.size()=%s rho=%s tryrho=%s success=%s p=%s",
                Int(seq.size()), LR(rho), LR(tryrho), Bool(success),
                PointResult.Format(pr)));
      RETURN seq
    END PickClosestPoints;

  PROCEDURE GetResult(p : LRVector.T) : MultiEvalLRVector.Result =
    VAR
      v : MultiEvalLRVector.Result;
      hadIt := values.get(p, v);
    BEGIN
      <*ASSERT hadIt*>
      RETURN v
    END GetResult;

  VAR
    n       := NUMBER(pr.p^);

    qdofs   := Qdofs(n);
    ldofs   := Ldofs(n);
    
    seq   : PointMetricSeq.T;
    bestQ : PointResult.T;

    points := SortByDistance(pr.p, parr);
    v : MultiEvalLRVector.Result;

    nv := modelvars.size();

    models := NEW(REF ARRAY OF StatFits.T, nv);
    pvarr  := NEW(REF ARRAY OF REF ARRAY OF PointMetricLR.T, nv);
    
  BEGIN
    (* first, pick enough points for the fit *)
    seq := PickClosestPoints();

    FOR i := FIRST(points^) TO LAST(points^) DO
      EVAL GetResult(points[i].p)
    END;

    FOR j := 0 TO nv - 1 DO
      pvarr[j] := NEW(REF ARRAY OF PointMetricLR.T, NUMBER(parr))
    END;
        
    FOR i := FIRST(parr) TO LAST(parr) DO
      WITH vv = Scatter.PointMetric(parr[i]) DO
        FOR j := 0 TO nv - 1 DO
          pvarr[j, i] := vv[j]
        END
      END
    END;
    
    (*
      We need to model each variable.  We will model them separately.

      For each computed variable, we need to model 
      -- nominal
      -- mu (mean shift)
      -- sigma
    *)
    
    FOR i := 0 TO modelvars.size() - 1 DO
      WITH mv  = modelvars.get(i),
           fit = StatFits.Attempt(pr.p,
                                  pvarr[i],
                                  selectByAll,
                                  orders := mv.orders,
                                  nomRho := rho,
                                  lambdaMult := lambdaMult
          ) DO
        Debug.Out(F("AttemptSurfaceFit : fitting response \"%s\"",
                    SchemeSymbol.ToText(mv.nm)));
        Debug.Out(F("AttemptSurfaceFit : fit result " & StatFits.Format(fit)));

        models[i] := fit
      END
    END;

    (* we have built the models for each variable 

       Now we need to optimize the target function over the computed
       models.
    *)

    (* we have optimized the modeled target function.

       we add the optimum point as well as a selection of closest
       actual points that we have already computed.
    *)

    RETURN models
  END AttemptSurfaceFit;

PROCEDURE Do1Var(vi              : CARDINAL; (* index of variable *)
                 ctrl            : ARRAY StatComponent.T OF ResponseModel.Order;
                 READONLY points : ARRAY OF PointMetric.T) =
  BEGIN
    (* model a single variable *)

    (* we pick a number of points that we use to model the variable *)

       
  END Do1Var;
  
PROCEDURE InsertClosestPoints(n             : CARDINAL;
                              bestQ         : LRVector.T;
                              READONLY parr : ARRAY OF PointMetric.T;
                              newpts        : LRVectorSet.T) =
  VAR
    darr := SortByDistance(bestQ, parr);
  BEGIN
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

PROCEDURE SortByDistance(to            : LRVector.T;
                         READONLY parr : ARRAY OF PointMetric.T)
  : REF ARRAY OF PointMetric.T =

  (* return copy of parr, where the metric has been replaced by
     the distance to "to" and sorted accordingly *)
  
  VAR
    darr := NEW(REF ARRAY OF PointMetric.T, NUMBER(parr));
  BEGIN
    FOR i := FIRST(parr) TO LAST(parr) DO
      WITH rec  = parr[i],
           dist = Vdist(rec.p, to) DO
        darr[i] := PointMetric.T { dist, rec.p, rec.result }
      END
    END;
    
    PointMetricArraySort.Sort(darr^);

    RETURN darr
  END SortByDistance;

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
         modelvars = GetModelVars(),
         nom     = MultiEvalLRVector.Nominal(resS),
         mu      = MultiEvalLRVector.Mean   (resS),
         sigma   = MultiEvalLRVector.Sdev   (resS)
     DO
      FOR i := 0 TO modelvars.size() - 1  DO
        WITH v = modelvars.get(i) DO
          pso.define(v.nm, nom[i], mu[i], sigma[i])
        END
      END;

      <*ASSERT resS.extra # NIL*>
      LOCK schemeMu DO
        WITH scm = NARROW(resS.extra,Scheme.T),
             e = SchemeUtils.List3(T2S("eval-in-env"),
                                   L2S(0.0d0),
                                   SchemeUtils.List2(T2S("quote"), toEval)),
             ee = NARROW(scm.getGlobalEnvironment(), SchemeEnvironment.T) DO
          TRY
          scm.bind(T2S("*the-stat-object*"), pso);
          <*ASSERT ee.lookup(T2S("*the-stat-object*")) # NIL *>

          WITH blah = scm.evalInGlobalEnv(T2S("*the-stat-object*")) DO
            Debug.Out("SchemeFinish : blah = " & SchemeUtils.Stringify(blah));
          END;
          
          Debug.Out("SchemeFinish : toEval = " & SchemeUtils.Stringify(e));

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

PROCEDURE OrthoRandom(rand : Random.T; VAR a : ARRAY OF LRVector.T) =
  VAR
    n        := NUMBER(a);
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      a[i] := RandomVector.GetDirV(rand, n, 1.0d0)
    END;
    (* orthogonalize the first n vectors with Gram-Schmidt *)
    Orthogonalize(a)
  END OrthoRandom;

PROCEDURE PickRandomPoints(s            : LRVectorSet.T;
                           p            : LRVector.T;
                           rho          : LONGREAL;
                           subdivide    : [1..LAST(CARDINAL)]) =
  TYPE
    LR = LONGREAL;
  VAR
    dp       := NEW(REF ARRAY OF LRVector.T, NUMBER(p^));
    pp       := NEW(LRVector.T, NUMBER(p^));
    rand     := NEW(Random.Default).init();

  BEGIN
    OrthoRandom(rand, dp^);

    FOR i := 1 TO subdivide DO
      WITH scale = FLOAT(i, LR) / FLOAT(subdivide, LR) * rho DO
        FOR j := FIRST(dp^) TO LAST(dp^) DO
          M.LinearCombinationV(1.0d0, p^, scale, dp[j]^, pp^);
          EVAL s.insert(pp)
        END
      END
    END;

    (* add a random point along dim 0 *)
    WITH r = rand.longreal(0.0d0, 1.0d0) DO
      M.LinearCombinationV(1.0d0, p^, r, dp[0]^, pp^);
      EVAL s.insert(pp)
    END
    
  END PickRandomPoints;
  
PROCEDURE Minimize(pa             : LRVector.T;
                   func           : MultiEvalLRVector.T;
                   toEval         : SchemeObject.T;
                   rhobeg, rhoend : LONGREAL;
                   lambdaMult     : LONGREAL;
                   newScheme      : SchemeMaker;
                   recorder       : ResultRecorder;
                   checkRd        : Rd.T;
                   progressWriter : ResultWriter) : Output =

  PROCEDURE LineMinimizationsDone() : BOOLEAN =
    BEGIN
      FOR i := FIRST(lm^) TO LAST(lm^) DO
        IF NOT lm[i].isDone() THEN RETURN FALSE END
      END;
      RETURN TRUE
    END LineMinimizationsDone;

  PROCEDURE LaunchLineMinimizations() =
    VAR
      da       := NEW(REF ARRAY OF LRVector.T, nv);
      pp       := NEW(REF ARRAY OF LRVector.T, nv);
      rand     := NEW(Random.Default).init();

    BEGIN
      OrthoRandom(rand, da^);
      
      FOR i := FIRST(da^) TO LAST(da^) DO
        
        pp[i] := LRVector.Copy(pr.p);
        WITH ff = NEW(MultiEvaluator).init(func,
                                           samples,
                                           values,
                                           fvalues,
                                           toEval,
                                           thisCyc,
                                           recorder) DO
          lm[i].start(pp[i],
                      LRVector.Copy(da[i]),
                      ff,
                      rho)
        END
      END
    END LaunchLineMinimizations;

    <*UNUSED*>
  PROCEDURE AwaitLineMinimizationsDone() =
    BEGIN
      Debug.Out("QuadRobust : Waiting for line searches to evaluate ...");
      FOR i := FIRST(lm^) TO LAST(lm^) DO
        lps[i] := lm[i].wait()
      END;
      Debug.Out("All tasks are done");
      (* all tasks are done *)
      <*ASSERT LineMinimizer.Running() = 0*>
    END AwaitLineMinimizationsDone;
    
  PROCEDURE LaunchPointEvals() =
    BEGIN
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
                                             fvalues,
                                             toEval,
                                             thisCyc,
                                             recorder) DO
            pe.start(q, ff);
            INC(i)
          END
        END
      END(*RAV*);

    END LaunchPointEvals;

  PROCEDURE AwaitPointEvals() =
    BEGIN
      Debug.Out("QuadRobust : Waiting for points to evaluate ... ");
      FOR i := 0 TO newPts.size() - 1 DO
        EVAL peSeq.get(i).wait()
      END;
    END AwaitPointEvals;
    
  VAR
    n        := NUMBER(pa^);

    (* line minimizations *)
    nv       := n;
    lps      := NEW(REF ARRAY OF LineProblem.T, nv);

    
    mins     := NEW(PointResultSeq.T).init();
    allMins  := NEW(PointResultSeq.T).init();
    lm       := NEW(REF ARRAY OF LineMinimizer.T, nv);

    message  : TEXT;
    thisCyc  : LRVectorSet.T;

    values   := NEW(LRVectorMRVTbl.Default).init();
    fvalues  := NEW(LRVectorLRTbl.Default).init();
    
    peSeq    := NEW(PointEvaluatorSeq.T).init();

    newPts   := NEW(LRVectorSetDef.T).init(); (* interesting points to eval *)

    samples : CARDINAL;

    pr       := PointResult.T { pa, LAST(LONGREAL), FALSE, LAST(LONGREAL) };

    startIter := 0;

    checkPoint : QuadCheckpoint.T := NIL;
    
  BEGIN
    rho   := rhobeg;
    
    FOR i := 0 TO nv - 1 DO
      lm[i] := NEW(LineMinimizer.T).init()
    END;
    
    (* setup complete *)

    TRY
    IF checkRd # NIL THEN
      checkPoint := Pickle.Read(checkRd);
      values     := checkPoint.values;
      fvalues    := checkPoint.fvalues;
      startIter  := checkPoint.iter + 1;

      Debug.Out("QuadRobust : restarting from checkpoint : startIter = " &
        Int(startIter))
    END
    EXCEPT
      Pickle.Error(txt) => Debug.Error("?Pickle.Error : " & txt)
    |
      Rd.Failure(x) => Debug.Error("I/O error reading Pickle : Rd.Failure : " &
        AL.Format(x))
    |
      Rd.EndOfFile => Debug.Error("I/O error reading Pickle : short read")
    END;
    
    FOR iter := startIter TO 100 * n - 1 DO

      (*******************  MAIN MINIMIZATION ITERATION  *******************)

      (* track evaluations on this iter *)
      LOCK valueMu DO
        thisCyc := NEW(LRVectorSetDef.T).init()
      END;

      (* sampling schedule *)
      IF iter = 0 THEN
        samples := 5
      ELSIF iter < 4 THEN
        samples := 10
      ELSIF iter < 8 THEN
        samples := 20
      ELSE
        samples := 40
      END;

      Debug.Out(F("QuadRobust.Minimize : rho=%s [rhoend=%s]",
                  LR(rho),
                  LR(rhoend)));

      (* add some random points in the rho-ball *)
      PickRandomPoints(newPts, pr.p, rho, 2);

      Debug.Out(F("QuadRobust.Minimize : iter %s : newPts.size()=%s",
                  Int(iter), Int(newPts.size())));
      Debug.Out(F("QuadRobust.Minimize : iter %s; database has %s points",
                  Int(iter), Int(values.size())));

      LaunchPointEvals();
            
      IF LineMinimizationsDone() THEN
        Debug.Out("QuadRobust.Minimize : line minimizations done, launching anew");
        LaunchLineMinimizations()
      END;
      
      
      IF doDebug THEN
        Debug.Out("QuadRobust.m3 : line minimizers running = " & Int(LineMinimizer.Running())) 
      END;

      AwaitPointEvals();

      newPts := NEW(LRVectorSetDef.T).init(); (* empty the set *)

      (* at this point we have the minima in all directions 
         in two orthonormal bases 0..n-1, and n..2*n-1 *)

      VAR
        gotNew      : BOOLEAN;
        bestq, newp : PointResult.T;
      BEGIN
        Debug.Out("About to call DoLeaderBoard.  values.size() = " & Int(values.size()));
        LOCK valueMu DO
          gotNew := DoLeaderBoard(pr, values, fvalues,
                                  newScheme, toEval,
                                  lambdaMult,
                                  newPts, bestq);
          IF gotNew THEN
            (* we should really have a better update method, right? *)
            newp := bestq
          END
        END;

        (* can we loop here, with gotNew always false? *)

        IF gotNew THEN
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
         clear cache so we don't get fooled by noise 
         Note that we expect the function evaluation to use memoization,
         so this defeats memoization (that's the point!)
      *)
      TYPECASE func.base OF
        LRVectorFieldPll.T(pll) => pll.clearTbls()
      ELSE
        (* we don't know how it works so we don't know how to clear it *)
      END;

      LOCK valueMu DO
      (* clear out the scheme interpreter links *)
        VAR
          newvalues := NEW(LRVectorMRVTbl.Default).init();
          iter := values.iterate();
          v : LRVector.T;
          m : MultiEvalResultLRVector.T;
        BEGIN
          WHILE iter.next(v, m) DO
            m.extra := NIL;
            EVAL newvalues.put(v, m)
          END;
          values := newvalues
        END;
      
        WITH checkpoint = NEW(QuadCheckpoint.T,
                              iter    := iter,
                              values  := values,
                              fvalues := fvalues),
             pi         = Pad(Int(iter), 6, padChar := '0'),
             fn         = F("quadcheckpoint.%s.chk", pi),
             tfn        = fn & "_temp",
             wr         = FileWr.Open(tfn) DO
          Pickle.Write(wr, checkpoint);
          Wr.Close(wr);
          FS.Rename(tfn, fn)
        END
      END;
      
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
                  models  : ARRAY StatComponent.T OF ResponseModel.Order) =
  BEGIN
    modelvars.addhi(ModelVar.T { varname, models })
  END DoModel;

VAR modelvars : ModelVarSeq.T;
  
PROCEDURE OptInit() =
  BEGIN
    modelvars := NEW(ModelVarSeq.T).init();
  END OptInit;

PROCEDURE GetModelVars() : ModelVarSeq.T =
  BEGIN RETURN modelvars END GetModelVars;
  
BEGIN
  schemeMu := NEW(MUTEX);
  OptInit()
END QuadRobust.
