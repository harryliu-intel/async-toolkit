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
IMPORT RandomVector;
IMPORT Random;
IMPORT LRMatrix2;
IMPORT Math;
IMPORT Debug;
FROM Fmt IMPORT LongReal, F, Int;
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

CONST LR = LongReal;

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
        EVAL me.values.put(p, res)
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
      Debug.Out(F("StocRobust.MEEval : mean=%s sigmaK=%s sdev=%s -> retval=%s",
                  LR(mean), LR(me.sigmaK), LR(sdev), LR(retval)));
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
    RETURN Math.sqrt(LRMatrix2.SumDiffSqV(a^, b^))
  END Vdist;
  
PROCEDURE Minimize(p              : LRVector.T;
                   func           : MultiEval.T;
                   rhobeg, rhoend : LONGREAL;
                   sigmaK         : LONGREAL) : Output =
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

      thisCyc := NEW(LRVectorSetDef.T).init();
      
      Debug.Out(F("Robust.Minimize : pass %s; database has %s points",
                  Int(pass), Int(values.size())));
      Debug.Out(F("Robust.Minimize : rho=%s [rhoend=%s]", LR(rho), LR(rhoend)));

      <*ASSERT LineMinimizer.Running() = 0*>
        
      FOR i := FIRST(da^) TO LAST(da^) DO
        
        pp[i] := LRVector.Copy(p);
        cl[i].start(pp[i],
                    LRVector.Copy(da[i]),
                    
                    MultiEvaluate(func, 10, sigmaK, values, thisCyc),
                    
                    (* how the HECK do we get the samples to Main? *)
                    (* I think we need to expose extra fields in 
                       PllEvaluator in Main and share it with this
                       module, then we can pass samples that way ... *)
                    
                    rho)
      END;
      
      IF FALSE THEN
        Debug.Out("Robust.m3 : running = " & Int(LineMinimizer.Running())) 
      END;
      
      FOR i := FIRST(da^) TO LAST(da^) DO
        lps[i] := cl[i].wait()
      END;
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
        parr := NEW(REF ARRAY OF PointMetric.T, values.size());
        iter := values.iterate();
        q : LRVector.T;
        r : MultiEval.Result;
        i := 0;
      BEGIN
        (* populate the array *)

        WHILE iter.next(q, r) DO
          WITH metric = ResMetric(r, sigmaK) DO
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
              Wx.PutText(wx, F("metric %s ; dist %s; result %s ; rec.p %s\n",
                               LR(rec.metric),
                               LR(dist),
                               MultiEval.Format(rec.result),
                               FmtP(rec.p)))
            END
          END;
          Debug.Out(Wx.ToText(wx))
        END

      END;
      
      
      WITH newp = lps[0].minp^,
           opt0 = Predict(p, SUBARRAY(pp^, 0, n)),
           opt1 = Predict(p, SUBARRAY(pp^, n, n))
       DO
        Debug.Out(F("Robust.m3 : opt0 (%s) ; opt1 (%s)",
                    LRMatrix2.FormatV(opt0^),
                    LRMatrix2.FormatV(opt1^)));
        
        Debug.Out(F("Robust.m3 : updating p (%s) -> (%s)",
                    LRMatrix2.FormatV(p^),
                    LRMatrix2.FormatV(newp)));

        WITH dp = LRVector.Copy(p) DO
          LRMatrix2.SubV(newp, p^, dp^);
          rho := LRMatrix2.Norm(dp^);
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
        
        LRMatrix2.SubV(opt0^, newp, da[0]^);
        LRMatrix2.SubV(opt1^, newp, da[n]^);

        p^ := newp;
        mins.addhi(lps[0]);
        allMins.addhi(lps[0]);

        WITH Lookback = 3 DO

          (* 
             if we haven't improved in three straight iterations,
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
      WITH Lookback = 5 DO
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
    LRMatrix2.ZeroV(sum^);
    FOR i := FIRST(d) TO LAST(d) DO
      <*ASSERT d[i] # NIL*>
      <*ASSERT s    # NIL*>
      <*ASSERT diff # NIL*>
      LRMatrix2.SubV(d[i]^, s^, diff^);
      LRMatrix2.AddV(diff^, sum^, sum^)
    END;
    LRMatrix2.AddV(s^, sum^, sum^);
    RETURN sum
  END Predict;

PROCEDURE RemoveComponent(READONLY ik : LRVector.S; VAR v : LRVector.S) =
  BEGIN
    WITH dot = LRMatrix2.Dot(ik, v) DO
      LRMatrix2.LinearCombinationV(-dot, ik, 1.0d0, v, v)
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
      WITH inorm = Math.sqrt(LRMatrix2.Dot(da[i]^, da[i]^)),
           mult  = 1.0d0 / inorm DO
        LRMatrix2.MulSV(mult, da[i]^, da[i]^)
      END
    END;
  END Orthogonalize;

BEGIN END StocRobust.
