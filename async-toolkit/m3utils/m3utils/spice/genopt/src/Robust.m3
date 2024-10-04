MODULE Robust;

(* Robust parallel minimization.

   We maintain a point p where we search.

   On each iteration, we perform line searches in various directions.

   The best result of the line searches is accepted at the next point.

   So that we can handle noisy functions, we pick the best result even
   if it is worse than our previous point.  (Mathematically, if the
   function evaluations had no error, every line search would give an
   answer no worse than the value at p.)

   The line searches are performed in parallel in 2N directions (currently,
   2N, but could be anything from N and higher.)

   The directions are chosen as follows:
   1. a special primary direction is chosen (see below)
   2. k = 2N random vectors are chosen.
   3. Each contiguous sequence of N vectors is orthogonalized using G-S
   
   The primary direction is what is inspired by Powell.

   On the previous iteration, a point "opt" is implied by all the 
   line minimizations: it is the point that results from adding all
   the line-optimal displacements together.

   When we update p, we set the primary direction as being from the
   new p to opt.  

   If the function is nicely quadratic, the primary direction will then
   point to the optimum, and the next iteration of the line
   search algorithm will find the optimum in one (major) step.

   THIS DOESN'T ACTUALLY WORK!

   Unlike classic Powell, there is no vector-space collapse since the
   search vectors are always randomly generated, except for the first
   direction.  (They are always an orthonormal basis by construction.)

   The random direction choice is performed using the Muller-Marsaglia
   method, so it is uniformly distributed in space (not Cartesian
   constrained).  Since the directions are uniformly distributed in
   space, there is no reason that Gram-Schmidt would introduce 
   Cartesian artefacts.

   The line search algorithm called out to is expected to be Brent's
   method, which is a dimensional lifting of the 
   van Wijngaarden-Dekker-Brent root-finding algorithm.

   Note that the line search algorithm is also slightly parallelized,
   and performs up to four simultaneous evaluations.

   Author : mika.nystroem@intel.com
   August-September, 2024
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
FROM GenOpt IMPORT rho, iter;
IMPORT LineMinimizer;

CONST LR = LongReal;

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
  
PROCEDURE Minimize(p              : LRVector.T;
                   func           : LRScalarField.T;
                   rhobeg, rhoend : LONGREAL) : Output =
  VAR
    n     := NUMBER(p^);
    nv    := 2 * n;
    da    := NEW(REF ARRAY OF LRVector.T, nv);
    pp    := NEW(REF ARRAY OF LRVector.T, nv);
    lps   := NEW(REF ARRAY OF LineProblem.T, nv);
    rand  := NEW(Random.Default).init();
    mins  := NEW(LineProblemSeq.T).init();
    allMins  := NEW(LineProblemSeq.T).init();
    cl    := NEW(REF ARRAY OF LineMinimizer.T, nv);
    
    message : TEXT;
    
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
      
      Debug.Out(F("Robust.m3 : pass %s", Int(pass)));

        <*ASSERT LineMinimizer.Running() = 0*>
        
        FOR i := FIRST(da^) TO LAST(da^) DO

          pp[i] := LRVector.Copy(p);
          cl[i].start(pp[i],
                      LRVector.Copy(da[i]),
                      func,
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
      
      WITH newp = lps[0].minp^,
           opt0 = Predict(p, SUBARRAY(pp^, 0, n)),
           opt1 = Predict(p, SUBARRAY(pp^, n, n))
       DO
        Debug.Out(F("Robust.m3 : opt0 (%s) ; opt1 (%s)",
                    LRMatrix2.FormatV(opt0^),
                    LRMatrix2.FormatV(opt1^)));
        
        Debug.Out(F("Robust.m3 : updating p (%s) -> (%s) [minval=%s]",
                    LRMatrix2.FormatV(p^),
                    LRMatrix2.FormatV(newp),
                    LR(lps[0].minval)));

        WITH dp = LRVector.Copy(p) DO
          LRMatrix2.SubV(newp, p^, dp^);
          WITH norm = LRMatrix2.Norm(dp^) DO
            rho := 0.25d0 * rho + 0.75d0 * norm
          END;
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
      TYPECASE func OF
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

BEGIN END Robust.
