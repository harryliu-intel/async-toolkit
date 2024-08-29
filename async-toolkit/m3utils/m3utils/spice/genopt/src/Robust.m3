MODULE Robust;
FROM NewUOAs IMPORT Output;
IMPORT LRVector, LRScalarField;
IMPORT RandomVector;
IMPORT Random;
IMPORT LRMatrix2;
IMPORT Math;
IMPORT Compress;
IMPORT Debug;
FROM Fmt IMPORT LongReal, F, Int;
IMPORT LineProblem;
IMPORT LineProblemSeq;
IMPORT LineProblemArraySort;
IMPORT LRScalarFieldPll;
IMPORT LongRealSeq AS LRSeq;
IMPORT Thread;
FROM GenOpt IMPORT rho, iter;

CONST LR = LongReal;

PROCEDURE GetFHist(seq : LineProblemSeq.T) : LRSeq.T =
  VAR
    res := NEW(LRSeq.T).init();
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      res.addhi(seq.get(i).minval)
    END;
    RETURN res
  END GetFHist;
  
PROCEDURE FindBest(seq : LineProblemSeq.T;
                   VAR bestval : LONGREAL;
                   VAR bestp   : LRVector.T) =
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

VAR
  mu := NEW(MUTEX);
  running := 0;
  
TYPE
  Closure = Thread.Closure OBJECT
    c       : Thread.Condition;
    
    (* semaphore *)
    done : BOOLEAN;
    
    (* input vars *)
    pp   : LRVector.T;
    dir  : LRVector.T;
    func : LRScalarField.T;
    rho  : LONGREAL;

    (* output var *)
    lps  : LineProblem.T;
  OVERRIDES
    apply := LinMinApply;
  END;

PROCEDURE LinMinApply(cl : Closure) : REFANY =
  BEGIN
    LOOP
      LOCK mu DO
        WHILE cl.done DO
          Thread.Wait(mu, cl.c)
        END
      END;

      IF FALSE THEN Debug.Out("Robust.m3 : LinMinApply : done FALSE.") END;
      
      (* NOT cl.done *)
      LOCK mu DO INC(running) END;
      
      WITH minval = Compress.LinMin(cl.pp,
                                    LRVector.Copy(cl.dir),
                                    cl.func,
                                    cl.rho,
                                    cl.rho / 10.0d0) DO
        Debug.Out("Robust.m3 : Line minimization returned " & LR(minval));
        LOCK mu DO
          cl.lps := LineProblem.T { cl.dir, cl.pp, minval };
          cl.done := TRUE;
          DEC(running);
          Thread.Signal(cl.c)
        END
      END
    END;
    <*ASSERT FALSE*>
  END LinMinApply;
  
PROCEDURE Minimize(p              : LRVector.T;
                   func           : LRScalarField.T;
                   rhobeg, rhoend : LONGREAL;
                   extraDirs      : CARDINAL;
                   ftarget        := FIRST(LONGREAL)) : Output =
  CONST
    Multithread = TRUE;
  VAR
    n     := NUMBER(p^);
    nv    := 2 * n;
    da    := NEW(REF ARRAY OF LRVector.T, nv);
    pp    := NEW(REF ARRAY OF LRVector.T, nv);
    lps   := NEW(REF ARRAY OF LineProblem.T, nv);
    rand  := NEW(Random.Default).init();
    mins  := NEW(LineProblemSeq.T).init();
    cl    := NEW(REF ARRAY OF Closure, nv);
    
    message : TEXT;
    
  BEGIN
    rho   := rhobeg;
    iter  := 0;
    
    IF Multithread THEN
      FOR i := 0 TO 2 * n - 1 DO
        cl[i] := NEW(Closure,
                     c    := NEW(Thread.Condition),
                     done := TRUE);
        EVAL Thread.Fork(cl[i])
      END
    END;
    
    (* allocate some random vectors *)
    FOR i := FIRST(da^) TO LAST(da^) DO
      da[i] := RandomVector.GetDirV(rand, n, 1.0d0)
    END;

    (* orthogonalize the first n vectors with Gram-Schmidt *)

    Orthogonalize(SUBARRAY(da^, 0, n));  (* first ortho. block *)
    Orthogonalize(SUBARRAY(da^, n, n));  (* second ortho. block *)
    
    FOR pass := 0 TO 100 * n - 1 DO
      Debug.Out(F("Robust.m3 : pass %s", Int(pass)));

      IF Multithread THEN
        (* all cl.done TRUE *)
        LOCK mu DO <*ASSERT running = 0*> END;
        
        FOR i := FIRST(da^) TO LAST(da^) DO
          pp[i]  := LRVector.Copy(p);
          LOCK mu DO
            cl[i].pp := pp[i];
            cl[i].dir := LRVector.Copy(da[i]);
            cl[i].func := func;
            cl[i].rho := rho;
            cl[i].done := FALSE;
            Thread.Signal(cl[i].c)
          END
        END;

        IF FALSE THEN
          LOCK mu DO Debug.Out("Robust.m3 : running = " & Int(running)) END;
        END;
        
        FOR i := FIRST(da^) TO LAST(da^) DO
          LOCK mu DO
            WHILE NOT cl[i].done DO
              Thread.Wait(mu, cl[i].c);
            END;
            lps[i] := cl[i].lps
          END
        END;
        (* all cl.done TRUE *)
        LOCK mu DO <*ASSERT running = 0*> END
      ELSE
        FOR i := FIRST(da^) TO LAST(da^) DO
          (* minimize in direction of da[i], from p *)
          (* this is the part that can be done in parallel *)
          pp[i]  := LRVector.Copy(p);
          VAR
            dir := LRVector.Copy(da[i]);
            minval : LONGREAL;
          BEGIN
            minval := Compress.LinMin(pp[i], dir, func, rho, rho/10.0d0);
            Debug.Out("Robust.m3 : Line minimization returned " & LR(minval));
            lps[i] := LineProblem.T { da[i], pp[i], minval }
          END
        END
      END;
      (* at this point we have the minima in all directions 
         in two orthonormal bases 0..n-1, and n..2*n-1 *)

      LineProblemArraySort.Sort(lps^);

      (* next point should be the best of the line minimizations *)
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

        (* SET the two anchor vectors *)
        LRMatrix2.SubV(opt0^, newp, da[0]^);
        LRMatrix2.SubV(opt1^, newp, da[n]^);

        p^ := newp;
        mins.addhi(lps[0]);

        WITH Lookback = 3 DO
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
      
      Orthogonalize(SUBARRAY(da^, 0, n));  (* first ortho. block *)
      Orthogonalize(SUBARRAY(da^, n, n));  (* second ortho. block *)

      (* clear cache so we don't get fooled by noise *)
      TYPECASE func OF
        LRScalarFieldPll.T(pll) =>
        pll.clearTbls()
      ELSE
      END;

      INC(iter);
      message := "stopping because of out of iterations"
      
    END      ;

    VAR
      bestval : LONGREAL;
      bestv   : LRVector.T;
    BEGIN
      FindBest(mins, bestval, bestv);

      Debug.Out("Robust.m3 : " & message);
      
      RETURN Output { iterations := iter,
                      funcCount  := 0,
                      fhist      := GetFHist(mins),
                      message    := message,
                      f          := bestval,
                      x          := bestv }
    END
  END Minimize;

PROCEDURE Predict(s : LRVector.T;
                  READONLY d : ARRAY OF LRVector.T) : LRVector.T =
  VAR
    n := NUMBER(s^);
    diff, sum := NEW(LRVector.T, n);

  BEGIN
    LRMatrix2.ZeroV(sum^);
    FOR i := FIRST(d) TO LAST(d) DO
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
