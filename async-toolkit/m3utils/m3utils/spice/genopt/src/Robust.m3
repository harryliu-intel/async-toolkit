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
IMPORT LineProblemArraySort;
IMPORT LRScalarFieldPll;

CONST LR = LongReal;

PROCEDURE OldMinimize(p              : LRVector.T;
                   func           : LRScalarField.T;
                   rhobeg, rhoend : LONGREAL;
                   extraDirs      : CARDINAL;
                   ftarget        := FIRST(LONGREAL)) : Output =
  VAR
    n    := NUMBER(p^);
    nv   := 2*n;
    da   := NEW(REF ARRAY OF LRVector.T, nv);
    lps  := NEW(REF ARRAY OF LineProblem.T, nv);
    rand := NEW(Random.Default).init();
  BEGIN
    (* allocate some random vectors *)
    FOR i := FIRST(da^) TO LAST(da^) DO
      da[i] := RandomVector.GetDirV(rand, n, 1.0d0)
    END;

    (* orthogonalize the first n vectors with Gram-Schmidt *)

    Orthogonalize(SUBARRAY(da^, 0, n));  (* first ortho. block *)
    Orthogonalize(SUBARRAY(da^, n, n));  (* second ortho. block *)

    FOR qass := 0 TO n - 1 DO
    FOR pass := 0 TO n - 1 DO
      Debug.Out(F("Robust.m3 : pass %s %s", Int(qass), Int(pass)));
      
    FOR i := FIRST(da^) TO LAST(da^) DO
      (* minimize in direction of da[i], from p *)
      (* this is the part that can be done in parallel *)
      VAR
        pp  := LRVector.Copy(p);
        dir := LRVector.Copy(da[i]);
        minval : LONGREAL;
      BEGIN
        minval := Compress.LinMin(pp, dir, func);
        Debug.Out("Robust.m3 : Line minimization returned " & LR(minval));
        lps[i] := LineProblem.T { da[i], pp, minval }
      END
    END;

    LineProblemArraySort.Sort(lps^);

    WITH p0p1v = LRVector.Copy(lps[0].minp) DO
      LRMatrix2.SubV(lps[1].minp^, lps[0].minp^, p0p1v^);
      IF LRMatrix2.Norm(p0p1v^) # 0.0d0 THEN
        LRMatrix2.Normalize(p0p1v^)
      ELSE
        p0p1v^ := lps[0].dir^
      END;
      
      Debug.Out(F("Robust.m3 : Best result is        %s p %s",
                  LR(lps[0].minval),
                  LRMatrix2.FormatV(lps[0].minp^)));
      Debug.Out(F("Robust.m3 : Second best result is %s p %s",
                  LR(lps[1].minval),
                  LRMatrix2.FormatV(lps[1].minp^)));

      Debug.Out(F("Robust.m3 : p0p1v = %s", LRMatrix2.FormatV(p0p1v^)));

      p := LRVector.Copy(lps[0].minp);
      
      da[0] := LRVector.Copy(p0p1v);
      FOR i := 1 TO LAST(da^) DO
        da[i] := LRVector.Copy(lps[i].dir);
        (* RandomVector.GetDir(rand, 1.0d0, da[i]^) *)
      END;
      Orthogonalize(da^)
    END
  END;
    (* re-init *)
      FOR i := FIRST(da^) TO LAST(da^) DO
         RandomVector.GetDir(rand, 1.0d0, da[i]^) 
      END;
      Orthogonalize(da^)
    
    END
  END OldMinimize;
  
PROCEDURE Minimize(p              : LRVector.T;
                   func           : LRScalarField.T;
                   rhobeg, rhoend : LONGREAL;
                   extraDirs      : CARDINAL;
                   ftarget        := FIRST(LONGREAL)) : Output =
  VAR
    n    := NUMBER(p^);
    nv   := 2*n;
    da   := NEW(REF ARRAY OF LRVector.T, nv);
    pp   := NEW(REF ARRAY OF LRVector.T, nv);
    lps  := NEW(REF ARRAY OF LineProblem.T, nv);
    rand := NEW(Random.Default).init();
    rho  := rhobeg;
  BEGIN
    (* allocate some random vectors *)
    FOR i := FIRST(da^) TO LAST(da^) DO
      da[i] := RandomVector.GetDirV(rand, n, 1.0d0)
    END;

    (* orthogonalize the first n vectors with Gram-Schmidt *)

    Orthogonalize(SUBARRAY(da^, 0, n));  (* first ortho. block *)
    Orthogonalize(SUBARRAY(da^, n, n));  (* second ortho. block *)

    FOR pass := 0 TO n * n - 1 DO
      Debug.Out(F("Robust.m3 : pass %s", Int(pass)));
      
      FOR i := FIRST(da^) TO LAST(da^) DO
        (* minimize in direction of da[i], from p *)
        (* this is the part that can be done in parallel *)
        pp[i]  := LRVector.Copy(p);
        VAR
          dir := LRVector.Copy(da[i]);
          minval : LONGREAL;
        BEGIN
          minval := Compress.LinMin(pp[i], dir, func, rho);
          Debug.Out("Robust.m3 : Line minimization returned " & LR(minval));
          lps[i] := LineProblem.T { da[i], pp[i], minval }
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
            Debug.Out(F("Robust.m3 : stopping because rho < rhoend"))
          END
        END;


        FOR i := FIRST(da^) TO LAST(da^) DO
          da[i] := RandomVector.GetDirV(rand, n, 1.0d0)
        END;

        (* SET the two anchor vectors *)
        LRMatrix2.SubV(opt0^, newp, da[0]^);
        LRMatrix2.SubV(opt1^, newp, da[n]^);

        p^ := newp
      END;
      
      Orthogonalize(SUBARRAY(da^, 0, n));  (* first ortho. block *)
      Orthogonalize(SUBARRAY(da^, n, n));  (* second ortho. block *)
          
      TYPECASE func OF
        LRScalarFieldPll.T(pll) =>
        pll.clearTbls()
      ELSE
      END
      
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
