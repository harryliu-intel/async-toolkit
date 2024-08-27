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

CONST LR = LongReal;

PROCEDURE Minimize(p              : LRVector.T;
                   func           : LRScalarField.T;
                   rhobeg, rhoend : LONGREAL;
                   extraDirs      : CARDINAL;
                   ftarget     := FIRST(LONGREAL)) : Output =
  VAR
    n := NUMBER(p^);
    nv := n + extraDirs;
    da := NEW(REF ARRAY OF LRVector.T, nv);
    lps := NEW(REF ARRAY OF LineProblem.T, nv);
    rand := NEW(Random.Default).init();
  BEGIN
    (* allocate some random vectors *)
    FOR i := FIRST(da^) TO LAST(da^) DO
      da[i] := RandomVector.GetDirV(rand, n, 1.0d0)
    END;

    (* orthogonalize the first n vectors with Gram-Schmidt *)

    Orthogonalize(da^);

    FOR qass := 0 TO n - 1 DO
    FOR pass := 0 TO n - 1 DO
      Debug.Out(F("Robust.m3 : pass %s %s", Int(qass), Int(pass)));
      
    FOR i := FIRST(da^) TO LAST(da^) DO
      (* minimize in direction of da[i], from p *)
      VAR
        pp := LRVector.Copy(p);
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
      LRMatrix2.Normalize(p0p1v^);
      
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
  END Minimize;

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
