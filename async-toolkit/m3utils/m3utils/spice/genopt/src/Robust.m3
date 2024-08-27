MODULE Robust;
FROM NewUOAs IMPORT Output;
IMPORT LRVector, LRScalarField;
IMPORT RandomVector;
IMPORT Random;
IMPORT LRMatrix2;
IMPORT Math;
IMPORT Compress;
IMPORT Debug;
FROM Fmt IMPORT LongReal;

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
    rand := NEW(Random.Default).init();
  BEGIN
    (* allocate some random vectors *)
    FOR i := FIRST(da^) TO LAST(da^) DO
      da[i] := RandomVector.GetDirV(rand, n, 1.0d0)
    END;

    (* orthogonalize the first n vectors with Gram-Schmidt *)

    FOR i := 1 TO n - 1 DO
      FOR j := 0 TO i - 1 DO
        RemoveComponent(da[j]^, da[i]^) (* remove da[j] from da[i] *)
      END;
      WITH inorm = Math.sqrt(LRMatrix2.Dot(da[i]^, da[i]^)),
           mult  = 1.0d0 / inorm DO
        LRMatrix2.MulSV(mult, da[i]^, da[i]^)
      END
    END;

    FOR i := FIRST(da^) TO LAST(da^) DO
      (* minimize in direction of da[i], from p *)
      VAR
        pp := LRVector.Copy(p);
        dir := LRVector.Copy(da[i]);
        minval : LONGREAL;
      BEGIN
        minval := Compress.LinMin(pp, dir, func);
        Debug.Out("Line minimization returned " & LR(minval))
      END
    END
    
  END Minimize;

PROCEDURE RemoveComponent(READONLY ik : LRVector.S; VAR v : LRVector.S) =
  BEGIN
    WITH dot = LRMatrix2.Dot(ik, v) DO
      LRMatrix2.LinearCombinationV(-dot, ik, 1.0d0, v, v)
    END
  END RemoveComponent;

BEGIN END Robust.
