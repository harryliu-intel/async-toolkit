MODULE PowerUtils EXPORTS Power;
FROM SvsTypes IMPORT CornerData;
IMPORT Random;
IMPORT NormalDeviate;
FROM PowerScaling IMPORT Interpolate;
IMPORT Fmt;
FROM Fmt IMPORT F;
IMPORT Debug;

CONST LR = Fmt.LongReal;

PROCEDURE MakeDie(rand : Random.T;
                  READONLY p : Params;
                  trunc : LONGREAL) : CornerData =
  (* returns the PVT voltage of a single die *)
  BEGIN
    LOOP
      VAR
        x := NormalDeviate.Get(rand, 0.0d0, 1.0d0);
      BEGIN
        IF ABS(x) <= trunc THEN
          RETURN Interpolate(p, x)
        END
      END
    END
  END MakeDie;

PROCEDURE DoDebugCorner(READONLY p : Params) =
  VAR
    dbgCorner := Interpolate(p, 0.0d0);
    q : Result;
  BEGIN
    dbgCorner.vpower := 0.750d0;
    
    q := Calc(p, dbgCorner);
    Debug.Out(F("DEBUG CORNER Vdd=%s leakPwr=%s totPwr=%s",
                LR(dbgCorner.vpower), LR(q.leakPwr), LR(q.totPwr)))
    
  END DoDebugCorner;

BEGIN END PowerUtils.
