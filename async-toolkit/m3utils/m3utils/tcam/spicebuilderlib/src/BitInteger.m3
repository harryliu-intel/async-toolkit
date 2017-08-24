MODULE BitInteger;
IMPORT Word, Debug;
FROM Fmt IMPORT Int, F;
IMPORT Bit;

REVEAL
  SmallPromise = PubSmallPromise BRANDED OBJECT 
    v : INTEGER ;
  OVERRIDES
    force := ForceSmallPromise;
  END;

PROCEDURE Small(z : INTEGER) : T =
  BEGIN RETURN NEW(SmallPromise, v := z) END Small;

PROCEDURE ForceSmallPromise(p    : SmallPromise; 
                            bits : CARDINAL) : Concrete =
  VAR
    res := NEW(Concrete, bits := NEW(REF ARRAY OF Bit.T, bits));
    w : Word.T := p.v; (* unsafe cast *)
    dbgStr := "";
  BEGIN
    FOR i := FIRST(res.bits^) TO LAST(res.bits^) DO
      IF i >= Word.Size THEN
        res.bits[i] := Word.Extract(w, Word.Size-1, 1)
      ELSE
        res.bits[i] := Word.Extract(w,           i, 1)
      END
    END;

    FOR i := LAST(res.bits^) TO FIRST(res.bits^) BY -1 DO
      dbgStr := dbgStr & " " & Int(res.bits[i])
    END;
    Debug.Out(F("Forcing small promise %s -> %s", Int(w), dbgStr));

    RETURN res
  END ForceSmallPromise;


BEGIN END BitInteger.
