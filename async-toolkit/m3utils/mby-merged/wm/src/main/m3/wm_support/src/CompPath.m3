MODULE CompPath;
FROM Fmt IMPORT F, Int;
IMPORT Debug AS M3Debug;
IMPORT CompAddr, CompRange;
IMPORT Env;

VAR silent := Env.Get("WM_SILENT") # NIL;
    
PROCEDURE Cat(a, b : T) : T =
  BEGIN
    IF silent THEN RETURN NIL END;
    IF a = NIL OR b = NIL THEN
      (* this case is the probing an array case *)
      RETURN NIL
    END;
    RETURN a & b
  END Cat;

PROCEDURE CatArray(a, b : T; i : CARDINAL) : T =
  BEGIN
    IF silent THEN RETURN NIL END;
    IF a = NIL OR b = NIL THEN
      (* this case is the probing an array case *)
      RETURN NIL
    END;
    RETURN Cat(a,b) & F("[%s]",Int(i))
  END CatArray;

PROCEDURE Debug(reg : T; at : CompRange.T) =
  VAR
  BEGIN
    IF reg # NIL THEN
      M3Debug.Out(F("%s @ 16_%s = %s", reg, Int(
                                          CompAddr.DeltaBytes(at.pos,
                                                              CompAddr.Zero),
                                          base := 16),
                  CompRange.Format(at)))
                  
    END
  END Debug;
  
BEGIN END CompPath.
