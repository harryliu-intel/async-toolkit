(* $Id$ *)

MODULE SchemeInteraction;
IMPORT SchemePrimitive;
IMPORT SchemeProcedure;
FROM Scheme IMPORT E, Object;
IMPORT Scheme;
IMPORT ReadLine;
FROM SchemeReadLine IMPORT ReturningMainLoop;
IMPORT IP, NetObj, Thread, ReadLineError, AL;

PROCEDURE InteractionApply(<*UNUSED*>p : SchemeProcedure.T;
                           interp : Scheme.T;
                           <*UNUSED*>args : Object) : Object RAISES { E } =
  BEGIN
    TRY
      WITH readLine = NEW(ReadLine.Default).init() DO
        RETURN ReturningMainLoop(readLine, interp)
      END
    EXCEPT
      IP.Error(e) => RAISE E ("IP.Error: " & AL.Format(e))
    |
      NetObj.Error => RAISE E ("NetObj.Error")
    |
      Thread.Alerted => RAISE E ("Thread.Alerted")
    |
      ReadLineError.E(x) => RAISE E ("ReadLineError.E: " & AL.Format(x))
    END
  END InteractionApply;

PROCEDURE Extend(prims : SchemePrimitive.ExtDefiner)  : SchemePrimitive.ExtDefiner =
  BEGIN
    prims.addPrim("run-interaction", NEW(SchemeProcedure.T, 
                                         apply := InteractionApply),
                  0, 0);
    RETURN prims
  END Extend;

BEGIN END SchemeInteraction.
