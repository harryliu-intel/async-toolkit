(* $Id$ *)

MODULE SchemeAtRun;
IMPORT SchemePrimitive, SchemeProcedure;
IMPORT Time;
IMPORT Scheme;
FROM Scheme IMPORT E, Object, Environment;
IMPORT Thread;
IMPORT SchemeUtils;
IMPORT SchemeLongReal;
IMPORT SchemePair, SchemeSymbol, SchemeString;
IMPORT Debug;

TYPE
  Closure = Thread.Closure OBJECT
    time           : Time.T;
    command        : Object;
    resultLambda   : Object;
    interp         : Scheme.T;
  OVERRIDES
    apply := ClApply;
  END;

PROCEDURE ClApply(cl : Closure) : REFANY =
  BEGIN
    WHILE Time.Now() < cl.time DO
      Thread.Pause(MIN(1.0d0, cl.time - Time.Now()))
    END;
    
    VAR res : Object; BEGIN
      TRY
        res := cl.interp.evalInGlobalEnv(NEW(SchemePair.T,
                                             first := cl.command))
      EXCEPT
        E(err) => 
        Debug.Out("SchemeAtRun.ClApply: caught Scheme.E: " & err);
        res := NEW(SchemePair.T,
                   first:= SchemeSymbol.FromText("**error-result**"),
                   rest := SchemeString.FromText(err))
      END;
      IF cl.resultLambda # NIL THEN
        TRY
          WITH toRun = SchemeUtils.List2(cl.resultLambda, 
                                         SchemeUtils.List2(SchemeSymbol.FromText("quote"),res)) DO
            Debug.Out("SchemeAtRun.ClApply: running: " &
              SchemeUtils.Stringify(toRun));
            EVAL cl.interp.evalInGlobalEnv(toRun)
          END
        EXCEPT
          E(e) => (* skip *)
          Debug.Out("SchemeAtRun.ClApply: caught Scheme.E running resultLambda: " & e)
        END
      END
    END;
    RETURN NIL
  END ClApply;

PROCEDURE AtRunApply(<*UNUSED*>proc : SchemeProcedure.T; 
                     interp : Scheme.T; 
                     args : Object) : Object RAISES { E } =
  BEGIN
    WITH time = SchemeLongReal.FromO(SchemeUtils.First(args)),
         command = SchemeUtils.Second(args),
         resultLambda = SchemeUtils.Third(args),
         env = interp.getGlobalEnvironment() DO
      IF NOT ISTYPE(env, Environment) THEN
        RAISE E ("SchemeAtRun.AtRunApply: environment type mismatch")
      END;

      EVAL Thread.Fork(NEW(Closure,
                           time := time,
                           command := command,
                           resultLambda := resultLambda,
                           interp := interp));
      RETURN resultLambda (* is this right? *)
    END
  END AtRunApply;

PROCEDURE Extend(definer : SchemePrimitive.ExtDefiner) : SchemePrimitive.ExtDefiner =
  BEGIN
    definer.addPrim("at-run",
                    NEW(SchemeProcedure.T,
                        apply := AtRunApply),
                    2, 3);

    RETURN definer
  END Extend;


BEGIN END SchemeAtRun.
