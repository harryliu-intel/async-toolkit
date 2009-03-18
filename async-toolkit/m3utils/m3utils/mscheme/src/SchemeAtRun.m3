(* $Id$ *)

MODULE SchemeAtRun;
IMPORT SchemePrimitive, SchemeProcedure;
IMPORT Time;
IMPORT Scheme;
FROM Scheme IMPORT E, Object, Environment;
IMPORT Thread;
IMPORT SchemeUtils;
IMPORT SchemeLongReal;
IMPORT SchemePair, SchemeSymbol, SchemeString, SchemeBoolean;
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
      END;
      RETURN res
    END
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
        RAISE E ("SchemeAtRun.AtRunApply: environment type mismatch: " &
              SchemeUtils.Stringify(env))
      END;

      RETURN NEW(Handle, 
                 t := Thread.Fork(NEW(Closure,
                                      time := time,
                                      command := command,
                                      resultLambda := resultLambda,
                                      interp := interp)))
    END
  END AtRunApply;

TYPE Handle = OBJECT t : Thread.T; joined := FALSE; res : REFANY END;

PROCEDURE AtJoinApply(<*UNUSED*>proc : SchemeProcedure.T; 
                      <*UNUSED*>interp : Scheme.T; 
                      args : Object) : Object RAISES { E } =
  BEGIN
    WITH x = SchemeUtils.First(args) DO
      IF x = NIL OR NOT ISTYPE(x,Handle) THEN
        RAISE E("SchemeAtRun.AtWaitApply: not a Handle: " &
              SchemeUtils.Stringify(x))
      END;
      WITH h = NARROW(x, Handle) DO
        IF NOT h.joined THEN h.joined := TRUE; h.res := Thread.Join(h.t) END;
        RETURN h.res
      END
    END
  END AtJoinApply;

(**********************************************************************)

TYPE 
  ClockClosure = Thread.Closure OBJECT
    interval   : LONGREAL;
    command    : Object;
    errorHook  : Object;
    interp     : Scheme.T;
  OVERRIDES
    apply := CCApply;
  END;

PROCEDURE CCApply(cl : ClockClosure) : REFANY =
  VAR
    next := Time.Now() + cl.interval;
  BEGIN
    LOOP
      WHILE Time.Now() < next DO 
        Thread.Pause(MIN(1.0d0, next - Time.Now()))
      END;

      next := Time.Now() + cl.interval;

      TRY
        WITH res = cl.interp.evalInGlobalEnv(NEW(SchemePair.T,
                                                 first := cl.command)) DO
          IF NOT SchemeBoolean.TruthO(res) THEN
            RETURN NIL
          END
        END
      EXCEPT
        E(err) => 
        Debug.Out("SchemeAtRun.CCApply: caught Scheme.E: " & err);
        TRY
          IF cl.errorHook # NIL THEN
            WITH toRun = SchemeUtils.List2(cl.errorHook, 
                                           SchemeString.FromText(err)) DO
              Debug.Out("SchemeAtRun.ClApply: running: " &
                SchemeUtils.Stringify(toRun));
              EVAL cl.interp.evalInGlobalEnv(toRun)
            END
          END
        EXCEPT
          E(e) => (* skip *)
          Debug.Out("SchemeAtRun.ClApply: caught Scheme.E running errorHook: " & e)
        END
      END
    END
  END CCApply;

PROCEDURE ClockRunApply(<*UNUSED*>proc : SchemeProcedure.T; 
                     interp : Scheme.T; 
                     args : Object) : Object RAISES { E } =
  BEGIN
    WITH interval = SchemeLongReal.FromO(SchemeUtils.First(args)),
         command = SchemeUtils.Second(args),
         errorHook = SchemeUtils.Third(args),
         env = interp.getGlobalEnvironment() DO
      IF NOT ISTYPE(env, Environment) THEN
        RAISE E ("SchemeAtRun.AtRunApply: environment type mismatch")
      END;

      EVAL Thread.Fork(NEW(ClockClosure,
                           interval := interval,
                           command := command,
                           errorHook := errorHook,
                           interp := interp));
      RETURN command (* is this right? *)
    END
  END ClockRunApply;

(**********************************************************************)

PROCEDURE Extend(definer : SchemePrimitive.ExtDefiner) : SchemePrimitive.ExtDefiner =
  BEGIN
    definer.addPrim("at-run",
                    NEW(SchemeProcedure.T,
                        apply := AtRunApply),
                    2, 3);
    (* (at-run <time> <cmd> <result-lambda>) returns a joinable *)

    definer.addPrim("at-join",
                    NEW(SchemeProcedure.T,
                        apply := AtJoinApply),
                    1, 1);
    (* (at-join <joinable>) *)
    

    definer.addPrim("clock-run",
                    NEW(SchemeProcedure.T,
                        apply := ClockRunApply),
                    2, 3);
    (* (clock-run <interval> <cmd> <error-hook>) *)

    RETURN definer
  END Extend;

BEGIN END SchemeAtRun.
