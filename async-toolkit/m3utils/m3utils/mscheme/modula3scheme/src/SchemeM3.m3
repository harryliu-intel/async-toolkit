(* $Id$ *)

MODULE SchemeM3;

IMPORT Scheme;
FROM Scheme IMPORT E, Object;

IMPORT SchemeProcedure, SchemePrimitive;
IMPORT SchemeJailBreak, SchemeM3TableOps;
IMPORT Time, Date, RTCollector, Pathname;
IMPORT OSError;
IMPORT SchemeSymbol;
FROM SchemeUtils IMPORT First, Second, Third, Stringify, Error;
IMPORT SchemeString, SchemeLongReal;
FROM SchemeLongReal IMPORT FromO;
FROM SchemeBoolean IMPORT True, Truth;
IMPORT Fmt;
IMPORT TextRefSchemeAutoTbl;
IMPORT Stdio, Wr, Debug, AL, FileWr, Thread;
IMPORT IP, Process;

<* FATAL Thread.Alerted *>

REVEAL
  T = Public BRANDED Brand OBJECT
    jailBreak : SchemeJailBreak.T := NIL;
    m3TableOps : SchemeM3TableOps.T := NIL;
  OVERRIDES
    init := Init;
    setTableOps       :=  SetTableOps;
  END;

PROCEDURE SetTableOps(t : T; to : SchemeM3TableOps.T) =
  BEGIN t.m3TableOps := to END SetTableOps;

PROCEDURE Init(t : T; 
               READONLY arr : ARRAY OF Pathname.T; 
               env : REFANY) : Scheme.T 
  RAISES { E } =
  BEGIN
    t.setPrimitives(prims); (* load in my special primitives *)
    RETURN Scheme.T.init(t,arr,env);
  END Init;

(**********************************************************************)

PROCEDURE JailBreakApply(<*UNUSED*>p : SchemeProcedure.T; interp : Scheme.T; args : Object) : Object RAISES { E } =
  BEGIN
    WITH i = NARROW(interp, T) DO
      IF i.jailBreak = NIL THEN
        RETURN Error("No jailbreak defined")
      ELSE
        RETURN i.jailBreak.apply(args)
      END
    END
  END JailBreakApply;

PROCEDURE Modula3OpApply(<*UNUSED*>p : SchemeProcedure.T; interp : Scheme.T; args : Object) : Object RAISES { E } =
  BEGIN
    WITH i = NARROW(interp, T) DO
      IF i.m3TableOps = NIL THEN
        RETURN Error("No table ops defined")
      ELSE
        RETURN i.m3TableOps.apply(args)
      END
    END
  END Modula3OpApply;

PROCEDURE FmtRealApply(<*UNUSED*>p : SchemeProcedure.T; <*UNUSED*>interp : Scheme.T; args : Object) : Object RAISES { E } =
  BEGIN
    WITH x = First(args), y = Second(args), z = Third(args) DO
      VAR
        style : Fmt.Style;
      BEGIN
        IF    SchemeSymbol.SymEq(y, "auto") THEN
          style := Fmt.Style.Auto
        ELSIF SchemeSymbol.SymEq(y, "fix") THEN
          style := Fmt.Style.Fix
        ELSIF SchemeSymbol.SymEq(y, "sci") THEN
          style := Fmt.Style.Sci
        ELSE
          RETURN Error("Unknown formatting style " & Stringify(y))
        END;
        
        RETURN SchemeString.FromText(Fmt.LongReal(FromO(x),
                                                  style,
                                                  TRUNC(FromO(z))))
      END
    END
  END FmtRealApply;

PROCEDURE GCApply(<*UNUSED*>p : SchemeProcedure.T; <*UNUSED*>interp : Scheme.T; <*UNUSED*>args : Object) : Object =
  BEGIN RTCollector.Collect(); RETURN True() END GCApply;

PROCEDURE StringifyApply(<*UNUSED*>p : SchemeProcedure.T; <*UNUSED*>interp : Scheme.T; args : Object) : Object RAISES { E } =
  BEGIN 
    WITH x = First(args) DO
      RETURN SchemeString.FromText(Stringify(x)) 
    END
  END StringifyApply;

PROCEDURE TimeToStringApply(<*UNUSED*>p : SchemeProcedure.T; 
                            <*UNUSED*>interp : Scheme.T; 
                                      args : Object) : Object RAISES { E } =
  BEGIN
    CONST
      Months = ARRAY Date.Month OF TEXT {
      "JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
      "JUL", "AUG", "SEP", "OCT", "NOV", "DEC" 
      };
    VAR
      x := First(args);
      t := SchemeLongReal.FromO(x);
      d : Date.T;
    BEGIN
      IF t > FLOAT(LAST(CARDINAL),Time.T) OR t < 0.0d0 THEN
        RETURN Error("Time out of range " & Fmt.LongReal(t))
      END;

      d := Date.FromTime(t);
      RETURN SchemeString.FromText(Fmt.F("%04s-%3s-%02s ",
                                         Fmt.Int(d.year),
                                         Months[d.month],
                                         Fmt.Int(d.day)) &
                                         Fmt.F("%02s:%02s:%02s.%03s",
                                               Fmt.Int(d.hour),
                                               Fmt.Int(d.minute),
                                               Fmt.Int(d.second),
                                               Fmt.Int(TRUNC((t - FLOAT(TRUNC(t),Time.T))*1000.0d0))))
    END


  END TimeToStringApply;

PROCEDURE TimenowApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                       <*UNUSED*>args : Object) : Object =
  BEGIN
    RETURN SchemeLongReal.FromLR(Time.Now())
  END TimenowApply;

PROCEDURE StdioStderrApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                       <*UNUSED*>args : Object) : Object =
  BEGIN
    RETURN Stdio.stderr
  END StdioStderrApply;

PROCEDURE DebugAddstreamApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  BEGIN
    WITH x = First(args) DO
      IF x = NIL OR NOT ISTYPE(x, Wr.T) THEN
        RETURN Error ("Not a Wr.T: " & Stringify(x))
      END;
      Debug.AddStream(x);
      RETURN x
    END
  END DebugAddstreamApply;

PROCEDURE DebugRemstreamApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  BEGIN
    WITH x = First(args) DO
      IF x = NIL OR NOT ISTYPE(x, Wr.T) THEN
        RETURN Error ("Not a Wr.T: " & Stringify(x))
      END;
      Debug.RemStream(x);
      RETURN x
    END
  END DebugRemstreamApply;

PROCEDURE FileWrOpenApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  BEGIN
    WITH x = SchemeString.ToText(First(args)) DO
      TRY
        RETURN FileWr.Open(x)
      EXCEPT
        OSError.E(err) => RETURN Error("FileWrOpenApply : " & AL.Format(err))
      END
    END
  END FileWrOpenApply;

PROCEDURE DebugSetEnvApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  BEGIN
    WITH x = SchemeString.ToText(First(args)) DO
      TRY
        Debug.SetEnv(x); RETURN True()
      EXCEPT
        (*
        OSError.E(err) => RETURN Error("DebugSetEnvApply : " & AL.Format(err))
        *)
      END
    END
  END DebugSetEnvApply;

PROCEDURE DebugClearEnvApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  BEGIN
    WITH x = SchemeString.ToText(First(args)) DO
      TRY
        Debug.ClearEnv(x); RETURN True()
      EXCEPT
        (*
          OSError.E(err) => RETURN Error("DebugClearEnvApply : " & AL.Format(err))
          *)
      END
    END
  END DebugClearEnvApply;

PROCEDURE DebugHaveEnvApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  BEGIN
    WITH x = SchemeString.ToText(First(args)) DO
      TRY
        RETURN Truth(Debug.HaveEnv(x))
      EXCEPT
        (*
        OSError.E(err) => RETURN Error("DebugHaveEnvApply : " & AL.Format(err))
        *)
      END
    END
  END DebugHaveEnvApply;

PROCEDURE WrCloseApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  BEGIN
    WITH x = (First(args)) DO
      TRY
        IF x = NIL OR NOT ISTYPE(x, Wr.T) THEN
          RETURN Error("WrCloseApply : type error : " & Stringify(x))
        END;
        Wr.Close(x);
        RETURN True()
      EXCEPT
        Wr.Failure(err) => RETURN Error("WrCloseApply : Wr.Failure : " & AL.Format(err))
      END
    END
  END WrCloseApply;

PROCEDURE HostnameApply(<*UNUSED*>p : SchemeProcedure.T; 
                        <*UNUSED*>interp : Scheme.T; 
                        <*UNUSED*>args : Object) : Object RAISES { E } =
  BEGIN
    TRY
      RETURN SchemeSymbol.Symbol(IP.GetCanonicalByAddr(IP.GetHostAddr()))
    EXCEPT
      IP.Error(ec) =>
      RETURN Error("HostnameApply : IP.Error : " & AL.Format(ec))
    END
  END HostnameApply;

PROCEDURE GetUnixPIDApply(<*UNUSED*>p : SchemeProcedure.T; 
                        <*UNUSED*>interp : Scheme.T; 
                        <*UNUSED*>args : Object) : Object =
  BEGIN
    RETURN SchemeLongReal.FromI(Process.GetMyID())
  END GetUnixPIDApply;

(**********************************************************************)

PROCEDURE ExtendWithM3(prims : SchemePrimitive.ExtDefiner) =
  BEGIN 
    prims.addPrim("jailbreak", NEW(SchemeProcedure.T, 
                                   apply := JailBreakApply), 
                  1, 1);
    
    prims.addPrim("stringify", NEW(SchemeProcedure.T, 
                                   apply := StringifyApply), 
                  1, 1);

    prims.addPrim("fmtreal", NEW(SchemeProcedure.T,
                                 apply := FmtRealApply), 
                  3, 3);

    prims.addPrim("gc", NEW(SchemeProcedure.T, 
                            apply := GCApply),
                  0, 0);

    prims.addPrim("timenow", NEW(SchemeProcedure.T, 
                                 apply := TimenowApply), 
                  0, 0);

    prims.addPrim("time->string", NEW(SchemeProcedure.T, 
                                      apply := TimeToStringApply), 
                  1, 1);

    prims.addPrim("modula-3-op", NEW(SchemeProcedure.T, 
                                     apply := Modula3OpApply), 
                  2, 3);

    prims.addPrim("stdio-stderr", NEW(SchemeProcedure.T,
                                      apply := StdioStderrApply), 
                  0, 0);

    prims.addPrim("debug-addstream", NEW(SchemeProcedure.T,
                                      apply := DebugAddstreamApply), 
                  1, 1);
    prims.addPrim("debug-remstream", NEW(SchemeProcedure.T,
                                      apply := DebugRemstreamApply), 
                  1, 1);

    prims.addPrim("debug-setenv", NEW(SchemeProcedure.T,
                                      apply := DebugSetEnvApply),
                  1, 1);

    prims.addPrim("debug-clearenv", NEW(SchemeProcedure.T,
                                      apply := DebugClearEnvApply),
                  1, 1);

    prims.addPrim("debug-haveenv", NEW(SchemeProcedure.T,
                                      apply := DebugHaveEnvApply),
                  1, 1);

    prims.addPrim("filewr-open", NEW(SchemeProcedure.T,
                                      apply := FileWrOpenApply), 
                  1, 1);
    prims.addPrim("wr-close", NEW(SchemeProcedure.T,
                                      apply := WrCloseApply), 
                  1, 1);

    prims.addPrim("hostname", NEW(SchemeProcedure.T, 
                                 apply := HostnameApply), 
                  0, 0);

    prims.addPrim("getunixpid", NEW(SchemeProcedure.T, 
                                    apply := GetUnixPIDApply), 
                  0, 0);

  END ExtendWithM3;

VAR 
  prims := NEW(SchemePrimitive.ExtDefiner).init();
BEGIN 
  TextRefSchemeAutoTbl.Register(); (* vide module initialization order,
                                      Green Book *)
  ExtendWithM3(prims)
END SchemeM3.
