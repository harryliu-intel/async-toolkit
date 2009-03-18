(* $Id$ *)

MODULE SchemeM3;

IMPORT Scheme;
FROM Scheme IMPORT E, Object, SymbolCheck;

IMPORT SchemeProcedure, SchemePrimitive;
IMPORT SchemeJailBreak, SchemeM3TableOps;
IMPORT Time, Date, RTCollector, Pathname;
IMPORT OSError;
IMPORT SchemeSymbol;
FROM SchemeUtils IMPORT First, Second, Third, Stringify, Error;
IMPORT SchemeString, SchemeLongReal;
FROM SchemeLongReal IMPORT FromO;
FROM SchemeBoolean IMPORT True, Truth;
IMPORT SchemeBoolean;
IMPORT Fmt;
IMPORT TextRefSchemeAutoTbl;
IMPORT Stdio, Wr, Debug, AL, FileWr, Thread;
IMPORT IP, Process;
IMPORT SchemeEnvironment;
IMPORT TZ, SchemeUtils;
IMPORT NetObj;
IMPORT FS;

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

PROCEDURE HaveGlobalNameApply(<*UNUSED*>p : SchemeProcedure.T; 
                              interp : Scheme.T; 
                              args : Object) : Object 
  RAISES { E } =
  VAR
    e : SchemeEnvironment.Public;
  BEGIN
    WITH sym = SymbolCheck(First(args)),
         ee = interp.getGlobalEnvironment() DO
      IF ISTYPE(ee, SchemeEnvironment.Public) THEN
        e := ee
      ELSE
        RAISE E("Unknown error: global environment of wrong type?")
      END;

      TRY
        EVAL e.lookup(sym);
        RETURN SchemeBoolean.True()
      EXCEPT
        E => RETURN SchemeBoolean.False()
      END
    END
  END HaveGlobalNameApply;

PROCEDURE DefineNewGlobal(<*UNUSED*>p : SchemeProcedure.T; 
                          interp : Scheme.T; 
                          args : Object) : Object 
  RAISES { E } =
  VAR
    e : SchemeEnvironment.Public;
  BEGIN
    WITH sym = SymbolCheck(First(args)),
         ee = interp.getGlobalEnvironment() DO
      IF ISTYPE(ee, SchemeEnvironment.Public) THEN
        e := ee
      ELSE
        RAISE E("Unknown error: global environment of wrong type?")
      END;

      RETURN e.define(sym, Second(args))
    END
  END DefineNewGlobal;
    
PROCEDURE JailBreakApply(<*UNUSED*>p : SchemeProcedure.T; 
                         interp : Scheme.T; 
                         args : Object) : Object RAISES { E } =
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
    CONST
      Months = ARRAY Date.Month OF TEXT {
      "JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
      "JUL", "AUG", "SEP", "OCT", "NOV", "DEC" 
      };
    VAR
      x      := First(args);
      tzName := Second(args);
      t      := SchemeLongReal.FromO(x);
      d      : Date.T;
    BEGIN
      IF t > FLOAT(LAST(CARDINAL),Time.T) OR t < 0.0d0 THEN
        RETURN Error("Time out of range " & Fmt.LongReal(t))
      END;

      IF tzName = NIL THEN
        d := Date.FromTime(t)
      ELSE
        TRY
          WITH tz = NEW(TZ.T).init(SchemeString.ToText(tzName)) DO
            d := tz.localtime(t)
          END
        EXCEPT
          OSError.E(err) =>
          RAISE E("Couldn't handle TZ \"" & SchemeString.ToText(tzName) & 
                "\": OSError.E: " & AL.Format(err))
        END
      END;

      RETURN SchemeString.FromText(Fmt.F("%04s-%3s-%02s ",
                                         Fmt.Int(d.year),
                                         Months[d.month],
                                         Fmt.Int(d.day)) &
                                         Fmt.F("%02s:%02s:%02s.%03s",
                                               Fmt.Int(d.hour),
                                               Fmt.Int(d.minute),
                                               Fmt.Int(d.second),
                                               Fmt.Int(TRUNC((t - FLOAT(TRUNC(t),Time.T))*1000.0d0))))


  END TimeToStringApply;

PROCEDURE TimeToListApply(<*UNUSED*>p : SchemeProcedure.T; 
                            <*UNUSED*>interp : Scheme.T; 
                                      args : Object) : Object RAISES { E } =
  CONST 
    LR = SchemeLongReal.FromLR;
    I  = SchemeLongReal.FromI;
  VAR
    x      := First(args);
    tzName := Second(args);
    t      := SchemeLongReal.FromO(x);
    d      : Date.T;
  BEGIN
    IF t > FLOAT(LAST(CARDINAL),Time.T) OR t < 0.0d0 THEN
      RETURN Error("Time out of range " & Fmt.LongReal(t))
    END;
    
    IF tzName = NIL THEN
      d := Date.FromTime(t)
    ELSE
      TRY
        WITH tz = NEW(TZ.T).init(SchemeString.ToText(tzName)) DO
          d := tz.localtime(t)
        END
      EXCEPT
        OSError.E(err) =>
        RAISE E("Couldn't handle TZ \"" & SchemeString.ToText(tzName) & 
              "\": OSError.E: " & AL.Format(err))
      END
    END;
    
    WITH subsecs = t - FLOAT(TRUNC(t),Time.T) DO
      RETURN SchemeUtils.MakeList(
                 ARRAY OF Object { I(d.year),
                                   I(ORD(d.month)+1),
                                   I(d.day),
                                   I(ORD(d.weekDay)),
                                   I(d.hour),
                                   I(d.minute),
                                   LR(FLOAT(d.second,LONGREAL)+subsecs) })
    END

  END TimeToListApply;

PROCEDURE TimenowApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                       <*UNUSED*>args : Object) : Object =
  BEGIN
    RETURN SchemeLongReal.FromLR(Time.Now())
  END TimenowApply;

PROCEDURE NetObjExportGlobalEnvApply(<*UNUSED*>p : SchemeProcedure.T; 
                                     interp      : Scheme.T; 
                                     args        : Object) : Object 
  RAISES { E } =
  BEGIN
    WITH nam = First(args) DO
      NetObj.Export(SchemeString.ToText(nam),
                    interp.getGlobalEnvironment());
      RETURN nam
    END
  END NetObjExportGlobalEnvApply;

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

PROCEDURE DebugSetLevelApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  BEGIN
    WITH x = TRUNC(SchemeLongReal.FromO(First(args))) DO
      Debug.SetLevel(x);
      RETURN First(args)
    END
  END DebugSetLevelApply;

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
  VAR
    fn : Pathname.T;
    append := FALSE;
  BEGIN
    IF ISTYPE(First(args),SchemeSymbol.T) THEN
      WITH sym = First(args) DO
        IF sym = SchemeSymbol.FromText("append") THEN
          append := TRUE
        END
      END;
      fn := SchemeString.ToText(Second(args))
    ELSE
      fn := SchemeString.ToText(First(args))
    END;

    TRY
      IF append THEN
        RETURN FileWr.OpenAppend(fn)
      ELSE
        RETURN FileWr.Open(fn)
      END
    EXCEPT
      OSError.E(err) => RETURN Error("FileWrOpenApply : " & AL.Format(err))
    END
  END FileWrOpenApply;

PROCEDURE FileRmApply(<*UNUSED*>p : SchemeProcedure.T; 
                       <*UNUSED*>interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  BEGIN
    WITH fn = SchemeString.ToText(First(args)) DO
      TRY
        FS.DeleteFile(fn)
      EXCEPT
        OSError.E(err) => RETURN Error("FileRmApply : " & AL.Format(err))
      END
    END;
    RETURN SchemeBoolean.True()
  END FileRmApply;

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

PROCEDURE ExtendWithM3(prims : SchemePrimitive.ExtDefiner)  : SchemePrimitive.ExtDefiner =
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
                  1, 2);

    prims.addPrim("time->list", NEW(SchemeProcedure.T, 
                                      apply := TimeToListApply), 
                  1, 2);

    prims.addPrim("modula-3-op", NEW(SchemeProcedure.T, 
                                     apply := Modula3OpApply), 
                  2, 3);

    prims.addPrim("stdio-stderr", NEW(SchemeProcedure.T,
                                      apply := StdioStderrApply), 
                  0, 0);

    prims.addPrim("debug-addstream", NEW(SchemeProcedure.T,
                                      apply := DebugAddstreamApply), 
                  1, 1);

    prims.addPrim("debug-setlevel", NEW(SchemeProcedure.T,
                                      apply := DebugSetLevelApply), 
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
                  1, 2);

    prims.addPrim("remove-file", NEW(SchemeProcedure.T,
                                     apply := FileRmApply),
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

    prims.addPrim("global-exists?", NEW(SchemeProcedure.T, 
                                        apply := HaveGlobalNameApply), 
                  1, 1);

    prims.addPrim("define-global-symbol", NEW(SchemeProcedure.T, 
                                              apply := DefineNewGlobal), 
                  2, 2);

    prims.addPrim("netobj-export-global-environment",
                  NEW(SchemeProcedure.T, 
                      apply := NetObjExportGlobalEnvApply), 
                  1, 1);

    RETURN prims
  END ExtendWithM3;

PROCEDURE GetPrims() : SchemePrimitive.ExtDefiner = 
  BEGIN RETURN prims END GetPrims;

VAR 
  prims := NEW(SchemePrimitive.ExtDefiner).init();
BEGIN 
  TextRefSchemeAutoTbl.Register(); (* vide module initialization order,
                                      Green Book *)
  prims := ExtendWithM3(prims);
  SchemeEnvironment.ExtendWithIntrospectionPrimitives(prims)  
END SchemeM3.
