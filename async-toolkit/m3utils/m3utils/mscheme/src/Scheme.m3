(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE Scheme;
IMPORT SchemeClass;
IMPORT SchemeInputPort, SchemeEnvironment, SchemePrimitives, SchemePrimitive;
IMPORT SchemeEnvironmentSuper;
IMPORT SchemeBoolean, SchemeSymbol, SchemeMacro;
IMPORT SchemeClosure, SchemeClosureClass, SchemeProcedure, SchemeString;
IMPORT Pathname, Stdio;
IMPORT Wr, TextRd, Thread;
IMPORT AL, FileRd, Rd, OSError, SchemeUtils;
FROM SchemeUtils IMPORT Stringify;
IMPORT SchemePair;
IMPORT Debug;

TYPE Pair = SchemePair.T;

<* FATAL Thread.Alerted *>

REVEAL
  T = SchemeClass.Private BRANDED Brand OBJECT
    globalEnvironment : SchemeEnvironment.T;
  METHODS
    readInitialFiles(READONLY files : ARRAY OF Pathname.T) RAISES { E } := ReadInitialFiles;
    reduceCond(clauses : Object; env : SchemeEnvironment.T) : Object RAISES { E } := ReduceCond;
  OVERRIDES
    init              :=  Init;
    defineInGlobalEnv :=  DefineInGlobalEnv;
    readEvalWriteLoop :=  ReadEvalWriteLoop;
    loadFile          :=  LoadFile;
    loadPort          :=  LoadPort;
    loadRd            :=  LoadRd;
    eval              :=  Eval;
    evalInGlobalEnv   :=  EvalInGlobalEnv;
    evalList          :=  EvalList;
  END;

PROCEDURE Init(t : T; READONLY files : ARRAY OF Pathname.T) : T 
  RAISES { E } =
  BEGIN
    t.input := NEW(SchemeInputPort.T).init(Stdio.stdin);
    t.output := Stdio.stdout;
    t.globalEnvironment := NEW(SchemeEnvironment.T).initEmpty();
    EVAL SchemePrimitive.InstallPrimitives(t.globalEnvironment);
    t.readInitialFiles(files);
    RETURN t
  END Init;

PROCEDURE ReadInitialFiles(t : T; READONLY files : ARRAY OF Pathname.T) RAISES { E } =
  BEGIN
    EVAL t.loadRd(NEW(TextRd.T).init(SchemePrimitives.Code));
    FOR i := FIRST(files) TO LAST(files) DO
      EVAL t.loadFile(SchemeString.FromText(files[i]))
    END
  END ReadInitialFiles;

PROCEDURE DefineInGlobalEnv(t : T; var, val : Object) =
  BEGIN EVAL t.globalEnvironment.define(var,val) END DefineInGlobalEnv;

PROCEDURE ReadEvalWriteLoop(t : T) RAISES { Wr.Failure } =
  BEGIN
    TRY
    LOOP
      Wr.PutText(t.output, ">"); Wr.Flush(t.output);
      WITH x = t.input.read() DO
        IF SchemeInputPort.IsEOF(x) THEN RETURN END;
        TRY
          EVAL SchemeUtils.Write(t.evalInGlobalEnv(x), t.output, TRUE);
        EXCEPT
          E(e) => Wr.PutText(t.output, "EXCEPTION! " & e & "\n")
        END;
        Wr.PutText(t.output, "\n"); Wr.Flush(t.output)
      END
    END
    EXCEPT
      E(e) =>
      (* only way we can get here is if we have a failure in t.input.read() *)
      TRY Wr.PutText(t.output, "READ FAILURE : "&e&" .\n") EXCEPT ELSE END;
      RETURN
    END
  END ReadEvalWriteLoop;

PROCEDURE LoadRd(t : T; rd : Rd.T) : Object RAISES { E } =
  BEGIN RETURN t.loadPort(NEW(SchemeInputPort.T).init(rd)) END LoadRd;

PROCEDURE LoadFile(t : T; fileName : Object) : Object RAISES { E } =
  BEGIN
    WITH name = SchemeUtils.StringifyQ(fileName,FALSE) DO
      TRY
        RETURN t.loadRd(FileRd.Open(name))
      EXCEPT
        OSError.E(err) => RETURN SchemeUtils.Error("can't load " & name & " : OSError.E : " & AL.Format(err))
      END
    END
  END LoadFile;

PROCEDURE LoadPort(t : T; in : Object) : Object 
  RAISES { E } =
  BEGIN
    IF in = NIL OR NOT ISTYPE(in, SchemeInputPort.T) THEN
      RAISE E("Not an input port: " & Stringify(in))
    END;

    LOOP
      WITH x = NARROW(in,SchemeInputPort.T).read() DO
        IF SchemeInputPort.IsEOF(x) THEN RETURN SchemeBoolean.True() END;
        EVAL t.evalInGlobalEnv(x)
      END
    END
  END LoadPort;

PROCEDURE Eval(t : T; x : Object; envP : SchemeEnvironmentSuper.T) : Object 
  RAISES { E } =
  TYPE  Macro     = SchemeMacro.T;
        Closure   = SchemeClosure.T;
        Procedure = SchemeProcedure.T;

  CONST First  = SchemeUtils.First;
        Second = SchemeUtils.Second;
        Third  = SchemeUtils.Third;
        Rest   = SchemeUtils.Rest;
        Cons   = SchemeUtils.Cons;
        Sym    = SchemeSymbol.Symbol;
        TruthO = SchemeBoolean.TruthO;

  VAR
    env := NARROW(envP, SchemeEnvironment.T);
  BEGIN
    LOOP
      IF x # NIL AND ISTYPE(x,Symbol) THEN
        RETURN env.lookup(x)
      ELSIF x = NIL OR NOT ISTYPE(x,Pair) THEN 
        RETURN x
      ELSE
        VAR
          fn   := First(x);
          args := Rest(x); 
        BEGIN
          IF    fn = SYMquote THEN
            RETURN First(args)
          ELSIF fn = SYMbegin THEN
            WHILE Rest(args) # NIL DO
              EVAL t.eval(First(args),env);
              args := Rest(args)
            END;
            x := First(args)
          ELSIF fn = SYMdefine THEN
            IF First(args) # NIL AND ISTYPE(First(args), Pair) THEN
              RETURN env.define(First(First(args)),
                                t.eval(Cons(Sym("lambda"),
                                            Cons(Rest(First(args)), 
                                                 Rest(args))), env))
            ELSE
              RETURN env.define(First(args),
                                t.eval(Second(args), env))
            END
          ELSIF fn = SYMsetB THEN
            RETURN env.set(First(args), t.eval(Second(args), env))
          ELSIF fn = SYMif THEN
            IF TruthO(t.eval(First(args), env)) THEN
              x := Second(args) 
            ELSE
              x := Third(args)
            END
          ELSIF fn = SYMcond THEN
            x := t.reduceCond(args, env)
          ELSIF fn = SYMlambda THEN
            RETURN NEW(SchemeClosure.T).init(First(args), 
                                             Rest(args),
                                             env)
          ELSIF fn = SYMmacro THEN
            RETURN NEW(Macro).init(First(args),
                                   Rest(args),
                                   env)
          ELSE
            (* procedure call *)
            fn := t.eval(fn, env);
            
            TYPECASE fn OF
              Macro(m) => 
(*
              Debug.Out("x = " & Stringify(x));
              Debug.Out("args = " & Stringify(args));
              Debug.Out("-------");
*)
              x := m.expand(t, x, args)
            |
              Closure(c) => x := c.body; 
              env := NEW(SchemeEnvironment.T).init(c.params, 
                                                   t.evalList(args,env),
                                                   c.env)
            |
              Procedure(p) =>
              RETURN p.apply(t, t.evalList(args,env))
            ELSE
              <* ASSERT FALSE *>  (* hmm? *)
            END
          END
        END
      END
    END
  END Eval;

PROCEDURE EvalInGlobalEnv(t : T; x : Object) : Object RAISES { E } =
  BEGIN RETURN t.eval(x, t.globalEnvironment) END EvalInGlobalEnv;

PROCEDURE EvalList(t : T; list : Object; env : SchemeEnvironmentSuper.T) : Object 
  RAISES { E } =
  CONST Error = SchemeUtils.Error;
  BEGIN
    TRY
      IF list = NIL THEN
        RETURN NIL
      ELSIF NOT ISTYPE(list, Pair) THEN
        EVAL Error("Illegal arg list: " & SchemeUtils.DebugFormat(list));
        RETURN NIL (*notreached*)
      ELSE
        RETURN SchemeUtils.Cons(t.eval(SchemeUtils.First(list), env), 
                                t.evalList(SchemeUtils.Rest(list), env))
      END
    EXCEPT
      E(ex) => 
      TRY
        Wr.PutText(Stdio.stdout, "Scheme.evalList raising E, evaluating " &
          SchemeUtils.DebugFormat(list));
      EXCEPT ELSE END;
      EVAL Error(ex); RETURN NIL (*notreached*)
    END
  END EvalList;

PROCEDURE ReduceCond(t : T; 
                     clauses : Object; env : SchemeEnvironment.T) : Object 
  RAISES { E } =

  CONST First  = SchemeUtils.First;
        Second = SchemeUtils.Second;
        Third  = SchemeUtils.Second;
        Rest   = SchemeUtils.Rest;
        Cons   = SchemeUtils.Cons;
        List2  = SchemeUtils.List2;
        Sym    = SchemeSymbol.Symbol;
        TruthO = SchemeBoolean.TruthO;

  VAR result : Object := NIL;

  BEGIN
    LOOP
      IF clauses = NIL THEN RETURN SchemeBoolean.False() END;
      
      WITH clause = First(clauses) DO
        clauses := Rest(clauses);
        VAR
          success := FALSE;
        BEGIN
          IF First(clause) = SYMelse THEN
            success := TRUE
          ELSE 
            result := t.eval(First(clause),env);
            (* is this a bug? we overwrite result even if we don't succeed,
               Norvig's SILK does this too... *)
            IF TruthO(result) THEN success := TRUE END; 
          END;

          IF success THEN
            IF Rest(clause) = NIL THEN
              RETURN List2(Sym("quote"),result)
            ELSIF Second(clause) = SYMarrow THEN
              RETURN List2(Third(clause),List2(Sym("quote"),result))
            ELSE 
              RETURN Cons(Sym("begin"), Rest(clause))
            END
          END
        END
      END
    END
  END ReduceCond;

VAR
  SYMquote := SchemeSymbol.Symbol("quote");
  SYMbegin := SchemeSymbol.Symbol("begin");
  SYMdefine := SchemeSymbol.Symbol("define");
  SYMsetB := SchemeSymbol.Symbol("set!");
  SYMif := SchemeSymbol.Symbol("if");
  SYMcond := SchemeSymbol.Symbol("cond");
  SYMlambda := SchemeSymbol.Symbol("lambda");
  SYMmacro := SchemeSymbol.Symbol("macro");
  SYMelse := SchemeSymbol.Symbol("else");
  SYMarrow := SchemeSymbol.Symbol("=>");

BEGIN END Scheme.



