(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE Scheme EXPORTS Scheme, SchemeClass;
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
<*NOWARN*>IMPORT Debug;

TYPE Pair = SchemePair.T;

<* FATAL Thread.Alerted *>

REVEAL
  T = SchemeClass.Private BRANDED Brand OBJECT
    globalEnvironment : SchemeEnvironment.T;
    interrupter : Interrupter := NIL;
    prims : SchemePrimitive.Definer;
  METHODS
    readInitialFiles(READONLY files : ARRAY OF Pathname.T) RAISES { E } := ReadInitialFiles;
    reduceCond(clauses : Object; env : SchemeEnvironment.T) : Object RAISES { E } := ReduceCond;
  OVERRIDES
    init              :=  Init;
    defineInGlobalEnv :=  DefineInGlobalEnv;
    readEvalWriteLoop :=  ReadEvalWriteLoop;
    setInterrupter    :=  SetInterrupter;
    loadFile          :=  LoadFile;
    loadPort          :=  LoadPort;
    loadRd            :=  LoadRd;
    eval              :=  Eval;
    evalInGlobalEnv   :=  EvalInGlobalEnv;
    evalList          :=  EvalList2;
    bind              :=  Bind;
    setInGlobalEnv    :=  SetInGlobalEnv;
    setPrimitives     :=  SetPrimitives;
  END;

PROCEDURE SetPrimitives(t : T; spd : REFANY) =
  BEGIN t.prims := spd END SetPrimitives;

PROCEDURE Bind(t : T; var : Symbol; val : Object) =
  BEGIN EVAL t.globalEnvironment.define(var,val) END Bind;

PROCEDURE SetInGlobalEnv(t : T; var : Symbol; val : Object) RAISES { E } =
  BEGIN EVAL t.globalEnvironment.set(var,val) END SetInGlobalEnv;

PROCEDURE Init(t : T; READONLY files : ARRAY OF Pathname.T) : T 
  RAISES { E } =
  BEGIN
    t.input := NEW(SchemeInputPort.T).init(Stdio.stdin);
    t.output := Stdio.stdout;
    t.globalEnvironment := NEW(SchemeEnvironment.Unsafe).initEmpty();
    EVAL NEW(SchemePrimitive.DefaultDefiner).installPrimitives(t.globalEnvironment);
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

PROCEDURE SetInterrupter(t : T; i : Interrupter) =
  BEGIN t.interrupter := i END SetInterrupter;

PROCEDURE ReadEvalWriteLoop(t : T; int : Interrupter) RAISES { Wr.Failure } =
  BEGIN
    t.setInterrupter(int);

    TRY
      t.bind(SchemeSymbol.Symbol("bang-bang"), NIL);

    LOOP
      Wr.PutText(t.output, ">"); Wr.Flush(t.output);
      WITH x = t.input.read() DO
        IF SchemeInputPort.IsEOF(x) THEN RETURN END;
        TRY
          WITH res = t.evalInGlobalEnv(x) DO
            EVAL SchemeUtils.Write(res, t.output, TRUE);
            EVAL t.globalEnvironment.set(SchemeSymbol.Symbol("bang-bang"),res)
          END
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
        TruthO = SchemeBoolean.TruthO;

  VAR
    env := NARROW(envP, SchemeEnvironment.T);
    envIsLocal := FALSE;
    DebugLevel := Debug.GetLevel();
  BEGIN
    LOOP
      IF DebugLevel >= 20 THEN Debug.Out("EVAL: " & Stringify(x)) END;


      IF t.interrupter # NIL AND t.interrupter.interrupt() THEN
        RAISE E("Command interrupted")
      END;

      IF x # NIL AND ISTYPE(x,Symbol) THEN
        RETURN env.lookup(x)
      ELSIF x = NIL OR NOT ISTYPE(x,Pair) THEN 
        RETURN x
      ELSE
        VAR
          fn   := NARROW(x,Pair).first;
          args := NARROW(x,Pair).rest; 
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
                                t.eval(Cons(SYMlambda,
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
              NULL => RAISE E("Not a procedure: " & Stringify(fn))
            |
              Macro(m) => 
              x := m.expand(t, x, args)
            |
              Closure(c) => 
              (* can we check here if this is the last thing we
                 eval, in which case we can just re-init the
                 environment?  it won't be re-used, right? *)
              x := c.body; 


              (* this is a teeny tiny tail call optimization.. *)

              (* the following code is a bit tricky and highly
                 optimized.

                 First: we know that the environment can't escape
                 this routine if it is locally manufactured. 
                 (We cannot overwrite passed-in environments, because
                 who knows where else those are used.)  Therefore,
                 if the environment has been allocated locally (on
                 a previous tail call, for instance), we take
                 the liberty to overwrite it instead of allocating
                 a new one.  This is a major performance optimization
                 on systems with slow garbage collectors!

                 Secondly: we see about a 2-4% increase in performance
                 tests by using initEval instead of evalList. 
                 InitEval skips the allocation of the list and instead
                 evaluates the arg list "in place" inside the environment.
                 This optimization is of more dubious value as most
                 of its advantages are already provided by the recycling
                 of cons cells that we do with the "ReturnCons"
                 mechanism. 
              *)
              VAR
                canRecyclePairs := TRUE;
              BEGIN
              IF envIsLocal AND c.env # env THEN
                INC(envsKilled);
                WITH lst = t.evalList(args,env) DO
                  EVAL env.init(c.params,
                                lst,
                                c.env,
                                canRecyclePairs);
                  IF canRecyclePairs THEN ReturnCons(t,lst) END
                END
              ELSE
                INC(envsMade);
                
                (* this is a LOCAL environment.  No need for it to
                   use synchronized methods *)
                
                env := NEW(SchemeEnvironment.Unsafe).initEval(c.params,
                                                              args,env,t,
                                                              c.env)

              END
              END;
              
              envIsLocal := TRUE

            |
              Procedure(p) =>

              (* more micro-optimizations:
                 
                 apply1 and apply2 unconditionally recycle their
                 list objects internally.

                 at present, this optimization is only provided in
                 SchemePrimitive.m3

                 Yes the code looks messy but at present it can be
                 as much as a 25% performance improvement on machines
                 with slow GC.
              *)
              TYPECASE args OF
                NULL =>
              |
                Pair(pp) =>
                IF pp.rest = NIL THEN
                  RETURN p.apply1(t, t.eval(pp.first, env))
                ELSIF ISTYPE(pp.rest,Pair) 
                  AND NARROW(pp.rest,Pair).rest = NIL THEN
                  RETURN p.apply2(t, 
                                  t.eval(pp.first,env),
                                  t.eval(NARROW(pp.rest,Pair).first, env))
                END
              ELSE END;
              RETURN p.apply(t, t.evalList(args,env))
            ELSE
              RAISE E("Not a procedure: " & Stringify(fn))
            END
          END
        END
      END
    END
  END Eval;

VAR envsMade := 0;
    envsKilled := 0;

PROCEDURE EvalInGlobalEnv(t : T; x : Object) : Object RAISES { E } =
  BEGIN RETURN t.eval(x, t.globalEnvironment) END EvalInGlobalEnv;

PROCEDURE EvalList2(t : T; list : Object; env : SchemeEnvironmentSuper.T) : Object 
  RAISES { E } =
  CONST Error = SchemeUtils.Error;
  VAR
    res : Pair := NIL;
    ptr : Pair;
  BEGIN
    LOOP
      IF list = NIL THEN
        RETURN res
      ELSIF NOT ISTYPE(list, Pair) THEN
        EVAL Error("Illegal arg list: " & SchemeUtils.StringifyT(list));
        RETURN NIL (*notreached*)
      ELSE
        WITH pair = NARROW(list,Pair) DO
          WITH new = GetCons(t) DO
            new.first := t.eval(pair.first, env);
            new.rest := NIL;

            IF res = NIL THEN
              ptr := new;
              res := ptr
            ELSE
              ptr.rest := new;
              ptr := new
            END;
            list := pair.rest
          END
        END
      END
    END
  END EvalList2;

PROCEDURE ReduceCond(t : T; 
                     clauses : Object; env : SchemeEnvironment.T) : Object 
  RAISES { E } =

  CONST First  = SchemeUtils.First;
        Second = SchemeUtils.Second;
        Third  = SchemeUtils.Second;
        Rest   = SchemeUtils.Rest;
        Cons   = SchemeUtils.Cons;
        List2  = SchemeUtils.List2;
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
              RETURN List2(SYMquote,result,t)
            ELSIF Second(clause) = SYMarrow THEN
              RETURN List2(Third(clause),List2(SYMquote,result),t)
            ELSE 
              RETURN Cons(SYMbegin, Rest(clause),t)
            END
          END
        END
      END
    END
  END ReduceCond;

PROCEDURE GetCons(t : T) : Pair =
  BEGIN
    IF t.freePairs # NIL THEN
      VAR 
        res := t.freePairs;
        next := res.rest;
      BEGIN
        t.freePairs := next;
        RETURN res
      END
    END;
    RETURN NEW(Pair)
  END GetCons;

PROCEDURE ReturnCons(t : T; cons : Pair) = 
  BEGIN
    IF cons = NIL THEN RETURN END;

    VAR p : Pair := cons; BEGIN
      WHILE p.rest # NIL DO
        p.first := SYMrip;
        p := p.rest
      END;

      p.first := SYMrip;

      p.rest := t.freePairs;
      t.freePairs := cons
    END
  END ReturnCons;
    
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
  SYMrip := SchemeSymbol.Symbol("####r.i.p.-dead-cons-cell####");

BEGIN 
END Scheme.



