(* $Id$ *)

MODULE Scheme;
IMPORT SchemeInputPort, SchemeEnvironment, SchemePrimitives, SchemePrimitive;
IMPORT SchemeBoolean, SchemeSymbol, SchemeMacro;
IMPORT Pathname, Stdio;
IMPORT Wr, TextRd;
IMPORT AL, FileRd, Rd, OSError, SchemeUtils;

REVEAL
  T = Public BRANDED Brand OBJECT
    input : SchemeInputPort.T;
    output : Wr.T;
    globalEnvironment : SchemeEnvironment.T;
  METHODS
    readInitialFiles(READONLY files : ARRAY OF Pathname.T) := ReadInitialFiles;
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

PROCEDURE Init(t : T; READONLY files : ARRAY OF Pathname.T) : T =
  BEGIN
    t.input := NEW(SchemeInputPort.T).init(Stdio.stdin);
    t.output := Stdio.stdout;
    t.globalEnvironment := NEW(SchemeEnvironment.T).initEmpty();
    EVAL SchemePrimitive.InstallPrimitives(t.globalEnvironment);
    t.readInitialFiles(files);
    RETURN t
  END Init;

PROCEDURE ReadInitialFiles(t : T; READONLY files : ARRAY OF Pathname.T) =
  BEGIN
    EVAL t.loadRd(NEW(TextRd.T).init(SchemePrimitives.Code));
    FOR i := FIRST(files) TO LAST(files) DO
      EVAL t.loadFile(files[i])
    END
  END ReadInitialFiles;

PROCEDURE DefineInGlobalEnv(t : T; var, val : Object) =
  BEGIN EVAL t.globalEnvironment.define(var,val) END DefineInGlobalEnv;

PROCEDURE ReadEvalWriteLoop(t : T) =
  BEGIN
    LOOP
      Wr.PutText(t.output, ">"); Wr.Flush(t.output);
      WITH x = t.input.read() DO
        IF SchemeInputPort.IsEOF(x) THEN RETURN END;

        EVAL SchemeUtils.Write(t.evalInGlobalEnv(x), t.output, TRUE);
        Wr.PutText(t.output, "\n"); Wr.Flush(t.output)
      END
    END
  END ReadEvalWriteLoop;

PROCEDURE LoadRd(t : T; rd : Rd.T) : Object =
  BEGIN RETURN t.loadPort(NEW(SchemeInputPort.T).init(rd)) END LoadRd;

PROCEDURE LoadFile(t : T; fileName : Object) : Object =
  BEGIN
    WITH name = SchemeUtils.Stringify(fileName) DO
      TRY
        RETURN t.loadRd(FileRd.Open(name))
      EXCEPT
        OSError.E(err) => RETURN SchemeUtils.Error("can't load " & name & " : OSError.E : " & AL.Format(err))
      |
        Rd.Failure(err) => RETURN SchemeUtils.Error("can't load " & name & " : Rd.Failure : " & AL.Format(err))
      END
    END
  END LoadFile;

PROCEDURE LoadPort(t : T; in : SchemeInputPort.T) : Object =
  BEGIN
    LOOP
      WITH x = in.read() DO
        IF SchemeInputPort.IsEOF(x) THEN RETURN SchemeBoolean.True() END;
        EVAL t.evalInGlobalEnv(x)
      END
    END
  END LoadPort;

PROCEDURE Eval(t : T; x : Object; env : SchemeEnvironment.T) : Object =
  TYPE  Macro = SchemeMacro.T;

  CONST First  = SchemeUtils.First;
        Second = SchemeUtils.Second;
        Third  = SchemeUtils.Second;
        Rest   = SchemeUtils.Rest;
        Cons   = SchemeUtils.Cons;
        SymEq  = SchemeSymbol.SymEq;

  BEGIN
    LOOP
      IF ISTYPE(x,Symbol) THEN
        RETURN env.lookup(x)
      ELSIF NOT ISTYPE(x,Pair) THEN 
        RETURN x
      ELSE
        WITH fn   = First(x), 
             args = Rest(x) DO
          IF    SymEq(fn, "quote") THEN
            RETURN First(args)
          ELSIF SymEq(fn, "begin") THEN
            WHILE Rest(args) # NIL DO
              EVAL t.eval(First(args),env);
              args := Rest(args)
            END;
            x := First(args)
          ELSIF SymEq(fn, "define") THEN
            IF ISTYPE(First(args), Pair) THEN
              RETURN env.define(First(First(args)),
                                t.eval(Cons(Symbol("lambda"),
                                            Cons(Rest(First(args)), 
                                                 Rest(args))), env))
            ELSE
              RETURN env.define(First(args),
                                t.eval(Second(args), env))
            END
          ELSIF SymEq(fn, "set!") THEN
            RETURN env.set(First(args), t.eval(Second(args), env))
          ELSIF SymEq(fn, "if") THEN
            IF Truth(t.eval(First(args), env)) THEN
              x := Second(args) 
            ELSE
              x := Third(args)
            END
          ELSIF SymEq(fn, "cond") THEN
            x := t.reduceCond(args, env)
          ELSIF SymEq(fn, "lambda") THEN
            RETURN NEW(SchemeClosure.T).init(First(args), 
                                             Rest(args),
                                             env)
          ELSIF SymEq(fn, "macro") THEN
            RETURN NEW(Macro).init(First(args),
                                   Rest(args),
                                   env)
          ELSE
            (* procedure call *)
            fn := t.eval(fn, env);
            
            TYPECASE fn OF
              Macro(m) => x := m.expand(t, x, args)
            |
              Closure(c) => x := c.body; 
              env := NEW(SchemeEnvironment.T).init(c.parms, 
                                                   t.evalList(args,env),
                                                   c.env)
            ELSE
              RETURN Procedure.Proc(fn).apply(t, t.evalList(args,env))
            END
          END
        END
      END
    END
  END Eval;

PROCEDURE EvalInGlobalEnv(t : T; x : Object) : Object =
  BEGIN RETURN t.eval(x, t.globalEnvironment) END EvalInGlobalEnv;

PROCEDURE EvalList(t : T; list : Object; env : SchemeEnvironment.T) : Pair =
  BEGIN
    TRY
      IF list = NIL THEN
        RETURN NIL
      ELSIF NOT ISTYPE(list, Pair) THEN
        RAISE E("Illegal arg list: " & SchemeUtils.DebugFormat(list));
        RETURN NIL
      ELSE
        RETURN SchemeUtils.Cons(t.eval(SchemeUtils.First(list), env), 
                                t.evalList(SchemeUtils.Rest(list), env))
      END
    EXCEPT
      E(ex) => 
        Wr.PutText(Stdio.stdout, "Scheme.evalList raising E, evaluating " &
          SchemeUtils.DebugFormat(list));
        RAISE E(ex)
    END
  END EvalList;

BEGIN END Scheme.



