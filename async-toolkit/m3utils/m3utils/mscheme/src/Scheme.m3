(* $Id$ *)

MODULE Scheme;
IMPORT SchemeInputPort, SchemeEnvironment, SchemePrimitives, SchemePrimitive;
IMPORT SchemeBoolean, SchemeSymbol, SchemeMacro;
IMPORT SchemeClosure, SchemeClosureClass, SchemeProcedure;
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
    reduceCond(clauses : Object; env : SchemeEnvironment.T) : Object := ReduceCond;
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
  TYPE  Macro     = SchemeMacro.T;
        Closure   = SchemeClosure.T;
        Procedure = SchemeProcedure.T;

  CONST First  = SchemeUtils.First;
        Second = SchemeUtils.Second;
        Third  = SchemeUtils.Second;
        Rest   = SchemeUtils.Rest;
        Cons   = SchemeUtils.Cons;
        SymEq  = SchemeSymbol.SymEq;
        Sym    = SchemeSymbol.Symbol;
        TruthO = SchemeBoolean.TruthO;

  BEGIN
    LOOP
      IF ISTYPE(x,Symbol) THEN
        RETURN env.lookup(x)
      ELSIF NOT ISTYPE(x,Pair) THEN 
        RETURN x
      ELSE
        VAR
          fn   := First(x);
          args := Rest(x); 
        BEGIN
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
                                t.eval(Cons(Sym("lambda"),
                                            Cons(Rest(First(args)), 
                                                 Rest(args))), env))
            ELSE
              RETURN env.define(First(args),
                                t.eval(Second(args), env))
            END
          ELSIF SymEq(fn, "set!") THEN
            RETURN env.set(First(args), t.eval(Second(args), env))
          ELSIF SymEq(fn, "if") THEN
            IF TruthO(t.eval(First(args), env))^ THEN
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

PROCEDURE EvalInGlobalEnv(t : T; x : Object) : Object =
  BEGIN RETURN t.eval(x, t.globalEnvironment) END EvalInGlobalEnv;

PROCEDURE EvalList(t : T; list : Object; env : SchemeEnvironment.T) : Pair =
  BEGIN
    TRY
      IF list = NIL THEN
        RETURN NIL
      ELSIF NOT ISTYPE(list, Pair) THEN
        RAISE E("Illegal arg list: " & SchemeUtils.DebugFormat(list));
        (* notreached *)
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

PROCEDURE ReduceCond(t : T; 
                     clauses : Object; env : SchemeEnvironment.T) : Object =

  CONST First  = SchemeUtils.First;
        Second = SchemeUtils.Second;
        Third  = SchemeUtils.Second;
        Rest   = SchemeUtils.Rest;
        Cons   = SchemeUtils.Cons;
        List2  = SchemeUtils.List2;
        SymEq  = SchemeSymbol.SymEq;
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
          IF SymEq(First(clause),"else") THEN
            success := TRUE
          ELSE 
            result := t.eval(First(clause),env);
            (* is this a bug? we overwrite result even if we don't succeed,
               Norvig's SILK does this too... *)
            IF TruthO(result)^ THEN success := TRUE END; 
          END;

          IF success THEN
            IF Rest(clause) = NIL THEN
              RETURN List2(Sym("quote"),result)
            ELSIF SymEq(Second(clause),"=>") THEN
              RETURN List2(Third(clause),List2(Sym("quote"),result))
            ELSE 
              RETURN Cons(Sym("begin"), Rest(clause))
            END
          END
        END
      END
    END
  END ReduceCond;

BEGIN END Scheme.



