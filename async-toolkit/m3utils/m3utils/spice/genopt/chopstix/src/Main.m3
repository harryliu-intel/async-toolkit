MODULE Main EXPORTS Main, QuadOpt;

(* 
   Generic optimizer
*)

IMPORT ParseParams;
IMPORT Debug;
IMPORT Pathname;
IMPORT Stdio;
FROM Fmt IMPORT F, LongReal, Int, Pad, Bool;
IMPORT Params;
IMPORT TextSeq;
IMPORT Thread;
IMPORT OSError;
IMPORT Scheme, SchemeM3;
IMPORT FS;
FROM SchemaGraph IMPORT ReadSchema, ReadData, EvalFormulas;
IMPORT SchemaGraph;
IMPORT Wr;
IMPORT SchemeStubs;
IMPORT ReadLine, SchemeReadLine;
IMPORT NewUOAs;
IMPORT LRVector;
IMPORT ProcUtils;
IMPORT Rd;
IMPORT TextWr;
IMPORT FileWr;
IMPORT SchemeSymbol;
IMPORT Process;
IMPORT AL;
IMPORT SchemeObject;
IMPORT SchemeUtils;
IMPORT SchemeLongReal;
IMPORT SchemeEnvironment;
IMPORT SchemeString;
IMPORT QuadRobust;
IMPORT LRVectorPair;
IMPORT LRVectorLRPair;
IMPORT LRVectorLRPairTextTbl;
IMPORT FileRd;
IMPORT Scan;
IMPORT FloatMode, Lex;
IMPORT SchemePair;
IMPORT MultiEvalLRVector;
IMPORT Word;
IMPORT IP, NetObj, ReadLineError; (* for exceptions *)
FROM GenOptUtils IMPORT FmtP;
FROM GenOpt IMPORT ResultWriter, rho, scmCb, doNetbatch,
                   schemaDataFn, schemaPath, outOfDomainResult;
FROM GenOpt IMPORT p, rhoEnd, rhoBeg, vseq, paramBindings;
IMPORT GenOpt;
FROM GenOptEnv IMPORT   NbPool, NbQslot, M3Utils, NbOpts;
FROM GenOptUtils IMPORT MustOpenWr, LRVectorSeq1, FmtLRVectorSeq;
FROM NewUOAs IMPORT Output;
IMPORT ModelVar;
IMPORT LRVectorSeq;
IMPORT LRVectorPairTextTbl;
IMPORT LRVectorField;
IMPORT LRVectorFieldPll;
IMPORT MELRVectorType;
IMPORT LRMatrix2;
IMPORT RandomVector;
FROM VectorUtils IMPORT Orthogonalize;
IMPORT Random;
IMPORT SymbolLRTbl;
IMPORT StatObject;
IMPORT LRScalarField;
IMPORT LineMinimizer;
FROM QuadRobust IMPORT schemeMu;

<*FATAL Thread.Alerted*>

VAR
  doDebug := Debug.DebugThis("chopstix");
  
CONST
  MyM3UtilsSrcPath = "spice/genopt/chopstix/src";

  

VAR
  pMu := NEW(MUTEX);

  rand := NEW(Random.Default).init();

TYPE
  Evaluator = LRVectorFieldPll.T OBJECT
    optVars, paramVars : SchemeObject.T;

    results            : LRVectorLRPairTextTbl.T;
    resultsMu          : MUTEX;

  METHODS
    init(    optVars, paramVars : SchemeObject.T) : LRVectorField.T := InitPll;

    multiEval(at : LRVector.T; samples : CARDINAL; VAR schemaScm : Scheme.T) : MultiEvalLRVector.Result :=
        BaseMultiEval;

    nominalEval(at : LRVector.T; VAR schemaScm : Scheme.T) : LRVector.T :=
        BaseNominalEval;
  OVERRIDES
    eval     := BaseEval;
    evalHint := BaseEvalHint;
  END;

PROCEDURE InitPll(pll                : Evaluator;
                  optVars, paramVars : SchemeObject.T) : LRVectorField.T =
  BEGIN
    pll.optVars   := optVars;
    pll.paramVars := paramVars;
    pll.results   := NEW(LRVectorLRPairTextTbl.Default).init();
    pll.resultsMu := NEW(MUTEX);
    RETURN LRVectorFieldPll.T.init(pll, pll)
  END InitPll;

VAR toEval : SchemeObject.T := NIL;
  
PROCEDURE DefEval(obj : SchemeObject.T) =
  BEGIN
    toEval := obj
  END DefEval;

PROCEDURE OptInit() =
  BEGIN
  END OptInit;
  
PROCEDURE BaseEvalHint(base : Evaluator; p : LRVector.T) =
  BEGIN
    EVAL base.eval(p)
  END BaseEvalHint;

PROCEDURE BaseEval(base : Evaluator; p : LRVector.T) : LRVector.T =
  BEGIN
    TRY
      WITH res = AttemptEval(base, p, 0, FALSE) DO
        TYPECASE res OF
          LRVectorResult(lrv) => RETURN lrv.res
        ELSE
          <*ASSERT FALSE*>
        END
      END
    EXCEPT
    ProcUtils.ErrorExit =>
      Debug.Error("BaseEval : Too many attempts p=" & FmtP(p));
      <*ASSERT FALSE*>
    END
  END BaseEval;

PROCEDURE BaseMultiEval(base    : Evaluator;
                        p       : LRVector.T;
                        samples : CARDINAL;
                        VAR schemaScm : Scheme.T) : MultiEvalLRVector.Result =
  BEGIN
    IF doDebug THEN
      Debug.Out("BaseMultiEval : samples " & Int(samples), 100)
    END;
    
    TRY
      WITH res = AttemptEval(base, p, samples, FALSE) DO
        TYPECASE res OF
          QuadResult(quad) =>
          <*ASSERT quad.res.sum # NIL*>
          <*ASSERT quad.res.sumsq # NIL*>
          schemaScm := res.schemaScm;
          RETURN quad.res
        ELSE
          <*ASSERT FALSE*>
        END
      END
    EXCEPT
    ProcUtils.ErrorExit =>
      Debug.Error("BaseMultiEval : Too many attempts p=" & FmtP(p));
      <*ASSERT FALSE*>
    END
  END BaseMultiEval;

PROCEDURE BaseNominalEval(base    : Evaluator;
                          p       : LRVector.T;
                          VAR schemaScm : Scheme.T) : LRVector.T = 
  BEGIN
    IF doDebug THEN
      Debug.Out("BaseNominalEval : " & FmtP(p))
    END;
    
    TRY
      WITH res = AttemptEval(base, p, 1, nominal := TRUE) DO
        TYPECASE res OF
          LRVectorResult(lrv) =>
          schemaScm := res.schemaScm;
          RETURN lrv.res
        ELSE
          <*ASSERT FALSE*>
        END
      END
    EXCEPT
    ProcUtils.ErrorExit =>
      Debug.Error("BaseMultiEval : Too many attempts p=" & FmtP(p));
      <*ASSERT FALSE*>
    END
  END BaseNominalEval;

TYPE
  EvalResult     = BRANDED OBJECT
    schemaScm : Scheme.T;
  END;

  LRVectorResult = EvalResult OBJECT res : LRVector.T END;

  QuadResult     = EvalResult OBJECT res : MultiEvalLRVector.Result END;
  
PROCEDURE AttemptEval(base                 : Evaluator;
                      q                    : LRVector.T;
                      samples              : CARDINAL;
                      nominal              : BOOLEAN) : EvalResult
  RAISES { ProcUtils.ErrorExit } =

  PROCEDURE SetNbOpts() =
    BEGIN
      IF NbOpts # NIL THEN
        nbopts := NbOpts
      ELSE
        nbopts := F("--target %s --class 4C --class SLES12", NbPool);
      END
    END SetNbOpts;

  PROCEDURE CopyVectorToScheme() =
    BEGIN
      IF doDebug THEN
        Debug.Out(F("CopyVectorToScheme : p=%s samples=%s nominal=%s",
                    FmtP(q),
                    Int(samples),
                    Bool(nominal)))
      END;
      
      LOCK pMu DO

        (* 
           only one copy can be in this section at any given time, since
           pMu is global 
        *)
        
        FOR i := FIRST(q^) TO LAST(q^) DO
          p.put(i, q[i])
        END;
        IF nominal THEN 
          cmd            := scmCb.command(-1)
        ELSE
          cmd            := scmCb.command(samples)
        END
      END
    END CopyVectorToScheme;

  PROCEDURE SetupDirectory() =
    BEGIN
      TRY
        TRY
          FS.CreateDirectory(subdirPath);
        EXCEPT
          OSError.E(x) =>
          IF doDirectoryWarning THEN
            Debug.Warning(F("creating directory \"%s\" : OSError.E : %s",
                            subdirPath,
                            AL.Format(x)))
          END
        END;
        WITH wr   = MustOpenWr(subdirPath & "/opt.values") DO
          FOR i := FIRST(q^) TO LAST(q^) DO
            Wr.PutText(wr, LongReal(q[i]));
            VAR c : CHAR; BEGIN
              IF i = LAST(q^) THEN c := '\n' ELSE c := ',' END;
              Wr.PutChar(wr, c);
            END
          END;
          Wr.Close(wr);
        END;
        
        WITH cmdDbgWr    = MustOpenWr(subdirPath & "/opt.cmd"),
             runCmdDbgWr = MustOpenWr(subdirPath & "/opt.runcmd"),
             
             nbCmd = F("nbjob run %s --mode interactive %s",
                       nbopts,
                       cmd) DO
          VAR
            runCmd : TEXT;
          BEGIN
            IF doNetbatch THEN
              runCmd := nbCmd
            ELSE
              runCmd := cmd
            END;
            
            Wr.PutText(cmdDbgWr, cmd);
            Wr.PutChar(cmdDbgWr, '\n');
            Wr.PutText(runCmdDbgWr, runCmd);
            Wr.PutChar(runCmdDbgWr, '\n');
            Wr.Close(cmdDbgWr); Wr.Close(runCmdDbgWr);
            
            cm             := ProcUtils.RunText(runCmd,
                                                stdout := stdout,
                                                stderr := stderr,
                                                stdin  := NIL,
                                                wd0    := subdirPath)
          END
        END
      EXCEPT
        Wr.Failure(x) =>
        Debug.Error(F("I/O error : Wr.Failure : while setting up %s : %s:",
                      subdirPath, AL.Format(x)))
      END
    END SetupDirectory;

  PROCEDURE RunCommand(VAR(*OUT*) schemaScm : Scheme.T) : LRVectorSeq.T 
    (* schemaScm is newly allocated *)
    RAISES { ProcUtils.ErrorExit } =
    BEGIN
      IF doDebug THEN
        Debug.Out(F("BaseEval : q = %s\nrunning : %s", FmtP(q), cmd));
        Debug.Out("subdirPath = " & subdirPath)
      END;
      
      cm.wait();

      IF doDebug THEN
        Debug.Out("BaseEval : ran command")
      END;
      
      (* read the schema *)
      LOCK schemeMu DO
      WITH dataPath  = subdirPath & "/" & schemaDataFn,
           schemaRes = SchemaReadResult(schemaPath,
                                        dataPath,
                                        scmFiles,
                                        MakeMultiEval(),
                                        base.optVars,
                                        base.paramVars,
                                        schemaScm) DO
        IF doDebug THEN
          Debug.Out("Schema-processed result : " & FmtLRVectorSeq(schemaRes))
        END;
        RETURN schemaRes
      END
      END
    END RunCommand;

  VAR
    schemaScm  : Scheme.T;
    Owr                  := NEW(TextWr.T).init();
    Ewr                  := NEW(TextWr.T).init();
    <*NOWARN*>stdout     := ProcUtils.WriteHere(Owr);
    <*NOWARN*>stderr     := ProcUtils.WriteHere(Ewr);

    sdIdx                := NextIdx();
    subdirPath           := F("%s/%s",
                              rundirPath,
                              Pad(Int(sdIdx), 6, padChar := '0'));

    nbopts     : TEXT;
    cmd        : TEXT;
    cm         : ProcUtils.Completion;

    theResult  : LRVectorSeq.T;
  BEGIN
    IF nominal THEN <*ASSERT samples=1*> END;
    
    SetNbOpts();
    CopyVectorToScheme();
    SetupDirectory();
    
    TRY
      TRY
        TRY
          theResult := RunCommand(schemaScm);
          WITH wr = MustOpenWr(subdirPath & "/opt.ok") DO
            Wr.Close(wr)
          END
        EXCEPT
          ProcUtils.ErrorExit(err) =>
          WITH msg = F("command \"%s\" \nraised ErrorExit : %s",
                       cmd,
                       
                       ProcUtils.FormatError(err)) DO
            
            WITH wr = MustOpenWr(subdirPath & "/opt.error") DO
              Wr.PutText(wr, ProcUtils.FormatError(err));
              Wr.PutChar(wr, '\n');
              Wr.Close(wr)
            END;
            
            Debug.Warning(msg);
            IF outOfDomainResult = FIRST(LONGREAL) THEN
              RAISE ProcUtils.ErrorExit(err)
            ELSE
              Debug.Out("Returning outOfDomainResult : " & LongReal(outOfDomainResult));
              theResult := LRVectorSeq1(outOfDomainResult,
                                        QuadRobust.GetOptVars().size())
            END
          END
        END
      EXCEPT
        Wr.Failure(x) =>
        Debug.Error(F("I/O error : Wr.Failure : while running in %s : %s:",
                      subdirPath, AL.Format(x)));
        <*ASSERT FALSE*>
      END
    FINALLY
      TRY
        WITH errWr = MustOpenWr(subdirPath & "/opt.stderr"),
             outWr = MustOpenWr(subdirPath & "/opt.stdout"),
             resWr = MustOpenWr(subdirPath & "/opt.result") DO
          Wr.PutText(errWr, TextWr.ToText(Ewr));
          Wr.PutText(outWr, TextWr.ToText(Owr));
          Wr.PutText(resWr, FmtLRVectorSeq(theResult) & "\n");
          Wr.Close(errWr);
          Wr.Close(outWr);
          Wr.Close(resWr)
        END;
        IF samples = 0 OR nominal THEN
          <*ASSERT theResult.size() = 1 *>
          RETURN NEW(LRVectorResult,
                     res       := theResult.get(0),
                     schemaScm := schemaScm)
        ELSE
          RETURN NEW(QuadResult,
                     res       := VectorSeqToMulti(theResult),
                     schemaScm := schemaScm)
        END
      EXCEPT
        Wr.Failure(x) =>
        Debug.Error(F("I/O error : Wr.Failure : while writing output in %s : %s:",
                      subdirPath, AL.Format(x)));
        <*ASSERT FALSE*>
        
      END
    END
  END AttemptEval;

VAR
  idMu          := NEW(MUTEX);
  idNx : Word.T := 0;

PROCEDURE NewZeroV(n : CARDINAL) : LRVector.T =
  VAR
    z := NEW(LRVector.T, n);
  BEGIN
    FOR i := FIRST(z^) TO LAST(z^) DO
      z[i] := 0.0d0
    END;
    RETURN z
  END NewZeroV;
  
PROCEDURE VectorSeqToMulti(seq : LRVectorSeq.T) : MultiEvalLRVector.Result =
  VAR
    dims := NUMBER(seq.get(0)^); 
    s, ss : LRVector.T;
    n    := seq.size();
    res : MultiEvalLRVector.Result;
  BEGIN
    FOR d := 0 TO dims - 1 DO
      s  := NewZeroV(dims);
      ss := NewZeroV(dims);
      FOR i := 0 TO seq.size() - 1 DO
        WITH x = seq.get(i) DO
          LRMatrix2.AddV(s^, x^, s^);
          LRMatrix2.AddV(ss^, MELRVectorType.Times(x, x)^, ss^)
        END
      END;
      
    END;

    LOCK idMu DO
      TRY
        <*ASSERT s # NIL*>
        <*ASSERT ss # NIL*>
        res :=  MultiEvalLRVector.Result { id    := idNx,
                                           n     := n,
                                           sum   := s,
                                           sumsq := ss,
                                           extra := NIL }
      FINALLY
        INC(idNx)
      END
    END;
    
    RETURN res
  END VectorSeqToMulti;

TYPE
  MyMultiEval = MultiEvalLRVector.T OBJECT
    base : Evaluator;
  OVERRIDES
    multiEval   := DoMultiEval;
    nominalEval := DoNominalEval;
  END;

PROCEDURE DoMultiEval(mme     : MyMultiEval;
                      at      : LRVector.T;
                      samples : CARDINAL) : MultiEvalLRVector.Result =
  VAR
    scm : Scheme.T;
    res := mme.base.multiEval(at, samples, scm);
  BEGIN
    res.extra := scm;
    RETURN res
  END DoMultiEval;

PROCEDURE DoNominalEval(mme : MyMultiEval;
                        at  : LRVector.T) : LRVector.T =
  VAR
    scm : Scheme.T;
    res := mme.base.nominalEval(at, scm);
  BEGIN
    RETURN res
  END DoNominalEval;
  
PROCEDURE MakeMultiEval() : SchemeObject.T =
  VAR 
    res : SchemeObject.T := NIL;
  BEGIN
    WITH optvars = QuadRobust.GetOptVars() DO
      FOR i := optvars.size() - 1 TO 0 BY -1 DO
        WITH v = optvars.get(i) DO
          res := SchemeUtils.Cons(v.nm, res)
        END
      END
    END;
    RETURN SchemeUtils.Cons(SchemeSymbol.FromText("list"), res)
   END MakeMultiEval;

PROCEDURE DoIt(optVars, paramVars : SchemeObject.T) =
  VAR
    N               := vseq.size();
    pr : LRVector.T := NEW(LRVector.T, N);
    evaluator       : Evaluator
                        := NEW(Evaluator).init(optVars, paramVars);
    output : NewUOAs.Output;
  BEGIN
    IF rhoEnd = LAST(LONGREAL) THEN
      rhoEnd := rhoBeg / 1.0d-6
    END;

    FOR i := FIRST(pr^) TO LAST(pr^) DO
      WITH v = vseq.get(i) DO
        pr[i] := v.defval / v.defstep
      END
    END;

    (**************************************************)

    Debug.Out(F("DoIt : Ready to call Minimize : pr=%s rhoBeg=%s rhoEnd=%s",
                FmtP(pr), LongReal(rhoBeg), LongReal(rhoEnd)));

    output := QuadRobust.Minimize(pr,
                                  NEW(MyMultiEval, base := evaluator).init(evaluator),
                                  toEval,
                                  rhoBeg,
                                  rhoEnd,
                                  NEW(MyResultWriter,
                                      evaluator := evaluator,
                                      root      := "progress"));
    
    TRY
      Debug.Out(F("Minimize : %s iters, %s funcc; f = %s; %s",
                  Int(output.iterations),
                  Int(output.funcCount),
                  LongReal(output.f),
                  output.message));
      Wr.PutText(Stdio.stdout, FmtP(output.x));
      Wr.PutChar(Stdio.stdout, '\n');
      Wr.Flush(Stdio.stdout);
      
      WITH writer = NEW(MyResultWriter,
                        root        := "genopt",
                        evaluator   := evaluator) DO
        writer.write(output)
      END
    EXCEPT
      Wr.Failure(x) =>
        Debug.Error(F("I/O error : Wr.Failure : while writing optimization result : %s:",
                      AL.Format(x)))

    END
  END DoIt;

TYPE
  MyResultWriter = ResultWriter OBJECT
    root        : Pathname.T;
    evaluator   : Evaluator;
  OVERRIDES
    write := MRWWrite;
  END;
  
PROCEDURE MRWWrite(mrw : MyResultWriter; output : NewUOAs.Output)
  RAISES { OSError.E, Wr.Failure } =
  BEGIN
    WITH wr   = FileWr.Open(mrw.root & ".opt"),
         cwr  = FileWr.Open(mrw.root & ".cols"),
         
         awr  = FileWr.Open(mrw.root & ".aopt"),
         acwr = FileWr.Open(mrw.root & ".acols"),
         
         rwr  = FileWr.Open(mrw.root & ".result"),
         rhwr = FileWr.Open(mrw.root & ".rho") DO
      FOR i := vseq.size() - 1 TO 0 BY -1 DO
        WITH v  = vseq.get(i),
             xi = output.x[i],
             pi = xi * v.defstep DO
          Wr.PutText(wr, LongReal(pi));
          Wr.PutText(cwr, v.nm);
          Wr.PutText(awr, LongReal(pi));
          Wr.PutText(acwr, v.nm);
          VAR c : CHAR; BEGIN
            IF i = 0 THEN c := '\n' ELSE
              Wr.PutChar(awr, ',');
              Wr.PutChar(acwr, ',');
              c := ','
            END;
            Wr.PutChar(wr, c);
            Wr.PutChar(cwr, c)
          END
        END
      END;

      VAR
        pIter := paramBindings.iterate();
        p, v : TEXT;
      BEGIN
        WHILE pIter.next(p, v) DO
          Wr.PutChar(awr, ',');
          Wr.PutChar(acwr, ',');
          Wr.PutText(awr, v);
          Wr.PutText(acwr, p)
        END
      END;

      Wr.PutText(rwr, LongReal(output.f) & "\n");
      Wr.PutText(awr, "," & LongReal(output.f));
      Wr.PutText(acwr, ",RESULT\n");

      Wr.PutText(rhwr, LongReal(output.stoprho) & "\n");

      VAR
        key := LRVectorLRPair.T { output.x, output.f };
        subdir : TEXT;
        haveIt : BOOLEAN;
      BEGIN
        LOCK mrw.evaluator.resultsMu DO
          haveIt := mrw.evaluator.results.get(key, subdir)
        END;
        IF haveIt THEN
          TRY
            WITH rd = FileRd.Open(subdir & "/" & schemaDataFn),
                 line = Rd.GetLine(rd) DO
              Wr.PutChar(awr, ',');
              Wr.PutText(awr, subdir);
              Wr.PutChar(awr, ',');
              Wr.PutText(awr, line);
              Wr.PutChar(awr, '\n');
              Rd.Close(rd)
            END
          EXCEPT
            OSError.E, Rd.Failure =>
            Debug.Warning("System error trying to read base result from " & subdir);
            Wr.PutChar(awr, '\n')
          END
        ELSE
          Debug.Warning("No base result for key.");
          Wr.PutChar(awr, '\n')
        END
      END;
      
      Wr.Close(wr); Wr.Close(cwr); Wr.Close(rwr); Wr.Close(rhwr);
      Wr.Close(awr); Wr.Close(acwr)
    END
  END MRWWrite;

VAR
  sdcnt        := 0;
  mu           := NEW(MUTEX);

PROCEDURE NextIdx() : CARDINAL =
  BEGIN
    LOCK mu DO
      TRY
        RETURN sdcnt
      FINALLY
        INC(sdcnt)
      END
    END
  END NextIdx;

VAR
  optVarsNm   := T2S("*opt-vars*");
  paramVarsNm := T2S("*param-vars*");
     

PROCEDURE BindParams(schemaScm : Scheme.T;
                     optVars, paramVars : SchemeObject.T) =
  BEGIN
    TRY
      FOR i := 0 TO vseq.size() - 1 DO
        WITH v  = vseq.get(i),
             nm = v.nm,
             ss = T2S(nm),
             pi = p.get(i),
             vv = pi * v.defstep,
             sx = L2S(vv) DO
          schemaScm.bind(ss, sx)
        END;

        VAR
          p : SchemePair.T := paramVars;
        BEGIN
          WHILE p # NIL DO
            WITH q = NARROW(p.first, SchemePair.T),
                 r = NARROW(q.rest , SchemePair.T) DO
              schemaScm.bind(q.first, r.first)
            END;
            p := p.rest
          END
        END;

        VAR
          iter := paramBindings.iterate();
          k, v : TEXT;
        BEGIN
          WHILE iter.next(k, v) DO
            WITH ss = T2S(k) DO
              TRY
                WITH lr = Scan.LongReal(v),
                     sx = L2S(lr) DO
                  schemaScm.bind(ss, sx)
                END
              EXCEPT
                Lex.Error, FloatMode.Trap =>
                schemaScm.bind(ss, SchemeString.FromText(v))
              END
            END
          END
        END;
        
        schemaScm.bind(T2S("*opt-rho*"), L2S(rho))
      END;
      
      schemaScm.setInGlobalEnv(optVarsNm, optVars);
      schemaScm.setInGlobalEnv(paramVarsNm, paramVars)
    EXCEPT
      Scheme.E(x) =>
      Debug.Error(F("EXCEPTION! Scheme.E \"%s\" when initializing interpreter variables", x))
    END;
  END BindParams;
  
PROCEDURE SchemaReadResult(schemaPath ,
                           dataPath                       : Pathname.T;
                           scmFiles                       : TextSeq.T;
                           schemaEval, optVars, paramVars : SchemeObject.T;
                           VAR (*OUT*) schemaScm : Scheme.T) : LRVectorSeq.T =

  (* schemaScm is newly allocated and in a state where the various
     parameter and optimization variables have been set to the correct values *)

  VAR
    dataFiles := NEW(TextSeq.T).init();
  BEGIN
    IF doDebug THEN
      Debug.Out(F("SchemaReadResult : schemaPath %s , dataPath %s , eval %s",
                  schemaPath, dataPath, SchemeUtils.Stringify(schemaEval)))
    END;
    
    dataFiles.addhi(dataPath);
    
    WITH scmarr = NEW(REF ARRAY OF Pathname.T, scmFiles.size()) DO
      FOR i := FIRST(scmarr^) TO LAST(scmarr^) DO
        scmarr[i] := scmFiles.get(i);
        IF doDebug THEN
          Debug.Out("SchemaReadResult: loading scheme file " & scmarr[i])
        END;
      END;
      TRY
        schemaScm := NEW(SchemeM3.T).init(scmarr^)
      EXCEPT
        Scheme.E(x) =>
        Debug.Error(F("EXCEPTION! Scheme.E \"%s\" when initializing interpreter", x))
      END
    END;

    BindParams(schemaScm, optVars, paramVars);

    IF doDebug THEN
      Debug.Out("SchemaReadResult : set up interpreter")
    END;
    
    TRY
      WITH schema = ReadSchema(schemaPath),
           data   = ReadData(schema, dataFiles),
           cb     = NEW(SGCallback,
                        schemaScm := schemaScm,
                        results   := NEW(LRVectorSeq.T).init()) DO
        IF doDebug THEN
          Debug.Out("SchemaReadResult : schema and data loaded")
        END;

        (* there is where the schema formulas are evaluated
           and we haven't yet loaded the parameters! *)
        EvalFormulas(schemaScm, schema, data, cb);

        IF doDebug THEN
          Debug.Out("SchemaReadResult : formulas evaluated data.size()=" &
            Int(data.size()))
        END;

        RETURN cb.results
      END
    EXCEPT
      OSError.E, Rd.Failure =>
      Debug.Warning("Caught system error reading schema/data.  Returning failure.");
      RETURN LRVectorSeq1(outOfDomainResult,
                                        QuadRobust.GetOptVars().size())
    |
      Scheme.E(x) =>
      Debug.Warning("?error in Scheme interpreter : " & x);
      RETURN LRVectorSeq1(outOfDomainResult,
                          QuadRobust.GetOptVars().size())
      (* is this right? *)
    END
  END SchemaReadResult;

TYPE
  SGCallback = SchemaGraph.Callback OBJECT
    schemaScm : Scheme.T;
    results   : LRVectorSeq.T;
  OVERRIDES
    next := SGCNext;
  END;

PROCEDURE SGCNext(cb : SGCallback) =
  BEGIN
    TRY
      WITH e = NARROW(cb.schemaScm.getGlobalEnvironment(), SchemeEnvironment.T) DO
        <*ASSERT e # NIL*>
        <*ASSERT e.lookup(T2S("eval-in-env")) # NIL *>
      END;
      WITH scmCode = SchemeUtils.List3(
                         T2S("eval-in-env"),
                         L2S(0.0d0),
                         SchemeUtils.List2(T2S("quote"), MakeMultiEval())),
           (* do we need to copy() the Scheme here? -- seems we don't 
              actually modify it? *)
           scm = cb.schemaScm,
           ee  = NARROW(scm.getGlobalEnvironment(), SchemeEnvironment.T) DO

        <*ASSERT ee # NIL*>
        <*ASSERT ee.lookup(T2S("eval-in-env")) # NIL *>
       
        WITH schemaRes = scm.evalInGlobalEnv(scmCode) DO
          IF doDebug THEN
            Debug.Out("schema eval returned " & SchemeUtils.Stringify(schemaRes))
          END;
          cb.results.addhi( LRVectorFromO(schemaRes) )
        END
      END
    EXCEPT
      Scheme.E(err) =>
      Debug.Error("SGCNext : caught Scheme.E : " & err)
    END
  END SGCNext;

PROCEDURE LRVectorFromO(o : SchemePair.T) : LRVector.T =
  VAR
    n := SchemeUtils.Length(o);
    v := NEW(LRVector.T, n);
    p : SchemePair.T := o;
    i := 0;
  BEGIN
    WHILE p # NIL DO
      v[i] := SchemeLongReal.FromO(p.first);
      INC(i);
      p := p.rest
    END;
    RETURN v
  END LRVectorFromO;
  
CONST T2S = SchemeSymbol.FromText;
      L2S = SchemeLongReal.FromLR;
             
VAR
  pp                        := NEW(ParseParams.T).init(Stdio.stderr);
  scm                : Scheme.T;
  rundirPath                := Process.GetWorkingDirectory();
  myFullSrcPath      : Pathname.T;
  scmFiles                  := NEW(TextSeq.T).init();
  interactive        : BOOLEAN;
  cfgFile            : Pathname.T;
  genOptScm          : Pathname.T;
  doDirectoryWarning : BOOLEAN;
  
BEGIN

  GenOpt.DoIt := DoIt;

  IF FALSE THEN
    (*TestQf.DoIt();*)
    
    Process.Exit(0)
  END;
  
  Debug.SetOptions(SET OF Debug.Options { Debug.Options.PrintThreadID } );
  scmFiles.addhi("require");
  scmFiles.addhi("m3");

  TRY
    interactive := pp.keywordPresent("-i") OR pp.keywordPresent("-interactive");

    doDirectoryWarning := pp.keywordPresent("-warndirexists");
    
    IF NbPool = NIL THEN
      Debug.Error("Must set NBPOOL env var.")
    END;
    IF NbQslot = NIL THEN
      Debug.Error("Must set NBQSLOT env var.")
    END;
    IF pp.keywordPresent("-m3utils") THEN
      M3Utils                   := pp.getNext()
    END;

    IF M3Utils = NIL THEN
      Debug.Error("? must specify M3UTILS---either on command line or in environment.")
    END;

    myFullSrcPath := M3Utils & "/" & MyM3UtilsSrcPath;
    WITH commonScm = myFullSrcPath & "/common.scm",
         genOptDefsScm = myFullSrcPath & "/genoptdefs.scm" DO
      scmFiles.addhi(commonScm);
      scmFiles.addhi(genOptDefsScm)
    END;

    (* schemes up till here are added to both versions of the scheme
       enviornment.  genOptScm is only added to the scheme environment
       used to interpret the configuration file (and not to the schema
       evaluator.) *)
    
    genOptScm := myFullSrcPath & "/genopt.scm";
    
    WHILE pp.keywordPresent("-scminit") OR pp.keywordPresent("-S") OR pp.keywordPresent("-scm") DO
      scmFiles.addhi(pp.getNext())
    END;

    WHILE pp.keywordPresent("-setparam") DO
      WITH pnm  = pp.getNext(),
           pval = pp.getNext() DO
        Debug.Out(F("Overriding param %s <- %s", pnm, pval));
        EVAL paramBindings.put(pnm, pval)
      END
    END;

    pp.skipParsed();

    cfgFile := pp.getNext();

    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & "\n" )
  END;

  WITH scmarr = NEW(REF ARRAY OF Pathname.T, scmFiles.size() + 2) DO
    FOR i := FIRST(scmarr^) TO LAST(scmarr^) - 2 DO
      scmarr[i] := scmFiles.get(i)
    END;
    scmarr[LAST(scmarr^) - 1] := genOptScm;
    scmarr[LAST(scmarr^)] := cfgFile;
    SchemeStubs.RegisterStubs();
    TRY
      scm := NEW(SchemeM3.T).init(scmarr^)
    EXCEPT
      Scheme.E(x) =>
      Debug.Error(F("EXCEPTION! Scheme.E \"%s\" when initializing interpreter", x))
    END
  END;

  IF interactive THEN
    <*FATAL IP.Error, NetObj.Error, ReadLineError.E*>
    BEGIN
      SchemeReadLine.MainLoop(NEW(ReadLine.Default).init(), scm)
    END
  ELSE
    WITH senv      = NARROW(scm.getGlobalEnvironment(), SchemeEnvironment.T),
         T2S       = SchemeSymbol.FromText,
         optVars   = senv.lookup(T2S("*opt-vars*")),
         paramVars = senv.lookup(T2S("*param-vars*")) DO
      DoIt(optVars, paramVars)
    END
  END

END Main.
