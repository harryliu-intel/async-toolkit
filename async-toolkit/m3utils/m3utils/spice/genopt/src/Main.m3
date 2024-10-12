MODULE Main EXPORTS Main, GenOpt;

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
IMPORT Env;
IMPORT SchemeStubs;
IMPORT ReadLine, SchemeReadLine;
IMPORT NewUOAs;
IMPORT LRScalarField;
IMPORT LRScalarFieldPll;
IMPORT LRVector;
IMPORT ProcUtils;
IMPORT Rd;
IMPORT TextWr;
IMPORT FileWr;
IMPORT OptVar;
IMPORT OptVarSeq;
IMPORT Wx;
IMPORT SchemeSymbol;
IMPORT LongRealSeq AS LRSeq;
IMPORT OptCallback;
IMPORT Process;
IMPORT AL;
IMPORT SchemeObject;
IMPORT SchemeUtils;
IMPORT SchemeLongReal;
IMPORT TextTextTbl;
IMPORT SchemeEnvironment;
IMPORT SchemeString;
IMPORT SchemeBoolean;
IMPORT Robust;
IMPORT StocRobust;
IMPORT LRVectorLRPair;
IMPORT LRVectorLRPairTextTbl;
IMPORT FileRd;
IMPORT Scan;
IMPORT FloatMode, Lex;
IMPORT SchemePair;
IMPORT MultiEval;
IMPORT Word;
IMPORT IP, NetObj, ReadLineError; (* for exceptions *)
FROM GenOptUtils IMPORT FmtP;

<*FATAL Thread.Alerted*>

VAR
  NbPool  := Env.Get("NBPOOL");
  NbQslot := Env.Get("NBQSLOT");
  M3Utils := Env.Get("M3UTILS");
  NbOpts  := Env.Get("NBRUNOPTIONS");

  doDebug := Debug.DebugThis("genopt");
  
CONST
  MyM3UtilsSrcPath = "spice/genopt/src";

VAR
  paramBindings := NEW(TextTextTbl.Default).init();

PROCEDURE GetParamBindings() : TextTextTbl.T =
  BEGIN RETURN paramBindings END GetParamBindings;

PROCEDURE GetParam(named : SchemeSymbol.T) : SchemeObject.T =
  (* returns #f if not defined; returns string if defined *)
  VAR
    val : TEXT;
  BEGIN
    WITH tnm    = SchemeSymbol.ToText(named),
         haveIt = paramBindings.get(tnm, val) DO
      IF haveIt THEN
        RETURN SchemeString.FromText(val)
      ELSE
        RETURN SchemeBoolean.False()
      END
    END
  END GetParam;

VAR
  vseq           : OptVarSeq.T;
  rhoBeg, rhoEnd : LONGREAL;
  scmCb          : OptCallback.T;
  schemaPath     : Pathname.T;
  schemaScmPaths : TextSeq.T;
  schemaDataFn   : Pathname.T;
  schemaEval     : SchemeObject.T;
  outOfDomainResult        := FIRST(LONGREAL);
  method                   := Method.NewUOAs;

PROCEDURE SetMethod(m : Method) = BEGIN method := m END SetMethod;

PROCEDURE GetMethod() : Method = BEGIN RETURN method END GetMethod;
  
VAR
  pMu := NEW(MUTEX);
  p : LRSeq.T;

PROCEDURE SetOptFailureIsError() =
  BEGIN
    outOfDomainResult := FIRST(LONGREAL)
  END SetOptFailureIsError;

PROCEDURE SetOptFailureResult(res : LONGREAL) =
  BEGIN
    outOfDomainResult := res
  END SetOptFailureResult;

PROCEDURE GetOptFailureResult() : LONGREAL =
  BEGIN
    RETURN outOfDomainResult
  END GetOptFailureResult;
  
PROCEDURE OptInit() =
  BEGIN
    vseq   := NEW(OptVarSeq.T).init();
    rhoBeg := FIRST(LONGREAL);
    rhoEnd := LAST(LONGREAL);
    p      := NEW(LRSeq.T).init();
    schemaPath := NIL;
    schemaScmPaths := NEW(TextSeq.T).init();
    schemaDataFn := NIL;
    schemaEval := NIL;
  END OptInit;

PROCEDURE DefSchemaPath(path : Pathname.T) =
  BEGIN schemaPath := path END DefSchemaPath;

PROCEDURE DefLoadScm(scmPath : Pathname.T) =
  BEGIN
    schemaScmPaths.addhi(scmPath)
  END DefLoadScm;

PROCEDURE DefDataFilename(fnm : Pathname.T) =
  BEGIN schemaDataFn := fnm END DefDataFilename;

PROCEDURE DefEval(obj : SchemeObject.T) =
  BEGIN schemaEval := obj END DefEval;
  
PROCEDURE DefOptVar(nm : SchemeSymbol.T; defval, defstep : LONGREAL) =
  BEGIN
    vseq.addlo(OptVar.T { SchemeSymbol.ToText(nm), defval, defstep });
    p.addlo(defval / defstep)
  END DefOptVar;

PROCEDURE SetRhoBeg(to : LONGREAL) =
  BEGIN
    rhoBeg := to
  END SetRhoBeg;

PROCEDURE GetRhoBeg() : LONGREAL =
  BEGIN
    RETURN rhoBeg
  END GetRhoBeg;

PROCEDURE SetRhoEnd(to : LONGREAL) =
  BEGIN
    rhoEnd := to
  END SetRhoEnd;

PROCEDURE GetRhoEnd() : LONGREAL =
  BEGIN
    RETURN rhoEnd
  END GetRhoEnd;

PROCEDURE GetRho() : LONGREAL =
  BEGIN
    RETURN rho
  END GetRho;

PROCEDURE GetIter() : CARDINAL =
  BEGIN
    RETURN iter
  END GetIter;

PROCEDURE GetCoords() : LRSeq.T =
  BEGIN
    RETURN p
  END GetCoords;

PROCEDURE SetCallback(obj : OptCallback.T) =
  BEGIN
    scmCb := obj
  END SetCallback;

PROCEDURE SetNetbatch(to : BOOLEAN) =
  BEGIN
    doNetbatch := to
  END SetNetbatch;

TYPE
  Evaluator = LRScalarFieldPll.T OBJECT
    optVars, paramVars : SchemeObject.T;

    results            : LRVectorLRPairTextTbl.T;
    resultsMu          : MUTEX;

  METHODS
    init(    optVars, paramVars : SchemeObject.T) : LRScalarField.T := InitPll;

    multiEval(at : LRVector.T; samples : CARDINAL) : MultiEval.Result :=
        BaseMultiEval;

    nominalEval(at : LRVector.T) : LONGREAL :=
        BaseNominalEval;
  OVERRIDES
    eval     := BaseEval;
    evalHint := BaseEvalHint;
  END;

PROCEDURE InitPll(pll                : Evaluator;
                  optVars, paramVars : SchemeObject.T) : LRScalarField.T =
  BEGIN
    pll.optVars   := optVars;
    pll.paramVars := paramVars;
    pll.results   := NEW(LRVectorLRPairTextTbl.Default).init();
    pll.resultsMu := NEW(MUTEX);
    RETURN LRScalarFieldPll.T.init(pll, pll)
  END InitPll;

PROCEDURE BaseEvalHint(base : Evaluator; p : LRVector.T) =
  BEGIN
    EVAL base.eval(p)
  END BaseEvalHint;

PROCEDURE BaseEval(base : Evaluator; p : LRVector.T) : LONGREAL =
  BEGIN
    TRY
      WITH res = AttemptEval(base, p, 0, FALSE) DO
        TYPECASE res OF
          LRResult(lr) => RETURN lr.res
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
                        samples : CARDINAL) : MultiEval.Result =
  BEGIN
    IF doDebug THEN
      Debug.Out("BaseMultiEval : samples " & Int(samples))
    END;
    
    TRY
      WITH res = AttemptEval(base, p, samples, FALSE) DO
        TYPECASE res OF
          StocResult(stoc) => RETURN stoc.res
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
                          p       : LRVector.T) : LONGREAL = 
  BEGIN
    IF doDebug THEN
      Debug.Out("BaseNominalEval : " & FmtP(p))
    END;
    
    TRY
      WITH res = AttemptEval(base, p, 1, nominal := TRUE) DO
        TYPECASE res OF
          LRResult(lr) => RETURN lr.res
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

PROCEDURE MustOpenWr(pn : Pathname.T) : Wr.T =
  BEGIN
    TRY
      RETURN FileWr.Open(pn)
    EXCEPT
      OSError.E(x) =>
      Debug.Error(F("Couldnt open %s : OSError.E : %s",
                    pn, AL.Format(x)));
      <*ASSERT FALSE*>
    END
  END MustOpenWr;

TYPE
  EvalResult = BRANDED OBJECT END;

  LRResult = EvalResult OBJECT res : LONGREAL END;

  StocResult = EvalResult OBJECT res : MultiEval.Result END;
  
PROCEDURE AttemptEval(base    : Evaluator;
                      q       : LRVector.T;
                      samples : CARDINAL;
                      nominal : BOOLEAN) : EvalResult
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

  PROCEDURE RunCommand() : LRSeq.T 
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
      WITH dataPath  = subdirPath & "/" & schemaDataFn,
           schemaRes = SchemaReadResult(schemaPath,
                                        dataPath,
                                        scmFiles,
                                        schemaEval,
                                        base.optVars,
                                        base.paramVars) DO
        IF doDebug THEN
          Debug.Out("Schema-processed result : " & FmtLRSeq(schemaRes))
        END;
        LOCK base.resultsMu DO
          (* this will just get the first value......... *)
          EVAL base.results.put(LRVectorLRPair.T { LRVector.Copy(q),
                                                   schemaRes.get(0)} ,
                                                   subdirPath )
        END;
        RETURN schemaRes
      END
    END RunCommand;

  VAR
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

    theResult  : LRSeq.T;
  BEGIN
    IF nominal THEN <*ASSERT samples=1*> END;
    
    SetNbOpts();
    CopyVectorToScheme();
    SetupDirectory();
    
    TRY
      TRY
        TRY
          theResult := RunCommand();
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
              theResult := LRSeq1(outOfDomainResult)
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
          Wr.PutText(resWr, FmtLRSeq(theResult) & "\n");
          Wr.Close(errWr);
          Wr.Close(outWr);
          Wr.Close(resWr)
        END;
        IF samples = 0 OR nominal THEN
          <*ASSERT theResult.size() = 1 *>
          RETURN NEW(LRResult, res := theResult.get(0))
        ELSE
          RETURN NEW(StocResult, res := SeqToMulti(theResult))
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

PROCEDURE SeqToMulti(seq : LRSeq.T) : MultiEval.Result =
  VAR
    s, ss := 0.0d0;
    n := seq.size();
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      WITH x = seq.get(i) DO
        s := s + x;
        ss := ss + x * x
      END
    END;

    LOCK idMu DO
      TRY
        RETURN MultiEval.Result { id    := idNx,
                                  n     := n,
                                  sum   := s,
                                  sumsq := ss }
      FINALLY
        INC(idNx)
      END
    END
  END SeqToMulti;

PROCEDURE LRSeq1(x : LONGREAL) : LRSeq.T =
  BEGIN
    WITH res = NEW(LRSeq.T).init() DO
      res.addhi(x);
      RETURN res
    END
  END LRSeq1;

PROCEDURE FmtLRSeq(seq : LRSeq.T) : TEXT =
  VAR
    res := "";
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      res := res & LongReal(seq.get(i)) & " "
    END;
    RETURN res
  END FmtLRSeq;
  
TYPE
  MyMultiEval = MultiEval.T OBJECT
    base : Evaluator;
  OVERRIDES
    multiEval := DoMultiEval;
    nominalEval := DoNominalEval;
  END;

PROCEDURE DoMultiEval(mme     : MyMultiEval;
                      at      : LRVector.T;
                      samples : CARDINAL) : MultiEval.Result =
  BEGIN
    RETURN mme.base.multiEval(at, samples)
  END DoMultiEval;

PROCEDURE DoNominalEval(mme : MyMultiEval;
                        at : LRVector.T) : LONGREAL =
  BEGIN
    RETURN mme.base.nominalEval(at)
  END DoNominalEval;
  
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

    Debug.Out(F("Ready to call Minimize : pr=%s rhoBeg=%s rhoEnd=%s",
                FmtP(pr), LongReal(rhoBeg), LongReal(rhoEnd)));

    CASE method OF
      Method.NewUOAs =>
      output := NewUOAs.Minimize(pr,
                                 evaluator,
                                 rhoBeg,
                                 rhoEnd,
                                 FIRST(LONGREAL))
    |
      Method.Robust =>
      output := Robust.Minimize(pr,
                                evaluator,
                                rhoBeg,
                                rhoEnd)
    |
      Method.StocRobust =>
      output := StocRobust.Minimize(pr,
                                    NEW(MyMultiEval, base := evaluator).init(evaluator),
                                    rhoBeg,
                                    rhoEnd,
                                    NEW(MyResultWriter,
                                        evaluator := evaluator,
                                        root      := "progress"))
    |
      Method.NewUOA => <*ASSERT FALSE*>
    END;

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
  
PROCEDURE SchemaReadResult(schemaPath ,
                           dataPath                       : Pathname.T;
                           scmFiles                       : TextSeq.T;
                           schemaEval, optVars, paramVars : SchemeObject.T) : LRSeq.T =
  (* code from the schemaeval program *)
  VAR
    dataFiles := NEW(TextSeq.T).init();
    schemaScm : Scheme.T;
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

    TRY
    WITH optVarsNm = T2S("*opt-vars*"),
         paramVarsNm = T2S("*param-vars*") DO
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
    END
    EXCEPT
      Scheme.E(x) =>
      Debug.Error(F("EXCEPTION! Scheme.E \"%s\" when initializing interpreter variables", x))
    END;

    IF doDebug THEN
      Debug.Out("SchemaReadResult : set up interpreter")
    END;
    
    TRY
      WITH schema = ReadSchema(schemaPath),
           data   = ReadData(schema, dataFiles),
           cb     = NEW(SGCallback,
                        schemaScm := schemaScm,
                        results := NEW(LRSeq.T).init()) DO
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
      RETURN LRSeq1(outOfDomainResult)
    |
      Scheme.E(x) =>
      Debug.Warning("?error in Scheme interpreter : " & x);
      RETURN LRSeq1(outOfDomainResult) (* hmm dunno if that is right *)
    END
  END SchemaReadResult;

TYPE
  SGCallback = SchemaGraph.Callback OBJECT
    schemaScm : Scheme.T;
    results : LRSeq.T;
  OVERRIDES
    next := SGCNext;
  END;

PROCEDURE SGCNext(cb : SGCallback) =
  BEGIN
    WITH scmCode = SchemeUtils.List3(
                       T2S("eval-in-env"),
                       L2S(0.0d0),
                       SchemeUtils.List2(T2S("quote"), schemaEval)),
         schemaRes = cb.schemaScm.evalInGlobalEnv(scmCode) DO
      IF doDebug THEN
        Debug.Out("schema eval returned " & SchemeUtils.Stringify(schemaRes))
      END;
      cb.results.addhi( SchemeLongReal.FromO(schemaRes) )
    END
  END SGCNext;
  
CONST T2S = SchemeSymbol.FromText;
      L2S = SchemeLongReal.FromLR;
             
VAR
  pp                        := NEW(ParseParams.T).init(Stdio.stderr);
  scm                : Scheme.T;
  rundirPath                := Process.GetWorkingDirectory();
  myFullSrcPath      : Pathname.T;
  doNetbatch                := TRUE;
  scmFiles                  := NEW(TextSeq.T).init();
  interactive        : BOOLEAN;
  cfgFile            : Pathname.T;
  genOptScm          : Pathname.T;
  doDirectoryWarning : BOOLEAN;
  
BEGIN

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

    IF pp.keywordPresent("-sigmaK") THEN
      StocRobust.SetSigmaK(pp.getNextLongReal())
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
