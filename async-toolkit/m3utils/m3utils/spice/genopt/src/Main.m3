MODULE Main EXPORTS Main, GenOpt;

(* 
   Generic optimizer
*)

IMPORT ParseParams;
IMPORT Debug;
IMPORT Pathname;
IMPORT Stdio;
FROM Fmt IMPORT F, LongReal, Int, Pad;
IMPORT Params;
IMPORT TextSeq;
IMPORT Thread;
IMPORT OSError;
IMPORT Scheme, SchemeM3;
IMPORT FS;
FROM SchemaGraph IMPORT ReadSchema, ReadData, EvalFormulas;
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
IMPORT LongRealSeq;
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

<*FATAL Thread.Alerted*>

VAR
  NbPool  := Env.Get("NBPOOL");
  NbQslot := Env.Get("NBQSLOT");
  M3Utils := Env.Get("M3UTILS");
  NbOpts  := Env.Get("NBRUNOPTIONS");
  
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
  vseq : OptVarSeq.T;
  rhoBeg, rhoEnd : LONGREAL;
  scmCb  : OptCallback.T;
  schemaPath : Pathname.T;
  schemaScmPaths : TextSeq.T;
  schemaDataFn : Pathname.T;
  schemaEval : SchemeObject.T;
  outOfDomainResult := FIRST(LONGREAL);
  method := Method.NewUOAs;

PROCEDURE SetMethod(m : Method) = BEGIN method := m END SetMethod;

PROCEDURE GetMethod() : Method = BEGIN RETURN method END GetMethod;
  
VAR
  pMu := NEW(MUTEX);
  p : LongRealSeq.T;

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
    p      := NEW(LongRealSeq.T).init();
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

PROCEDURE GetCoords() : LongRealSeq.T =
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
  BaseEvaluator = LRScalarField.T OBJECT
    optVars, paramVars : SchemeObject.T;
  METHODS
    init()                      : LRScalarField.T := InitDummy;
  OVERRIDES
    eval     := BaseEval;
    evalHint := BaseEvalHint;
  END;

  PllEvaluator = LRScalarFieldPll.T OBJECT
  METHODS
    init(    optVars, paramVars : SchemeObject.T) : LRScalarField.T := InitPll;
  END;

PROCEDURE InitDummy(base : BaseEvaluator) : LRScalarField.T =
  BEGIN
    RETURN base
  END InitDummy;
  
PROCEDURE InitPll(pll : PllEvaluator;    optVars, paramVars : SchemeObject.T) : LRScalarField.T =
  BEGIN
    RETURN LRScalarFieldPll.T.init(pll, NEW(BaseEvaluator,
                                            optVars := optVars,
                                            paramVars := paramVars));
  END InitPll;

PROCEDURE BaseEvalHint(base : BaseEvaluator; p : LRVector.T) =
  BEGIN
    EVAL base.eval(p)
  END BaseEvalHint;

PROCEDURE FmtP(p : LRVector.T) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    FOR i := FIRST(p^) TO LAST(p^) DO
      Wx.PutText(wx, LongReal(p[i]));
      Wx.PutChar(wx, ' ')
    END;
    RETURN Wx.ToText(wx)
  END FmtP;

PROCEDURE BaseEval(base : BaseEvaluator; p : LRVector.T) : LONGREAL =
  CONST
    MaxAttempts = 1;
  BEGIN
    FOR i := 1 TO MaxAttempts DO
      TRY
        RETURN AttemptEval(base, p)
      EXCEPT
        ProcUtils.ErrorExit =>
        (* loop *)
        IF i # MaxAttempts THEN Thread.Pause(10.0d0) END
      END
    END;
    Debug.Error("BaseEval : Too many attempts p=" & FmtP(p));
    RETURN 0.0d0
  END BaseEval;

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

PROCEDURE AttemptEval(base : BaseEvaluator; q : LRVector.T) : LONGREAL
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
      LOCK pMu DO
        FOR i := FIRST(q^) TO LAST(q^) DO
          p.put(i, q[i])
        END;
        cmd            := scmCb.command()
      END
    END CopyVectorToScheme;

  PROCEDURE SetupDirectory() =
    BEGIN
      TRY
        TRY
          FS.CreateDirectory(subdirPath);
        EXCEPT
          OSError.E(x) =>
          Debug.Warning(F("creating directory \"%s\" : OSError.E : %s",
                          subdirPath,
                          AL.Format(x)))
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
        
        WITH cmdDbgWr = MustOpenWr(subdirPath & "/opt.cmd"),
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

  PROCEDURE RunCommand() : LONGREAL
    RAISES { ProcUtils.ErrorExit } =
    BEGIN
      Debug.Out(F("BaseEval : q = %s\nrunning : %s", FmtP(q), cmd));
      
      cm.wait();
      
      Debug.Out("BaseEval : ran command");
      
      
      (* read the schema *)
      WITH dataPath  = subdirPath & "/" & schemaDataFn,
           schemaRes = SchemaReadResult(schemaPath,
                                        dataPath,
                                        scmFiles,
                                        schemaEval,
                                        base.optVars,
                                        base.paramVars) DO
        Debug.Out("Schema-processed result : " & LongReal(schemaRes));
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

    theResult  : LONGREAL;
  BEGIN

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
            theResult := outOfDomainResult
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
        Wr.PutText(resWr, LongReal(theResult) & "\n");
        Wr.Close(errWr);
        Wr.Close(outWr);
        Wr.Close(resWr)
      END;
      RETURN theResult
    EXCEPT
      Wr.Failure(x) =>
        Debug.Error(F("I/O error : Wr.Failure : while writing output in %s : %s:",
                      subdirPath, AL.Format(x)));
        <*ASSERT FALSE*>

    END
    END
  END AttemptEval;

PROCEDURE DoIt(optVars, paramVars : SchemeObject.T) =
  VAR
    N               := vseq.size();
    pr : LRVector.T := NEW(LRVector.T, N);
    evaluator       := NEW(PllEvaluator).init(optVars, paramVars);
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
                                rhoEnd,
                                20,
                                FIRST(LONGREAL))
    |
      Method.NewUOA =>
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
    
    WITH wr = FileWr.Open("genopt.opt"),
         cwr = FileWr.Open("genopt.cols") DO
      FOR i := vseq.size() - 1 TO 0 BY -1 DO
        WITH v = vseq.get(i),
             xi = output.x[i],
             pi = xi * v.defstep DO
          Wr.PutText(wr, LongReal(pi));
          Wr.PutText(cwr, v.nm);
          VAR c : CHAR; BEGIN
            IF i = 0 THEN c := '\n' ELSE c := ',' END;
            Wr.PutChar(wr, c);
            Wr.PutChar(cwr, c)
          END
        END
      END;
      Wr.Close(wr); Wr.Close(cwr)
    END
    EXCEPT
      Wr.Failure(x) =>
        Debug.Error(F("I/O error : Wr.Failure : while optimization result : %s:",
                      AL.Format(x)))

    END
  END DoIt;

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
                           dataPath : Pathname.T;
                           scmFiles : TextSeq.T;
                           schemaEval, optVars, paramVars : SchemeObject.T) : LONGREAL =
  (* code from the schemaeval program *)
  VAR
    dataFiles := NEW(TextSeq.T).init();
    schemaScm : Scheme.T;
  BEGIN
    Debug.Out(F("SchemaReadResult : schemaPath %s , dataPath %s , eval %s",
                schemaPath, dataPath, SchemeUtils.Stringify(schemaEval)));
    
    dataFiles.addhi(dataPath);
    
    WITH scmarr = NEW(REF ARRAY OF Pathname.T, scmFiles.size()) DO
      FOR i := FIRST(scmarr^) TO LAST(scmarr^) DO
        scmarr[i] := scmFiles.get(i);
        Debug.Out("SchemaReadResult: loading scheme file " & scmarr[i]);
      END;
      TRY
        schemaScm := NEW(SchemeM3.T).init(scmarr^)
      EXCEPT
        Scheme.E(x) =>
        Debug.Error(F("EXCEPTION! Scheme.E \"%s\" when initializing interpreter", x))
      END
    END;

    WITH T2S = SchemeSymbol.FromText,
         optVarsNm = T2S("*opt-vars*"),
         paramVarsNm = T2S("*param-vars*") DO
      schemaScm.setInGlobalEnv(optVarsNm, optVars);
      schemaScm.setInGlobalEnv(paramVarsNm, paramVars)
    END;

    Debug.Out("SchemaReadResult : set up interpreter");
    
    TRY
      WITH schema = ReadSchema(schemaPath),
           data   = ReadData(schema, dataFiles) DO
        Debug.Out("SchemaReadResult : schema and data loaded");

        EvalFormulas(schemaScm, schema, data);

        Debug.Out("SchemaReadResult : formulas evaluated");

        WITH T2S = SchemeSymbol.FromText,
             scmCode = SchemeUtils.List2(
                            T2S("eval-in-env"),
                            SchemeUtils.List2(T2S("quote"), schemaEval)),
             schemaRes = schemaScm.evalInGlobalEnv(scmCode) DO
          Debug.Out("schema eval returned " & SchemeUtils.Stringify(schemaRes));
          RETURN SchemeLongReal.FromO(schemaRes)
        END
      END
    EXCEPT
      OSError.E, Rd.Failure =>
      Debug.Warning("Caught system error reading schema/data.  Returning failure.");
      RETURN outOfDomainResult
    |
      Scheme.E(x) =>
      Debug.Warning("?error in Scheme interpreter : " & x);
      RETURN outOfDomainResult (* hmm dunno if that is right *)
    END
  END SchemaReadResult;
  
VAR
  pp                        := NEW(ParseParams.T).init(Stdio.stderr);
  scm           : Scheme.T;
  rundirPath                := Process.GetWorkingDirectory();
  myFullSrcPath : Pathname.T;
  doNetbatch := TRUE;
  scmFiles := NEW(TextSeq.T).init();
  interactive : BOOLEAN;
  cfgFile : Pathname.T;
  genOptScm : Pathname.T;
  
BEGIN
  Debug.SetOptions(SET OF Debug.Options { Debug.Options.PrintThreadID } );
  scmFiles.addhi("require");
  scmFiles.addhi("m3");

  TRY
    interactive := pp.keywordPresent("-i") OR pp.keywordPresent("-interactive");
    
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
    SchemeReadLine.MainLoop(NEW(ReadLine.Default).init(), scm)
  ELSE
    WITH senv = NARROW(scm.getGlobalEnvironment(), SchemeEnvironment.T),
         T2S = SchemeSymbol.FromText,
         optVars = senv.lookup(T2S("*opt-vars*")),
         paramVars = senv.lookup(T2S("*param-vars*")) DO
      DoIt(optVars, paramVars)
    END
  END
  
END Main.
