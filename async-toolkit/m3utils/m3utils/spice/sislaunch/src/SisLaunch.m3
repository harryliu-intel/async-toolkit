MODULE SisLaunch EXPORTS Main;

(* 
   siliconsmart-characterization batch-job controller.

   Allows for detailed control over parallel job issuing using 
   the rechar_flow from Intel Labs.

   Author: mika.nystroem@intel.com
   September-October, 2023
*)


IMPORT Pathname;
IMPORT Debug;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Params;
IMPORT Rd, FileRd;
IMPORT TextReader;
IMPORT Text;
IMPORT Wr, FileWr;
<*NOWARN*>IMPORT Process; 
IMPORT SisTask, SisTaskSeq;
FROM Fmt IMPORT Int, F, FN, Bool; IMPORT Fmt;
IMPORT Thread;
IMPORT RefSeq;
IMPORT TextCardTbl;
IMPORT ProcUtils;
IMPORT Env;
IMPORT CitTextUtils AS TextUtils;
IMPORT Math;
IMPORT AL;
IMPORT TextWr;
IMPORT OSError;
IMPORT Word;

<*FATAL Thread.Alerted*>

CONST
  Usage = "[-clearenv|-ce][-sispath|-sp <path to SiliconSmart>][-sisworkers <n>][-pllcmds <n>][-celllist|-cl <cell_list path>] <command-file>";
  TE    = Text.Equal;
  LR    = Fmt.LongReal;

  CharScript = "char.tcl"; (* name of characterization script *)
  MaxFailures = 10; (* max failures allowed for a task *)
  
VAR
  NbPool  := Env.Get("NBPOOL"); (* current Netbatch pool *)

TYPE
  TA = ARRAY OF TEXT;
  
  Cmd = AllocatedTask OBJECT
    cwd : Pathname.T;
    cmd : TEXT;
  END;

  AllocatedTask = SisTask.T OBJECT
    label    : TEXT;
    nhosts   : CARDINAL;
    id       : CARDINAL;
    env      : ProcUtils.Env;
    failures : CARDINAL := 0;
    running             := FALSE;
  END;

  Launch = AllocatedTask OBJECT
    bundle    : TEXT;
    sisDir    : Pathname.T; (* first part of working directory *)
    clistPath : Pathname.T; (* path to cell_list file *)
    bunDir    : Pathname.T; (* second part of working directory *)
  END;

PROCEDURE DebugTask(task : AllocatedTask) : TEXT =
  VAR
    res := F("label %s, nhosts %s, failures %s, running %s",
             Debug.UnNil(task.label),
             Int(task.nhosts),
             Int(task.failures),
             Bool(task.running));
  BEGIN
    TYPECASE task OF
      Cmd(cmd) => res := F("Cmd %s cwd %s cmd %s", res, cmd.cwd, cmd.cmd)
    |
      Launch(launch) => res := F("Launch %s sisDir %s bunDir %s",
                                 res, launch.sisDir, launch.bunDir)
    ELSE
      res := "UNKNOWN " & res
    END;
    RETURN res
  END DebugTask;

PROCEDURE Parse(cmdPath : Pathname.T; curEnv : ProcUtils.Env) : SisTaskSeq.T =
  VAR
    rd         : Rd.T;
    stdcellDir : Pathname.T;
    tasks := NEW(SisTaskSeq.T).init();
    lNo := 0;
    nextId := 0;
  BEGIN
    TRY
      rd := FileRd.Open(cmdPath);
    EXCEPT
      OSError.E(x) => Debug.Error(F("Parse : cant open command file \"%s\" : OSError.E : %s", cmdPath, AL.Format(x)))
    END;

    TRY
      LOOP
        INC(lNo);
        WITH line   = Rd.GetLine(rd),
             reader = NEW(TextReader.T).init(line),
             first  = reader.nextE(":") DO
          IF    TE(first, "CMD") THEN
            (* just a generic command *)
            WITH cwd = reader.nextE(":"),
                 cmd  = reader.nextE(""),
                 task = NEW(Cmd,
                            cmd := cmd,
                            cwd := cwd,
                            dbg := line,
                            label := "cmd",
                            id := nextId,
                            env := curEnv) DO
              Debug.Out("Got CMD : " & cmd);
              tasks.addhi(task);
              INC(nextId)
            END
          ELSIF TE(first, "STDCELLDIR") THEN
            stdcellDir := reader.nextE(":")
          ELSIF TE(first, "ENV") THEN
            WITH setting = reader.nextE(""),
                 eqPos   = Text.FindChar(setting, '='),
                 nm      = Text.Sub(setting, 0, eqPos),
                 val     = Text.Sub(setting, eqPos + 1, LAST(CARDINAL)) DO
              <*ASSERT eqPos > 0*>
              IF Debug.GetLevel() > 10 THEN
                Debug.Out(F("Setting env var %s := %s", nm, val))
              END;
              curEnv := SetEnvVar(curEnv, nm, val)
            END
          ELSIF TE(first, "CLEARENV") THEN
            curEnv := NEW(ProcUtils.Env, 0)
          ELSIF TE(first, "LAUNCH") THEN
            (* a siliconsmart characterization launch *)
            WITH bundle     = reader.nextE(":"),
                 sisDir     = reader.nextE(":"),
                 clistPath  = reader.nextE(":"),
                 bunDir     = reader.nextE(""),
                 task       = NEW(Launch,
                                  dbg := line,
                                  bundle := bundle,
                                  sisDir := sisDir,
                                  clistPath := clistPath,
                                  bunDir := bunDir,
                                  label := "launch",
                                  id := nextId,
                                  env := curEnv) DO
              Debug.Out("Got LAUNCH : " & bundle);
              tasks.addhi(task);
              INC(nextId)

            END
          ELSE
          END
        END
      END
    EXCEPT
      Rd.EndOfFile => TRY Rd.Close(rd) EXCEPT ELSE END
    |
      TextReader.NoMore => Debug.Error(F("?Syntax error on line %s of %s", Int(lNo), cmdPath))
    |
      Rd.Failure(x) => Debug.Error(F("Parse : cant read command file \"%s\" : Rd.Failure : %s", cmdPath, AL.Format(x)))
    END;

    RETURN tasks

  END Parse;

VAR
  mu := NEW(MUTEX);
  c  := NEW(Thread.Condition);

  (* design:

     master waits on global c,
     worker waits on worker.privateC 

     all protected by same global mu

     Not a high-bandwidth program, should be OK.
  *)

TYPE
  (* a Worker is the data structure for a worker thread *)
  Worker = Thread.Closure OBJECT
    task     : AllocatedTask := NIL;
    quit                     := FALSE;
    me       : Thread.T;
    privateC : Thread.Condition; (* wait on this while idle *)
    id       : CARDINAL;
  OVERRIDES
    apply := WApply;
  END;

PROCEDURE RunText(cmd            : TEXT;
                  stdout, stderr : ProcUtils.Writer;
                  stdin          : ProcUtils.Reader;
                  wd0            : Pathname.T        := NIL;
                  env            : ProcUtils.Env     := NIL) : ProcUtils.Completion =
  BEGIN
    Debug.Out(F("RunText(%s)", cmd));
    
    RETURN ProcUtils.RunText(cmd, stdout, stderr, stdin, wd0, env)
  END RunText;
  
PROCEDURE RunCommand(cmdtext : TEXT;
                     cmdid   : CARDINAL;
                     env     : ProcUtils.Env;
                     wd      : Pathname.T)
  RAISES { OSError.E, ProcUtils.ErrorExit } =
  VAR
    Owr            := NEW(TextWr.T).init();
    Ewr            := NEW(TextWr.T).init();
    stdout         := ProcUtils.WriteHere(Owr);
    stderr         := ProcUtils.WriteHere(Ewr);

    runcmd         :=
        FN("nbjob run --target %s --class 4C --class SLES12 --mode interactive %s",
           TA { NbPool, cmdtext });
    cm             : ProcUtils.Completion;
  BEGIN
    
    TRY
      WITH cmdWr  = FileWr.Open(Int(cmdid) & ".cmd"),
           cwdWr  = FileWr.Open(Int(cmdid) & ".cwd"),
           envWr  = FileWr.Open(Int(cmdid) & ".env")
           DO

        Wr.PutText(cmdWr, runcmd);
        Wr.PutChar(cmdWr, '\n');
        Wr.PutText(cwdWr, Debug.UnNil(wd));
        Wr.PutChar(cwdWr, '\n');

        FOR i := FIRST(env^) TO LAST(env^) DO
          Wr.PutText(envWr, env[i]);
          Wr.PutChar(envWr, '\n')
        END;
        
        Wr.Close(cmdWr);
        Wr.Close(cwdWr);
        Wr.Close(envWr)
        
      END;

      TRY
        cm := RunText(runcmd,
                      stdout := stdout,
                      stderr := stderr,
                      stdin  := NIL,
                      env    := env,
                      wd0    := wd);
        
        cm.wait()
      FINALLY
        WITH output = TextWr.ToText(Owr),
             outerr = TextWr.ToText(Ewr),
             
             outWr  = FileWr.Open(Int(cmdid) & ".stdout"),
             errWr  = FileWr.Open(Int(cmdid) & ".stderr") DO
          Debug.Out("Ran command stdout was " & output);
          Debug.Out("Ran command stderr was " & outerr);
          
          Wr.PutText(outWr, output);
          Wr.PutText(errWr, outerr);
          
          Wr.Close(outWr);
          Wr.Close(errWr);
        END
      END;
      
      RETURN
    EXCEPT
      ProcUtils.ErrorExit(err) =>
      WITH msg = F("command \"%s\" with output\n====>\n%s\n<====\n\nraised ErrorExit : %s",
                   runcmd,
                   TextWr.ToText(Ewr),
                   ProcUtils.FormatError(err)) DO
        Debug.Warning(msg);
        RAISE ProcUtils.ErrorExit(err)
      END
      |
        Wr.Failure(x) =>
        Debug.Warning(F("Error writing output from command id %s : Wr.Failure : %s",
                        Int(cmdid),
                        AL.Format(x)))
      END
   END RunCommand;
  
PROCEDURE WApply(w : Worker) : REFANY =
  (* the worker thread *)
  BEGIN
    TRY
      LOOP
        LOCK mu DO
          WHILE w.task = NIL AND NOT w.quit DO
            Thread.Wait(mu, w.privateC)
          END;
          <*ASSERT w.task # NIL OR w.quit*>
        END;
        
        IF w.task = NIL AND w.quit THEN
          Debug.Out(F("Worker %s exiting", Int(w.id)));
          
          EXIT
        END;

        Debug.Out(F("Worker %s got task %s", Int(w.id), NARROW(w.task, AllocatedTask).label));
        (* run task *)
        TRY
        TYPECASE w.task OF
          Cmd(cmd) =>
          RunCommand(cmd.cmd, cmd.id, cmd.env, cmd.cwd)
        |
          Launch(launch) =>
          IF sisPath = NIL THEN
            Debug.Error("Must specify -sispath")
          END;
          WITH sisCmd = sisPath & " " & CharScript,
               runEnv = SetEnvVar(launch.env,
                                  "run_list_maxsize",
                                  Int(launch.nhosts)),
               runDir = launch.sisDir & "/" & launch.bunDir DO
            RunCommand(sisCmd, launch.id, runEnv, runDir)
          END
        ELSE
          <*ASSERT FALSE*>
        END;
        w.task.failures := 0;
        
        EXCEPT
          ProcUtils.ErrorExit =>
          LOCK mu DO
            INC(w.task.failures);
            Debug.Warning("Caught ProcUtils.ErrorExit attempting command execution : task : " & DebugTask(w.task));
            Thread.Pause(FLOAT(Word.Shift(1, w.task.failures), LONGREAL));
            
            w.task.running := FALSE
          END
        |
          OSError.E(x) =>
          Debug.Warning("Caught OSError.E attempting command execution : " & AL.Format(x))
        END;
        
        Debug.Out(F("Worker %s done with task", Int(w.id)));

        LOCK mu DO
          w.task.running := FALSE;
          w.task := NIL;
          Thread.Signal(c)
        END
      END
    FINALLY
      RETURN NIL
    END
  END WApply;

PROCEDURE GetWholeEnv() : ProcUtils.Env =
  (* get the entire environment of this process *)
  VAR
    n   := Env.Count;
    res := NEW(ProcUtils.Env, n);
  BEGIN
    FOR i := 0 TO n - 1 DO
      VAR
        nm, val : TEXT;
      BEGIN
        Env.GetNth(i, nm, val);
        res[i] := F("%s=%s", nm, val)
      END
    END;
    RETURN res
  END GetWholeEnv;

PROCEDURE SetEnvVar(env : ProcUtils.Env; nm, val : TEXT) : ProcUtils.Env =
  (* set a single env var to given value *)
  VAR
    pfx := nm & "=";
  BEGIN
    FOR i := 0 TO NUMBER(env^) - 1 DO
      IF TextUtils.HavePrefix(env[i], pfx) THEN
        env[i] := F("%s=%s", nm, val);
        RETURN env
      END
    END;
    WITH newEnv = NEW(ProcUtils.Env, NUMBER(env^) + 1) DO
      SUBARRAY(newEnv^, 0, NUMBER(env^)) := env^;
      newEnv[LAST(newEnv^)] := F("%s=%s", nm, val);
      RETURN newEnv
    END
  END SetEnvVar;

PROCEDURE WorkersReady(VAR worker : Worker) : CARDINAL =
  (* 
     return # of workers ready to run --

     REQUIRES mu to be locked on entry
  *)
  VAR
    res := 0;
    hostsActive := 0;
    first : Worker;
  BEGIN
    FOR i := 0 TO workers.size() - 1 DO
      WITH w = NARROW(workers.get(i),Worker) DO
        IF w.task = NIL THEN
          INC(res);
          IF first = NIL THEN
            first := w
          END
        ELSE
          hostsActive := hostsActive + w.task.nhosts
        END
      END
    END;

    IF hostsActive > sisWorkers THEN
      Debug.Out(F("WorkersReady : Oversubscribed! : hostsActive = %s > sisWorkers %s", Int(hostsActive), Int(sisWorkers)));
      res := 0
    END;
    
    IF res # 0 THEN
      worker := first
    END;
    RETURN res
  END WorkersReady;

VAR
  workers := NEW(RefSeq.T).init();

PROCEDURE Run(tasks : SisTaskSeq.T) =

  PROCEDURE DispatchTask(task : AllocatedTask) =
    BEGIN
      LOCK mu DO
        VAR
          worker : Worker := NIL;
          nready : CARDINAL;
        BEGIN
          LOOP
            nready := WorkersReady(worker);
            IF nready = 0 THEN
              Thread.Wait(mu, c)
            ELSE
              EXIT
            END
          END;
          <*ASSERT worker # NIL AND nready # 0*>
          
          (* if task is a Launch, we need to determine the allocation of
             machines to it, else allocation is 1 *)
          
          TYPECASE task OF
            Cmd(cmd) => cmd.nhosts := 1
          |
            Launch(launch) =>
            
                (* here we need to compute how many worker hosts to allocate
                   to this bundle.
                   
                   This is the hardest of the whole program.
                   
                   We have a certain number of bundles,    nbundles,
                   a total number of cells,                ncells,
                   and a total number of parallel batches, nparallel,
                   and a total number of working hosts,    sisWorkers.
                   
                   We need to run nsequential = MAX(nbundles / nparallel, 1)
                   sequential jobs (on average).  That means that total
                   worker assignments will be (on average) 
                   
                   nassignments = nsequential * sisWorkers
                   
                   if we divide assignments equally among bundles, we get
                   
                   nequalPerBundle = nassignments / nbundles
                   
                   and if assignments are divided equally among cells, we get
                   
                   nweightedForBundle = cells[bundle] / ncells * nassignments
                   
                   we use a weighting geometric mean to do this.
                   
                   nForBundle = 
                   nequalPerBundle^(1-alpha) * nweightedForBundle^alpha
                   
                *)
            
            CONST
              alpha = 0.7d0;
            TYPE
              LRT = LONGREAL;
            VAR
              bundleCells : CARDINAL;
            BEGIN
              WITH hadIt = bundleCnts.get(launch.bundle, bundleCells) DO
                <*ASSERT hadIt*>
              END;
              
              WITH ntasks       = FLOAT(launchs.size(),LRT),
                   nparallel    = FLOAT(pllCmds,LRT),
                   nsequential  = MAX(ntasks / nparallel, 1.0d0),
                   nassignments = nsequential * FLOAT(sisWorkers,LRT),
                   
                   nEqualPerTask    = nassignments / ntasks,
                   nWeightedForTask =
                     MIN(FLOAT(bundleCells,LRT) / FLOAT(ncells,LRT) * nassignments, FLOAT(sisWorkers,LRT)),
                   
                   nForTask =
                         Math.pow(nEqualPerTask   , 1.0d0 - alpha) *
                         Math.pow(nWeightedForTask,         alpha) DO

                <*ASSERT launch.bundle # NIL*>
                Debug.Out(FN("Bundle %s ; %s : ntasks=%s nparallel=%s nsequential=%s nassignments=%s -> nEqualPerTask=%s, nWeightedForTask=%s ; alpha=%s; nForTask=%s ; ROUND=%s",
                             TA{launch.bundle,
                                launch.bunDir,
                                LR(ntasks),
                                LR(nparallel),
                                LR(nsequential),
                                LR(nassignments),
                                LR(nEqualPerTask),
                                LR(nWeightedForTask),
                                LR(alpha),
                                LR(nForTask),
                                Int(ROUND(nForTask))})
                );
                
                
                task.nhosts := ROUND(nForTask)
              END
            END
          ELSE
            <* ASSERT FALSE *>
          END;

          <*ASSERT worker.task = NIL*>

          task.running := TRUE;
          worker.task := task;
          Thread.Signal(worker.privateC)
        END
      END(*LOCK*)
    END DispatchTask;

  PROCEDURE RepeatFailedTasks() =
    VAR
      worker : Worker := NIL;
      nready : CARDINAL;
      task   : AllocatedTask;
      haveFailed : BOOLEAN;
    BEGIN
      LOCK mu DO
        LOOP
          nready     := WorkersReady(worker);
          haveFailed := FailedTasks(task);
          
          IF nready = 0 THEN
            (* all workers busy *)
            Thread.Wait(mu, c)
          ELSIF nready = workers.size() AND NOT haveFailed THEN
            (* all workers idle, no failed tasks -- we are done *)
            EXIT
          ELSIF NOT haveFailed THEN
            (* some worker available, but no failed tasks *)
            Thread.Wait(mu, c)
          ELSIF task.failures > MaxFailures THEN
            Debug.Error("Task failed too many times: " & DebugTask(task))
          ELSE
            (* some worker available, some failed task available *)
            Debug.Out("Repeating failed task : " & DebugTask(task));
            task.running := TRUE;
            worker.task := task;
            Thread.Signal(worker.privateC)
          END
        END
      END
    END RepeatFailedTasks;

  PROCEDURE FailedTasks(VAR task : AllocatedTask) : BOOLEAN =
    BEGIN
      FOR i := 0 TO tasks.size() - 1 DO
        WITH this = NARROW(tasks.get(i),AllocatedTask) DO
          IF NOT this.running AND this.failures # 0 THEN
            task := this;
            RETURN TRUE
          END
        END
      END;
      RETURN FALSE
    END FailedTasks;
    
  VAR
    cmds    := NEW(SisTaskSeq.T).init();
    launchs := NEW(SisTaskSeq.T).init();
    ncells  : CARDINAL := 0; (* total cells to char *)
  BEGIN
    
    FOR i := 0 TO tasks.size() - 1 DO
      
      WITH t = tasks.get(i) DO
        TYPECASE t OF
          Cmd => cmds.addhi(t)
        |
          Launch(l) => launchs.addhi(t);

          <*ASSERT bundleCnts # NIL*>
          <*ASSERT l # NIL*>
          
          VAR
            bcn : CARDINAL;
            hadIt := bundleCnts.get(l.bundle, bcn);
          BEGIN
            <*ASSERT hadIt*>
            ncells := ncells + bcn
          END

        ELSE
          (* skip *)
        END
      END
    END;

    Debug.Out(F("SisLaunch.Run : %s cmds ; %s launchs",
                Int(cmds.size()),
                Int(launchs.size())));

    FOR i := 0 TO pllCmds - 1 DO
      WITH worker = NEW(Worker, privateC := NEW(Thread.Condition), id := i) DO
        worker.me := Thread.Fork(worker);
        workers.addhi(worker)
      END
    END;

    (* launch all the tasks once *)
    FOR i := 0 TO tasks.size() - 1 DO
      WITH task = tasks.get(i) DO
        DispatchTask(task)
      END
    END;

    RepeatFailedTasks()
    
  END Run;

PROCEDURE ShutdownWorkers() =
  BEGIN

    FOR i := 0 TO workers.size() - 1 DO
      Debug.Out(F("Commanding worker %s to exit", Int(i)));
      WITH w = NARROW(workers.get(i), Worker) DO
        LOCK mu DO
          w.quit := TRUE;
          Thread.Signal(w.privateC)
        END
      END
    END;

    
    FOR i := 0 TO workers.size() - 1 DO
      Debug.Out(F("Awaiting worker %s exit", Int(i)));
      EVAL Thread.Join(NARROW(workers.get(i), Worker).me)
    END
  END ShutdownWorkers;
  
PROCEDURE ReadCellList(cellListFn : Pathname.T) : TextCardTbl.T =
  VAR
    res := NEW(TextCardTbl.Default).init();
    rd : Rd.T;
    q  : CARDINAL;
  BEGIN
    TRY
      rd := FileRd.Open(cellListFn);

      LOOP
        WITH line   = Rd.GetLine(rd),
             reader = NEW(TextReader.T).init(line),
             first  = reader.nextE(" \t", skipNulls := TRUE),
             <*UNUSED*>second = reader.nextE(" \t", skipNulls := TRUE) DO
          q := 0;
          EVAL res.get(first, q);
          INC(q);
          EVAL res.put(first, q)
        END
      END
    EXCEPT
      Rd.EndOfFile =>
      TRY Rd.Close(rd) EXCEPT ELSE END
    |
      OSError.E(x) => Debug.Error(F("ReadCellList : cant open cell-list file \"%s\" : OSError.E : %s", cellListFn, AL.Format(x)))
    |
      Rd.Failure(x) => Debug.Error(F("ReadCellList : cant read cell-list file \"%s\" : OSError.E : %s", cellListFn, AL.Format(x)))
    |
      TextReader.NoMore => Debug.Error(F("Syntax error reading cell-list file \"%s\"", cellListFn))
    END;

    IF Debug.GetLevel() >= 10 THEN
      VAR
        bnm : TEXT;
        cnt : CARDINAL;
        iter := res.iterate();
      BEGIN
        WHILE iter.next(bnm, cnt) DO
          Debug.Out(F("bundle %s cells %s", bnm, Int(cnt)))
        END
      END
    END;
    
    RETURN res
  END ReadCellList;
  
VAR
  ifn        : Pathname.T;
  pp                         := NEW(ParseParams.T).init(Stdio.stderr);
  sisWorkers : CARDINAL      := 100;
  pllCmds    : CARDINAL      :=   5;
  cellListFn : Pathname.T    := NIL;
  bundleCnts : TextCardTbl.T := NIL;
  sisPath    : Pathname.T    := NIL;
  clearEnv                   := FALSE;
  baseEnv    : ProcUtils.Env;
  
BEGIN
  TRY

    IF pp.keywordPresent("-sisworkers") THEN
      sisWorkers := pp.getNextInt()
    END;

    clearEnv := pp.keywordPresent("-clearenv") OR pp.keywordPresent("-ce");

    IF clearEnv THEN
      baseEnv := NEW(ProcUtils.Env, 0)
    ELSE
      baseEnv := GetWholeEnv()
    END;

    IF pp.keywordPresent("-pllcmds") THEN
      pllCmds := pp.getNextInt()
    END;

    IF pp.keywordPresent("-celllist") OR pp.keywordPresent("-cl") THEN
      cellListFn := pp.getNext()
    END;

    IF pp.keywordPresent("-sispath") OR pp.keywordPresent("-sp") THEN
      sisPath := pp.getNext()
    END;
    
    pp.skipParsed();
    
    ifn := pp.getNext();

    pp.finish()
  EXCEPT
    ParseParams.Error =>
    Debug.Error("Cant parse command-line parameters\nUsage:\n" & Params.Get(0) & " " & Usage)
  END;

  IF cellListFn # NIL THEN
    bundleCnts := ReadCellList(cellListFn)
  END;
  
  WITH tasks = Parse(ifn, baseEnv) DO
    Run(tasks)
  END;

  ShutdownWorkers();

  (*
  Debug.Out("SisLaunch exiting!");
  Process.Exit(1)
  *)

END SisLaunch.

  (* earlier stab:

              VAR
                myCells, readyCells, nxtCells : CARDINAL;
                allocatedHosts := 0;
                remainingHosts : INTEGER;
              BEGIN
                WITH hadIt = bundleCnts.get(launch, myCells) DO
                  <*ASSERT hadIt*>
                END;

                FOR i := 0 TO workers.size() - 1 DO
                  IF worker.task # NIL THEN
                    allocatedHosts := allocatedHosts + worker.nhosts
                  END
                END;

                remainingHosts := sisWorkers - allocatedHosts;

                readyCells := myCells;
                
                FOR np := i + 1 TO MIN(tasks.size() - 1, i + nready) DO
                  (* these are the tasks ready to run *)
                  TYPECASE tasks.get(np) OF
                    Cmd => INC(readyCmds)
                  |
                    Launch(nxtLaunch) =>
                    WITH hadIt = bundleCnts.get(nxtLaunch.bundle, nxtCells) DO
                      <*ASSERT hadIt*>
                    END;
                    readyCells := readyCells + nxtCells
                  END
                END;

                (* inputs ready:

                   I have myCells to run.
                   There are readyCmds ready CMDs.
                   There are readyCells total cells ready to run.
                   There are remainingHosts to allocate.
                *)

                WITH cellHosts  = remainingHosts - readyCmds,
                     myCellFrac = FLOAT(myCells,LR) / FLOAT(readyCells,LR),
                     myHosts    = FLOOR(FLOAT(cellHosts, LR) * myCellFrac) DO
                  launch.nhosts := myHosts
                END
                
              END
 
  *)

