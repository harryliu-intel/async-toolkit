MODULE DistRewriter;
IMPORT Stdio;
IMPORT Trace;
IMPORT Wr;
IMPORT Rd;
IMPORT TextReader;
FROM Fmt IMPORT F, Int, LongReal;
IMPORT Text;
IMPORT Pickle;
IMPORT TraceOp;
IMPORT DistZTrace;
IMPORT Pathname;
IMPORT Debug;
IMPORT RefSeq;
IMPORT Thread;
IMPORT ProcUtils;
IMPORT OSError;
IMPORT SpiceCompress;
IMPORT TraceRewriter;
IMPORT TempDataRep;
IMPORT TextSeq;
IMPORT FsdbComms;
IMPORT FileWr;
IMPORT TextRd;
IMPORT ArithConstants;
IMPORT TextWr;

<*FATAL Thread.Alerted*>

CONST TE      = Text.Equal;
      doDebug = TRUE;
      LR      = LongReal;
      
REVEAL
  T = Public BRANDED Brand OBJECT
    root    : Pathname.T;      (* static/unprotected *)
    cmdPath : Pathname.T;      (* static/unprotected *)

    mu      : MUTEX;
    c       : Thread.Condition;
    
    cmdseq  : RefSeq.T;        (* protected by mu *)
    resseq  : RefSeq.T;        (* protected by mu *)
    rew     : TraceRewriter.T; (* protected by mu *)
    active  : CARDINAL;        (* protected by mu *)
    running : CARDINAL;        (* protected by mu *)
    
  OVERRIDES
    init       := Init;
    addNamedOp := AddNamedOp;
    flush      := Flush;
  END;

  (* 
     idea is that clients enter jobs into cmdseq

     threads pick up jobs from cmdseq, execute them on the remote

     results are tacked onto the jobs, jobs are inserted into resseq

     single thread picks up results

     everything is protected by the single mutex mu

     all signalling is done with the single condition var c, using Broadcast

     I doubt we will have performance problems; if we do, we can add
     other cond vars and/or mutexes
  *)
     

PROCEDURE Init(t        : T;
               root     : Pathname.T;
               nthreads : CARDINAL;
               cmdPath  : Pathname.T;
               rew      : TraceRewriter.T) : T =
  BEGIN
    t.root    := root;
    t.cmdPath := cmdPath;
    t.mu      := NEW(MUTEX);
    t.c       := NEW(Thread.Condition);
    t.cmdseq  := NEW(RefSeq.T).init();
    t.resseq  := NEW(RefSeq.T).init();
    t.rew     := rew;
    t.active  := 0;
    t.running := 0;

    FOR i := 0 TO nthreads - 1 DO
      WITH cl = NEW(Closure, t := t).init() DO
      END
    END;

    WITH cl = NEW(ResClosure, t := t) DO
      EVAL Thread.Fork(cl)
    END;

    (* await all the threads starting *)
    LOCK t.mu DO
      WHILE t.running < nthreads DO
        Thread.Wait(t.mu, t.c)
      END
    END;

    Debug.Out("DistRewriter.Init : All threads running, ready to compute.");
    
    RETURN t
  END Init;

PROCEDURE AddNamedOp(t       : T;
                     op      : TraceOp.T;
                     nm      : TEXT;
                     relPrec : LONGREAL;
                     code    : ArithConstants.Encoding) =
  BEGIN
    WITH task = NEW(Task,
                    op       := op,
                    nodeId   := 16_c0edbabe,
                    relPrec  := relPrec,
                    nm       := nm,
                    code     := code) DO
      LOCK t.mu DO
        t.cmdseq.addhi(task);
        INC(t.active);
        Thread.Broadcast(t.c)
      END
    END
  END AddNamedOp;

PROCEDURE Flush(t : T) =
  BEGIN
    (* how to know we don't have jobs out and running *)
    LOCK t.mu DO
      WHILE t.active # 0 DO
        Thread.Wait(t.mu, t.c)
      END;

      (* note that we flush under mu *)
      t.rew.flush()
    END
  END Flush;
  
TYPE
  ResClosure = Thread.Closure OBJECT
    t : T;
  OVERRIDES
    apply := ResApply;
  END;

PROCEDURE Seq1(txt : TEXT) : TextSeq.T =
  VAR
    res := NEW(TextSeq.T).init();
  BEGIN
    res.addhi(txt);
    RETURN res
  END Seq1;

PROCEDURE ResApply(cl : ResClosure) : REFANY =
  CONST
    DebugAll = TRUE;
  VAR
    rep : TempDataRep.T;
  BEGIN
    LOOP
      LOCK cl.t.mu DO
        WHILE cl.t.resseq.size() = 0 DO
          Thread.Wait(cl.t.mu, cl.t.c)
        END;
        WITH res = NARROW(cl.t.resseq.remlo(), Task) DO
          IF DebugAll THEN
            <*FATAL Wr.Failure,OSError.E*>
            BEGIN
              WITH wr = FileWr.Open("result.dat") DO
                Wr.PutText(wr, res.result);
                Wr.Close(wr)
              END
            END
          END;
          TempDataRep.ReadFromTempNoNorm(TextRd.New(res.result),
                                         rep,
                                         Text.Length(res.result));
          rep.norm := res.norm;
          EVAL cl.t.rew.addhi(rep.finalData, rep.norm, rep.code, Seq1(res.nm))
        END;
        DEC(cl.t.active);
        Thread.Broadcast(cl.t.c)
      END
    END
  END ResApply;
  
TYPE
  Closure = Thread.Closure OBJECT
    t : T;
  METHODS
    init() : Closure := ClInit;
  OVERRIDES
    apply := ClApply;
  END;

PROCEDURE ClInit(cl : Closure) : Closure =
  BEGIN
    EVAL Thread.Fork(cl);
    RETURN cl
  END ClInit;

TYPE
  Task = OBJECT
    op      : TraceOp.T;
    nodeId  : CARDINAL;
    relPrec : LONGREAL;
    nm      : TEXT;
    code    : ArithConstants.Encoding;

    result  : TEXT;               (* filled in by execution *)
    norm    : SpiceCompress.Norm; (* filled in by execution *)
  END;
  
PROCEDURE ClApply(cl : Closure) : REFANY =

  PROCEDURE Await(txt : TEXT) =
    BEGIN
      LOOP
        WITH line    = Rd.GetLine(cmdRd) DO
          IF TE(line, txt) THEN RETURN END
        END
      END
    END Await;

  PROCEDURE PushTask(task : Task) =
    BEGIN
      Wr.PutText  (cmdWr, F("P %s %s %s\n",
                            Int(task.nodeId),
                            LR(task.relPrec),
                            Int(task.code)));
      Pickle.Write(cmdWr, task.op);
      Wr.Flush    (cmdWr)
    END PushTask;

  PROCEDURE ReceiveResult(VAR nodeId : Trace.NodeId;
                          VAR norm   : SpiceCompress.Norm) : TEXT =
    BEGIN
      RETURN FsdbComms.ReadCompressedNodeDataG(cmdRd, nodeId, norm)
    END ReceiveResult;
    
  VAR
    cmdStdin   : ProcUtils.Reader;
    cmdStdout  : ProcUtils.Writer;
    completion : ProcUtils.Completion;
    cmdWr      : Wr.T;
    cmdRd      : Rd.T;

    task       : Task;
    resId      : CARDINAL;
  BEGIN
    <*FATAL OSError.E*>
    BEGIN
      cmdStdin  := ProcUtils.GimmeWr(cmdWr);
      cmdStdout := ProcUtils.GimmeRd(cmdRd);
    END;

    WITH cmd = F("%s -slave -root %s",
                 cl.t.cmdPath,
                 cl.t.root) DO
      Debug.Out(F("Distrewriter.ClApply running %s", cmd));
      
      completion := ProcUtils.RunText(cmd,
                                      stdin  := cmdStdin,
                                      stderr := ProcUtils.Stderr(),
                                      stdout := cmdStdout)
    END;

    TRY
      Await("READY");

      Debug.Out("DistRewriter.ClApply : slave is ready");

      LOCK cl.t.mu DO
        INC(cl.t.running);
        Thread.Broadcast(cl.t.c)
      END;

      LOOP
        LOCK cl.t.mu DO
          WHILE cl.t.cmdseq.size() = 0 DO
            Thread.Wait(cl.t.mu, cl.t.c)
          END;

          task := cl.t.cmdseq.remlo()
        END;

        PushTask(task);
        VAR
          line   := Rd.GetLine(cmdRd);
          reader := NEW(TextReader.T).init(line);
          result : TEXT;
          kw     : TEXT;
        BEGIN
          IF reader.next(" ", kw, TRUE) THEN
            IF TE(kw, "AAA") THEN
              result := ReceiveResult(resId, task.norm);
              <*ASSERT resId = task.nodeId*>
            ELSIF TE(kw, "PPP") THEN
              VAR
                len := reader.getInt();
                buff := NEW(REF ARRAY OF CHAR, len);
              BEGIN
                WITH got = Rd.GetSub(cmdRd, buff^) DO
                  <*ASSERT got = len*>
                END;
                result := Text.FromChars(buff^);
                task.code := ArithConstants.Pickle;
              END
            END
          END;
          
          task.result := result;
          
          LOCK cl.t.mu DO
            cl.t.resseq.addhi(task);
            Thread.Broadcast(cl.t.c)
          END
        END
      END
    EXCEPT
    END; 
    RETURN NIL
  END ClApply;
  
(**********************************************************************)

PROCEDURE RunSlave(root : Pathname.T) =
  VAR
    rd := Stdio.stdin;
    tr := NEW(Trace.T).init(root);
    kw : TEXT;
  BEGIN

    (* a little bit of software engineering here would make this 
       a lot easier.

       We could 

       1.
       pipeline the master side of the operation, to allow
       multiple jobs to be stuffed down the pipe before the first has
       returned.

       2. 
       parallelize the slave code so that the reading and writing
       over the pipes is sequential, as well as the reading the 
       Trace structure, but the compression (and only
       the compression) is done in parallel.

       This should allow the memory of the slave machines to be shared
       well.  We could possibly get 10X speedups on the slave for
       some tasks.  Unless the computation is the heavy part, in
       which case that too can be parallelized.  (The Trace file cannot
       be parallelized, nor can the comms with the master.)

    *)
    
    Wr.PutText(Stdio.stdout, "READY\n"); Wr.Flush(Stdio.stdout);
    TRY
      LOOP
        WITH line   = Rd.GetLine(rd),
             reader = NEW(TextReader.T).init(line) DO
          IF doDebug THEN
            Debug.Out(F("RewriterMain.RunSlave line \"%s\"", line));
          END;
          IF reader.next(" ", kw, TRUE) THEN
            IF TE(kw, "P") THEN
              WITH id        = reader.getInt(),
                   prec      = reader.getLR(),
                   code      = reader.getInt(),
                   ref       = Pickle.Read(rd),
                   arr       = NEW(REF ARRAY OF LONGREAL, tr.getSteps()) DO
                
                <*ASSERT ISTYPE(ref, TraceOp.T)*>
                
                Debug.Out("DistRewriter.RunSlave : ready to execute task");

                TYPECASE ref OF
                  TraceOp.Array(arrOp) =>
                  
                  arrOp.exec(tr, arr^);

                  Wr.PutText(Stdio.stdout, "AAA\n");
                  Debug.Out("DistRewriter.RunSlave : TraceOp.Array task done, writing to master");
                  
                  DistZTrace.WriteOut(Stdio.stdout,
                                      arr^,
                                      id,
                                      FALSE,
                                      prec,
                                      FALSE,
                                      code);
                  Wr.Flush(Stdio.stdout);

                |
                  TraceOp.Pickle(pklOp) =>
                  Debug.Out("DistRewriter.RunSlave : TraceOp.Pickle, will execute");

                  (* we need to use a text writer to get the length *)
                  WITH textWr = TextWr.New() DO
                    pklOp.exec(tr, textWr);
                    WITH pTxt = TextWr.ToText(textWr) DO
                      Wr.PutText(Stdio.stdout, F("PPP %s\n",
                                                 Int(Text.Length(pTxt))));
                      Wr.PutText(Stdio.stdout, pTxt)
                    END
                  END;
                  Wr.Flush(Stdio.stdout)
                END;

                Debug.Out("DistRewriter.RunSlave : writing to master complete!");

              END
            END
          END
        END
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    END
  END RunSlave;

BEGIN END DistRewriter.
