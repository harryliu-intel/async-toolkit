(* $Id: ProcUtils.m3,v 1.21 2011/02/17 21:07:02 mika Exp $ *)

MODULE ProcUtils;

(* wrapper to simplify starting and running of Unix processes under
   Modula-3.  Authors: Karl Papadantonakis, Mika Nystrom *)

IMPORT FS;
IMPORT File;
IMPORT FileRd;
IMPORT FileWr;
IMPORT Fmt;
IMPORT Pathname;
IMPORT Pipe;
IMPORT Process;
IMPORT Rd;
IMPORT Stdio;
IMPORT Text;
IMPORT TextList;
IMPORT TextRd;
IMPORT Thread;
IMPORT Wr;
IMPORT OSError;
IMPORT Atom;
IMPORT AL;
IMPORT Debug;
IMPORT Time;
IMPORT Usignal;

<* FATAL Thread.Alerted *>

VAR DoDebug := Debug.DebugThis("PROCUTILS");

TYPE
  PrivateCompletion = Completion OBJECT
    po, pe: Writer;
    pi: Reader;
    main: MainClosure;
    th: Thread.T;
  OVERRIDES
    wait := Wait;
  END;

  MainClosure = Thread.Closure OBJECT
    c: PrivateCompletion;
    src: Rd.T;
    wd0: Pathname.T;
    created := FALSE;
    mu : MUTEX;
    cn : Thread.Condition;
    sub : Process.T := NIL;
  OVERRIDES
    apply := Apply;
  END;

  SSClosure = Thread.Closure OBJECT ss: SS; OVERRIDES apply:=SSApply; END;

PROCEDURE SSApply(self: SSClosure): REFANY =
  BEGIN
    TRY
      LOOP
        Wr.PutChar(self.ss.wr,Rd.GetChar(self.ss.rd));
        Wr.PutText(self.ss.wr,Rd.GetText(self.ss.rd,
                                         Rd.CharsReady(self.ss.rd)));
      END;
    EXCEPT 
      Rd.EndOfFile => 
      IF DoDebug THEN Debug.Out("SSApply exiting on Rd.EndOfFile") END;
      TRY Rd.Close(self.ss.rd) EXCEPT ELSE END;
      RETURN NIL
    |
      Rd.Failure, Wr.Failure => 
         Process.Crash("I/O Error in ProcUtils.SSApply.")
    END;
    <* ASSERT FALSE *>
  END SSApply;

PROCEDURE ForkWriter(w: Writer): Writer =
  BEGIN
    IF DoDebug THEN Debug.Out("ForkWriter") END;
    IF w # NIL AND w.ss # NIL THEN
      w.th := Thread.Fork(NEW(SSClosure, ss:=w.ss, apply := SSApply));
      IF DoDebug THEN Debug.Out("ForkWriter w.th = 16_" & Debug.FmtPointer(w.th)) END;
    END;
    RETURN w;
  END ForkWriter;

PROCEDURE ForkReader(r: Reader): Reader =
  BEGIN
    IF r # NIL AND r.ss # NIL THEN
      EVAL Thread.Fork(NEW(SSClosure, ss:=r.ss, apply := SSApply));
    END;
    RETURN r;
  END ForkReader;

PROCEDURE RunText(source: TEXT;
                  stdout,stderr: Writer;
                  stdin: Reader;
                  wd0: Pathname.T): Completion =
  BEGIN RETURN Run(TextRd.New(source),stdout,stderr,stdin,wd0) END RunText;

PROCEDURE Run(source: Rd.T;
              stdout,stderr: Writer := NIL;
              stdin: Reader := NIL;
              wd0: Pathname.T := NIL): Completion =
  VAR
    c := NEW(PrivateCompletion,
             po := ForkWriter(stdout),
             pe := ForkWriter(stderr),
             pi := ForkReader(stdin),
             main := NEW(MainClosure, 
                         src:=source, 
                         wd0:=wd0,
                         mu := NEW(MUTEX), 
                         cn := NEW(Thread.Condition)));
  BEGIN
    c.main.c := c;
    c.th := Thread.Fork(c.main);
    IF DoDebug THEN Debug.Out("Run : c.th = 16_" & Debug.FmtPointer(c.th)) END;

    LOCK c.main.mu DO
      (* if we dont wait for the process to be created here we get a very 
         nasty race condition!  The Pipe will have zero writers before this
         point.  That means that it will immediately hit EOF if it is read
         from!  So be sure the pipe has a writer before it is passed back to
         the calling process! *)
      WHILE NOT c.main.created DO
        Thread.Wait(c.main.mu, c.main.cn)
      END
    END;

    IF DoDebug THEN Debug.Out("Run : process has been created, continuing...") END;
    
    RETURN c;
  END Run;

PROCEDURE Apply(self: MainClosure): REFANY =
  CONST
    White = SET OF CHAR{' ','\t'};
    Break = SET OF CHAR{'\n',';'};
    Break1 = SET OF CHAR{'\n',';','|'};
    WB = White + Break;
    Special = SET OF CHAR{'|','&','<','>'} + WB;
    ReDir = SET OF CHAR{'<','>'};
  VAR
    wd := self.wd0;
    cm := self.c;
    rd := self.src;
    c: CHAR;
    p: TEXT;
    i2, stdout,stderr,stdin: File.T := NIL;
    rMode: CHAR;
    l: TextList.T;

  PROCEDURE PutArg() RAISES { OSError.E } =
    BEGIN
        CASE rMode OF
        | '>' => stdout := FS.OpenFile(p);
        | '&' => stdout := FS.OpenFile(p); stderr := stdout;
        | '<' => stdin := FS.OpenFileReadonly(p);
        ELSE
          l := TextList.Cons(p, l);
        END
    END PutArg;

  PROCEDURE Exec() RAISES { Rd.Failure, OSError.E, ErrorExit } =
    BEGIN
      IF c = '|' THEN
        TRY
          c := Rd.GetChar(rd);
          WHILE c IN White DO c := Rd.GetChar(rd); END;
          VAR
            r,w: Pipe.T;
          BEGIN
            Pipe.Open(r,w);
            i2 := r;
            stdout := w;
          END;
          IF c = '&' THEN
            c := Rd.GetChar(rd);
            stderr := stdout;
          END;
        EXCEPT Rd.EndOfFile => <* ASSERT FALSE *>
        END;
      ELSE
        IF cm.pi = NIL THEN i2:=NIL ELSE i2:=cm.pi.f; END;
      END;
      VAR
        params := NEW(REF ARRAY OF TEXT, TextList.Length(l)-1);
      BEGIN
        FOR i := LAST(params^) TO 0 BY -1 DO
          params[i] := l.head;
          l := l.tail;
        END;
        IF Text.Equal(l.head, "cd") THEN
          wd := Pathname.Join(wd, l.head);
        ELSE
          TRY
            IF DoDebug THEN
              Debug.Out("ProcUtils.Apply.Exec: running command: " & l.head);
              FOR i := FIRST(params^) TO LAST(params^) DO
                Debug.Out("ProcUtils.Apply.Exec: params[" & Fmt.Int(i) & "] : " & 
                  params[i])
              END
            END;

            VAR
              code : Process.ExitCode;
              sub : Process.T;
            BEGIN
              IF DoDebug THEN
                Debug.Out("ProcUtils.Apply.Exec: creating subprocess")
              END;
              TRY 
                sub := Process.Create(l.head, params^,
                                      NIL, wd,
                                      stdin, stdout,stderr);
              EXCEPT
                OSError.E(x) =>
                  Debug.Out("Got exception OSError.E in Process.Create(" & l.head & "...) : " & AL.Format(x));
                  RAISE OSError.E(x)
              END;
              IF DoDebug THEN
                Debug.Out("ProcUtils.Apply.Exec: created subprocess")
              END;

              LOCK self.mu DO
                (* mark process as created so that requester can continue 
                   
                   This routine is NOT allowed to continue until the Pipe
                   between the subprocess and the calling process has been
                   opened for writing by the subprocess, which happens
                   in Process.Create.
                *)
                self.sub := sub;
                self.created := TRUE;
                Thread.Signal(self.cn)
              END;

              IF DoDebug THEN
                Debug.Out("ProcUtils.Apply.Exec: waiting for subprocess")
              END;

              code := Process.Wait(sub);

              IF DoDebug THEN
                Debug.Out("ProcUtils.Apply.Exec: subprocess returned " & 
                  Fmt.Int(code))
              END;

              (* we need to attempt closing all three of stdin, stdout,
                 and stderr.  Only the LAST exception, if any, will be
                 reported. (right now none are reported) *)
              IF DoDebug THEN Debug.Out("Closing descriptors") END;

              IF stdin  # NIL THEN TRY stdin .close() EXCEPT ELSE 
                IF DoDebug THEN Debug.Out("cant close stdin") END
              END END;

              IF stdout # NIL THEN TRY stdout.close() EXCEPT ELSE 
                IF DoDebug THEN Debug.Out("cant close stdout") END
              END END;

              IF stderr # NIL THEN TRY stderr.close() EXCEPT ELSE 
                IF DoDebug THEN Debug.Out("cant close stderr") END
              END END;


              IF DoDebug THEN Debug.Out("Descriptors closed") END;

              IF code # 0 THEN
                IF DoDebug THEN Debug.Out("Process exited with code " & 
                  Fmt.Int(code)) 
                END;
                RAISE ErrorExit(NEW(ExitCode, code := code))
              END;

              IF DoDebug THEN Debug.Out("Exec done") END;
            END
          EXCEPT
            OSError.E(e) => 
              RAISE ErrorExit(NEW(OS, al := e))
          END
        END
      END
    END Exec;

  <*FATAL Timeout*>
  BEGIN
    IF wd = NIL THEN wd := "."; END;
    TRY
      TRY
      TRY
        (* default input *)
        IF cm.pi = NIL THEN i2:=NIL ELSE i2:=cm.pi.f; END;
        c := Rd.GetChar(rd);
        (* loop invariant:
           c is the character just before
           the mark,
           and everything before that has been processed and then added to p
        *)
        LOOP
          (* set up default i/o *)
          stdin := i2;
          IF cm.po # NIL THEN stdout := cm.po.f; END;
          IF cm.pe # NIL THEN stderr := cm.pe.f; END;
          l := NIL;
          WHILE c IN WB DO c := Rd.GetChar(rd); END;
          REPEAT
            IF c IN ReDir THEN
              rMode := c;
              IF c = '>' THEN
                WHILE c IN White DO c := Rd.GetChar(rd); END;
                IF c = '&' THEN
                  c := Rd.GetChar(rd);
                  rMode := '&';
                END;
              END;
            ELSE
              rMode := '-';
            END;
            p := "";
            CASE c OF
            | '\'' =>
              c := Rd.GetChar(rd);
              WHILE c # '\'' DO p:=p&Fmt.Char(c); c := Rd.GetChar(rd) END;
              c := Rd.GetChar(rd) (* to maintain loop invariant *)
            | '`' =>
              c := Rd.GetChar(rd);
              WHILE c # '`' DO p:=p&Fmt.Char(c); c := Rd.GetChar(rd) END;
              c := Rd.GetChar(rd); (* to maintain loop invariant *)
              p := ToText(p, wd0:=wd);
            ELSE
              WHILE NOT c IN Special DO p:=p&Fmt.Char(c); c:=Rd.GetChar(rd); END;
            END;
            WHILE c IN White DO c := Rd.GetChar(rd); END;
            PutArg();
          UNTIL c IN Break1;
          IF DoDebug THEN Debug.Out("Calling Exec") END;
          Exec();
          IF DoDebug THEN Debug.Out("Exec Returned") END;
        END
      EXCEPT Rd.EndOfFile =>
        IF DoDebug THEN
          Debug.Out("ProcUtils.Apply : short read, putting semicolon")
        END;
        PutArg();
        c := ';';
        Exec();
      END;
      
      FINALLY
        (* close i/o *)
        IF DoDebug THEN 
          Debug.Out("ProcUtils.Apply: finally clause : close i/o");
        END;

        IF DoDebug THEN Debug.Out("ProcUtils.Apply: closing cm.po") END;

        IF cm.po#NIL AND cm.po.close THEN 
          TRY cm.po.f.close() EXCEPT ELSE END; 
          (*
          IF cm.po.aux # NIL THEN
            TRY cm.po.aux.close() EXCEPT ELSE END 
          END
          *)
        END;

        IF DoDebug THEN Debug.Out("ProcUtils.Apply: closing cm.pe") END;

        IF cm.pe#NIL AND cm.pe.close AND cm.po#cm.pe THEN 
          TRY cm.pe.f.close() EXCEPT ELSE END;
          (*
          IF cm.pe.aux # NIL THEN
            TRY cm.pe.aux.close() EXCEPT ELSE END 
          END
          *)
        END;

        IF DoDebug THEN Debug.Out("ProcUtils.Apply: closing cm.pi") END;

        IF cm.pi#NIL AND cm.pi.close THEN 
          TRY cm.pi.f.close() EXCEPT ELSE END;
          (*
          IF cm.pi.aux # NIL THEN
            TRY cm.pi.aux.close() EXCEPT ELSE END 
          END
          *)
        END
      END;
      RETURN NIL;
    EXCEPT
      ErrorExit(ee) => RETURN ee
    |
      OSError.E(e) => 
        RETURN NEW(OS, error := FormatOSError(e))
    |
      Rd.Failure(e) => 
        RETURN NEW(Error, error := FormatOSError(e))
    END
  END Apply;

PROCEDURE Wait(c: PrivateCompletion) RAISES { ErrorExit } =
  VAR
    r : REFANY;
  BEGIN
    IF DoDebug THEN Debug.Out("c.th = 16_" & Debug.FmtPointer(c.th)) END;
    r := Thread.Join(c.th);
    IF DoDebug THEN Debug.Out("Wait") END;
    IF c.po # NIL AND c.po.th # NIL THEN EVAL Thread.Join(c.po.th) END; 
    IF c.pe # NIL AND c.pe.th # NIL THEN EVAL Thread.Join(c.pe.th) END;

    (* we don't need to Join std input, that's the child's problem *)

    IF r # NIL AND ISTYPE(r,Error) THEN 
      IF DoDebug THEN
        Debug.Out("ProcUtils.Wait: Raising ErrorExit: " & FormatError(r))
      END;
      RAISE ErrorExit(r) 
    END

  END Wait;

(* Helpers *)

PROCEDURE ToText(source: T;
                 stderr:  Writer := NIL;
                 stdin: Reader := NIL;
                 wd0: Pathname.T := NIL;
                 timeout := LAST(Time.T)): TEXT
  RAISES { Rd.Failure, ErrorExit, OSError.E, Timeout } =
  VAR
    rd: Rd.T;
    srcRd := TextRd.New(source);
    comp : PrivateCompletion;
    res : TEXT;
    watchdog : Thread.T := NIL;
    alerted := FALSE;
  BEGIN
    comp := RdToRd(srcRd, stderr, stdin, wd0, rd);
    IF DoDebug THEN Debug.Out("Reading out rd") END;

    (* the sequence below is curious.

       first, we read from the output of the child process.  RdToRd guarantees 
       there is a writer to the pipe.  Since we read to LAST(INTEGER), the
       subprocess has to close before we can return to this procedure.

       THEN, we wait for the subprocess to exit and its puppeteering thread
       to exit.

       Finally we close the reader.  If something went wrong and the sequence
       happened incorrectly, the writer will deadlock before this since we
       did not read from it! 
    *)

    (* start watchdog if requested *)
    IF timeout # LAST(Time.T) THEN
      WITH et = Time.Now() + timeout DO
        watchdog := Thread.Fork(NEW(WatchdogCl,
                                    killSub := comp.main.sub,
                                    alertTh := Thread.Self(),
                                    alertAt := et))
      END
    END;

    TRY
      IF DoDebug THEN Debug.Out("ProcUtils.ToText: Calling Rd.GetText") END;

      (* this doesnt seem to work as expected: even if this thread 
         is alerted here, Rd.GetText doesn't return! *)
      
      res := Rd.GetText(rd, LAST(INTEGER));
      IF Thread.TestAlert() THEN
        alerted := TRUE
      END;
      IF DoDebug THEN Debug.Out("ProcUtils.ToText: Rd.GetText returned alerted=" & Fmt.Bool(alerted)) END
    EXCEPT
      Thread.Alerted =>
      IF DoDebug THEN Debug.Out("ProcUtils.ToText: Rd.GetText alerted") END;
      alerted := TRUE
    END;

    (* kill the watchdog *)
    IF watchdog # NIL THEN
      Thread.Alert(watchdog);
      EVAL Thread.Join(watchdog);
      IF DoDebug THEN Debug.Out("ProcUtils.ToText: Killed watchdog") END
    END;
    
    IF DoDebug THEN Debug.Out("ProcUtils.ToText: Calling comp.wait()") END;
    comp.wait();
    (* because of how the watchdog works...(it kills the subprocess
       and attempts to alert us) 
       if we get a ProcUtils.ErrorExit here, maybe we should catch it,
       check whether TestAlert is true.  If so, raise Timeout.
       Else re-raise ErrorExit *)
       
    
    IF DoDebug THEN Debug.Out("ProcUtils.ToText: comp.wait() done") END;
    TRY Rd.Close(rd) EXCEPT ELSE END;
    IF DoDebug THEN Debug.Out("ProcUtils.ToText: Rd.Close done") END;
    IF alerted THEN
      RAISE Timeout
    ELSE
      RETURN res;
    END
  END ToText;

TYPE
  WatchdogCl = Thread.Closure OBJECT
    alertTh : Thread.T;
    alertAt : Time.T;
    killSub : Process.T;
  OVERRIDES
    apply := WDApply;
  END;

PROCEDURE WDApply(cl : WatchdogCl) : REFANY =
  BEGIN
    IF DoDebug THEN Debug.Out("Watchdog started") END;
    TRY
      LOOP
        WITH now = Time.Now() DO
          IF DoDebug THEN Debug.Out(Fmt.F("Watchdog now %s deadline %s wait %s",
                                      Fmt.LongReal(now),
                                      Fmt.LongReal(cl.alertAt),
                                      Fmt.LongReal(cl.alertAt-now))) END;
          IF now < cl.alertAt THEN
            Thread.AlertPause(cl.alertAt - now)
          ELSE
            IF DoDebug THEN Debug.Out("Watchdog alerting") END;
            Thread.Alert(cl.alertTh);

            WITH subId = Process.GetID(cl.killSub) DO
              IF DoDebug THEN Debug.Out("Watchdog killing sub: INT") END;
              EVAL Usignal.kill(subId, Usignal.SIGINT);
              Thread.Pause(1.0d0);
              IF DoDebug THEN Debug.Out("Watchdog killing sub: KILL") END;
              EVAL Usignal.kill(subId, Usignal.SIGKILL);
              IF DoDebug THEN Debug.Out("Watchdog done") END
            END;

            RETURN NIL
          END
        END
      END
    EXCEPT
      Thread.Alerted => RETURN NIL
    END
  END WDApply;

PROCEDURE RdToRd(source: Rd.T;
                 stderr: Writer := NIL;
                 stdin: Reader := NIL;
                 wd0: Pathname.T := NIL;
                 VAR rd: Rd.T): Completion RAISES { OSError.E } =
  VAR
    myWriter := GimmeRd(rd);
  BEGIN
    IF DoDebug THEN Debug.Out("calling Run") END;
    WITH cpl = Run(source, myWriter, stderr, stdin, wd0) DO
      IF DoDebug THEN Debug.Out("Run returned") END;
      (*TRY myWriter.f.close(); EXCEPT ELSE END;*)
      RETURN cpl
    END
  END RdToRd;



(* I/O control *)

REVEAL
  Reader = BRANDED "ProcUtilRd" OBJECT 
    f, aux: File.T := NIL; close: BOOLEAN; ss: SS; END;
  Writer = BRANDED "ProcUtilWr" OBJECT 
    f, aux: File.T := NIL; close: BOOLEAN; ss: SS; th : Thread.T END;

TYPE
  SS = OBJECT rd: Rd.T; wr: Wr.T; END;

PROCEDURE WriteHere(wr: Wr.T): Writer RAISES { OSError.E } =
  BEGIN
    IF wr = Stdio.stdout THEN RETURN Stdout();
    ELSIF wr = Stdio.stderr THEN RETURN Stderr();
    ELSE
      VAR
        rd: Rd.T;
        w := GimmeRd(rd);
      BEGIN
        w.ss := NEW(SS,rd:=rd,wr:=wr);
        RETURN w;
      END;
    END;
  END WriteHere;

PROCEDURE GimmeRd(VAR rd: Rd.T): Writer RAISES { OSError.E } =
  VAR
    hr,hw: Pipe.T;
  BEGIN
    Pipe.Open(hr, hw);
    rd := NEW(FileRd.T).init(hr);
    RETURN NEW(Writer,f:=hw,ss:=NIL,aux := hr,close:=TRUE);
  END GimmeRd;

PROCEDURE Stdout(): Writer =
  BEGIN
    RETURN NEW(Writer,f:=so,ss:=NIL,close:=FALSE);
  END Stdout;

PROCEDURE Stderr(): Writer =
  BEGIN
    RETURN NEW(Writer,f:=se,ss:=NIL,close:=FALSE);
  END Stderr; 

PROCEDURE ReadHere(rd: Rd.T): Reader  RAISES { OSError.E } =
  BEGIN
    IF rd = Stdio.stdin THEN RETURN Stdin();
    ELSE
      VAR
        wr: Wr.T;
        r := GimmeWr(wr);
      BEGIN
        r.ss := NEW(SS,rd:=rd,wr:=wr);
        RETURN r;
      END;
    END;
  END ReadHere;

PROCEDURE ReadThis(t: TEXT): Reader  RAISES { OSError.E } =
  BEGIN
    RETURN ReadHere(TextRd.New(t));
  END ReadThis;

PROCEDURE GimmeWr(VAR wr: Wr.T): Reader RAISES { OSError.E } =
  VAR
    hr,hw: Pipe.T;
  BEGIN
    Pipe.Open(hr, hw);
    wr := NEW(FileWr.T).init(hw);
    RETURN NEW(Reader,f:=hr,ss:=NIL,aux:=hw,close:=TRUE);
  END GimmeWr;

PROCEDURE Stdin(): Reader =
  BEGIN
    RETURN NEW(Reader,f:=si,ss:=NIL,close:=FALSE)
  END Stdin; 

PROCEDURE FormatOSError(e : OSError.Code) : TEXT =
  VAR
    res := "";
  BEGIN
    WHILE e # NIL DO
      res := res & Atom.ToText(e.head);
      IF e.tail # NIL THEN res := res & " " END;
      e := e.tail
    END;
    RETURN res
  END FormatOSError;

PROCEDURE FormatError(e : Error) : TEXT =
  BEGIN
    TYPECASE e OF
      OS(os) => RETURN "ProcUtils.Error.OS: " & UnNil(e.error) & "; " & AL.Format(os.al)
    |
      ExitCode(ec) => RETURN "ProcUtils.Error.ExitCode: " & UnNil(e.error) & 
        " exitCode=" & Fmt.Int(ec.code)
    |
      Error => RETURN "ProcUtils.Error.Unknown: " & UnNil(e.error)
    END
  END FormatError;

PROCEDURE UnNil(txt : TEXT) : TEXT = 
  BEGIN IF txt = NIL THEN RETURN "**NIL**" ELSE RETURN txt END END UnNil;

VAR
  so,si,se: File.T;
BEGIN
  Process.GetStandardFileHandles(si,so,se);
END ProcUtils.
