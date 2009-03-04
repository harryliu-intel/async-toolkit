(* $Id$ *)

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
IMPORT Wr, TextWr;
IMPORT OSError;
IMPORT Atom;
IMPORT AL;

<* FATAL Thread.Alerted *>


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
  OVERRIDES
    apply := Apply;
  END;

  SSClosure = Thread.Closure OBJECT ss: SS; OVERRIDES apply:=SSApply; END;

PROCEDURE DebugFormat(cmd : TEXT; READONLY params : ARRAY OF TEXT) : TEXT =
  <* FATAL Wr.Failure *>
  VAR
    wr := NEW(TextWr.T).init();
  BEGIN
    Wr.PutText(wr, cmd);
    FOR i := 0 TO LAST(params) DO
      Wr.PutChar(wr, ' ');
      Wr.PutText(wr, params[i])
    END;
    RETURN TextWr.ToText(wr)
  END DebugFormat;

PROCEDURE SSApply(self: SSClosure): REFANY =
  BEGIN
    TRY
      LOOP
        Wr.PutChar(self.ss.wr,Rd.GetChar(self.ss.rd));
        Wr.PutText(self.ss.wr,Rd.GetText(self.ss.rd,
                                         Rd.CharsReady(self.ss.rd)));
      END;
    EXCEPT 
      Rd.EndOfFile => RETURN NIL;
    |
      Rd.Failure, Wr.Failure => 
         Process.Crash("I/O Error in ProcUtils.SSApply.")
    END;
    <* ASSERT FALSE *>
  END SSApply;

PROCEDURE ForkWriter(w: Writer): Writer =
  BEGIN
    IF w # NIL AND w.ss # NIL THEN
      EVAL Thread.Fork(NEW(SSClosure, ss:=w.ss, apply := SSApply));
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
             main := NEW(MainClosure, src:=source, wd0:=wd0));
  BEGIN
    c.main.c := c;
    c.th := Thread.Fork(c.main);
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

  PROCEDURE PutArg() =
    BEGIN
      TRY
        CASE rMode OF
        | '>' => stdout := FS.OpenFile(p);
        | '&' => stdout := FS.OpenFile(p); stderr := stdout;
        | '<' => stdin := FS.OpenFileReadonly(p);
        ELSE
          l := TextList.Cons(p, l);
        END
      EXCEPT
        OSError.E(e) => Process.Crash("ProcUtils.Run.PutArg: problems opening file \"" & p & "\"!" & "\n" & FormatOSError(e))
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
            VAR
              code := Process.Wait(Process.Create(l.head, params^,
                                                  NIL, wd,stdin, stdout,stderr));
            BEGIN
              IF code # 0 THEN
                RAISE ErrorExit(NEW(ExitCode, code := code))
(*
                Debug.Error("Process exited with code " & Fmt.Int(code) & 
                  "\ncommand: \"" & DebugFormat(l.head,params^) & "\"")
*)
              END
            END
          EXCEPT
            OSError.E(e) => 
              RAISE ErrorExit(NEW(OS, al := e))
          END
        END
      END
    END Exec;

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
          Exec()
        END
      EXCEPT Rd.EndOfFile =>
        PutArg();
        c := ';';
        Exec();
      END;
      
      FINALLY
      (* close i/o *)
        IF cm.po#NIL AND cm.po.close THEN 
          TRY cm.po.f.close() EXCEPT ELSE END 
        END;
        IF cm.pe#NIL AND cm.pe.close AND cm.po#cm.pe THEN 
          TRY cm.pe.f.close() EXCEPT ELSE END
        END;
        IF cm.pi#NIL AND cm.pi.close THEN 
          TRY cm.pi.f.close() EXCEPT ELSE END
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
    r := Thread.Join(c.th);
  BEGIN
    IF r # NIL AND ISTYPE(r,Error) THEN RAISE ErrorExit(r) END
  END Wait;


(* Helpers *)

PROCEDURE ToText(source: T;
                 stderr:  Writer := NIL;
                 stdin: Reader := NIL;
                 wd0: Pathname.T := NIL): TEXT RAISES { Rd.Failure, ErrorExit } =
  VAR
    rd: Rd.T;
    comp := RdToRd(TextRd.New(source), stderr, stdin, wd0, rd);
    res := Rd.GetText(rd, LAST(INTEGER));
  BEGIN
    comp.wait();
    TRY Rd.Close(rd) EXCEPT ELSE END;
    RETURN res;
  END ToText;

PROCEDURE RdToRd(source: Rd.T;
                 stderr: Writer := NIL;
                 stdin: Reader := NIL;
                 wd0: Pathname.T := NIL;
                 VAR rd: Rd.T): Completion =
  VAR
    myWriter := GimmeRd(rd);
  BEGIN
    RETURN Run(source, myWriter, stderr, stdin, wd0)
  END RdToRd;



(* I/O control *)

REVEAL
  Reader = BRANDED "ProcUtilRd" OBJECT f: File.T; close: BOOLEAN; ss: SS; END;
  Writer = BRANDED "ProcUtilWr" OBJECT f: File.T; close: BOOLEAN; ss: SS; END;

TYPE
  SS = OBJECT rd: Rd.T; wr: Wr.T; END;

PROCEDURE WriteHere(wr: Wr.T): Writer =
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

PROCEDURE GimmeRd(VAR rd: Rd.T): Writer =
  VAR
    hr,hw: Pipe.T;
  BEGIN
    TRY
      Pipe.Open(hr, hw);
      rd := NEW(FileRd.T).init(hr)
    EXCEPT 
      OSError.E(e) => Process.Crash("ProcUtils.GimmeRd: trouble opening pipe!"& "\n" & FormatOSError(e))
    END;
    RETURN NEW(Writer,f:=hw,ss:=NIL,close:=TRUE);
  END GimmeRd;

PROCEDURE Stdout(): Writer =
  BEGIN
    RETURN NEW(Writer,f:=so,ss:=NIL,close:=FALSE);
  END Stdout;

PROCEDURE Stderr(): Writer =
  BEGIN
    RETURN NEW(Writer,f:=se,ss:=NIL,close:=FALSE);
  END Stderr; 

PROCEDURE ReadHere(rd: Rd.T): Reader =
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

PROCEDURE ReadThis(t: TEXT): Reader =
  BEGIN
    RETURN ReadHere(TextRd.New(t));
  END ReadThis;

PROCEDURE GimmeWr(VAR wr: Wr.T): Reader =
  VAR
    hr,hw: Pipe.T;
  BEGIN
    TRY
      Pipe.Open(hr, hw);
      wr := NEW(FileWr.T).init(hw)
    EXCEPT
      OSError.E(e) => Process.Crash("ProcUtils.GimmeWr: trouble opening pipe!"& "\n" & FormatOSError(e))
    END;
    RETURN NEW(Reader,f:=hr,ss:=NIL,close:=TRUE);
  END GimmeWr;

PROCEDURE Stdin(): Reader =
  BEGIN
    RETURN NEW(Reader,f:=si,ss:=NIL,close:=FALSE);
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
