MODULE ProcUtils;
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

<* FATAL Rd.Failure, Wr.Failure, Thread.Alerted, OSError.E *>

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

PROCEDURE SSApply(self: SSClosure): REFANY =
  BEGIN
    TRY
      LOOP
        Wr.PutChar(self.ss.wr,Rd.GetChar(self.ss.rd));
        Wr.PutText(self.ss.wr,Rd.GetText(self.ss.rd,
                                         Rd.CharsReady(self.ss.rd)));
      END;
    EXCEPT Rd.EndOfFile => RETURN NIL;
    END;
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
      CASE rMode OF
      | '>' => stdout := FS.OpenFile(p);
      | '&' => stdout := FS.OpenFile(p); stderr := stdout;
      | '<' => stdin := FS.OpenFileReadonly(p);
      ELSE
        l := TextList.Cons(p, l);
      END;
    END PutArg;

  PROCEDURE Exec() =
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
          EVAL Process.Wait(Process.Create(l.head, params^,
                                           NIL, wd, stdin, stdout, stderr));
        END;
      END;
    END Exec;

  BEGIN
    IF wd = NIL THEN wd := "."; END;
    TRY
      (* default input *)
      IF cm.pi = NIL THEN i2:=NIL ELSE i2:=cm.pi.f; END;
      c := Rd.GetChar(rd);
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
            WHILE c # '\'' DO p:=p&Fmt.Char(c); END;
          | '`' =>
            WHILE c # '`' DO p:=p&Fmt.Char(c); END;
            p := ToText(p, wd0:=wd);
          ELSE
            WHILE NOT c IN Special DO p:=p&Fmt.Char(c); c:=Rd.GetChar(rd); END;
          END;
          WHILE c IN White DO c := Rd.GetChar(rd); END;
          PutArg();
        UNTIL c IN Break1;
        Exec();
      END;
    EXCEPT Rd.EndOfFile =>
      PutArg();
      c := ';';
      Exec();
    END;

    (* close i/o *)
    IF cm.po#NIL AND cm.po.close THEN cm.po.f.close(); END;
    IF cm.pe#NIL AND cm.pe.close AND cm.po#cm.pe THEN cm.pe.f.close(); END;
    IF cm.pi#NIL AND cm.pi.close THEN cm.pi.f.close(); END;
    RETURN NIL;
  END Apply;

PROCEDURE Wait(c: PrivateCompletion) =
  BEGIN
    EVAL Thread.Join(c.th);
  END Wait;


(* Helpers *)

PROCEDURE ToText(source: T;
                 stderr:  Writer := NIL;
                 stdin: Reader := NIL;
                 wd0: Pathname.T := NIL): TEXT =
  VAR
    rd: Rd.T;
    comp := RdToRd(TextRd.New(source), stderr, stdin, wd0, rd);
    res := Rd.GetText(rd, LAST(INTEGER));
  BEGIN
    comp.wait();
    RETURN res;
  END ToText;

PROCEDURE RdToRd(source: Rd.T;
                 stderr: Writer := NIL;
                 stdin: Reader := NIL;
                 wd0: Pathname.T := NIL;
                 VAR rd: Rd.T): Completion =
  BEGIN
    RETURN Run(source, GimmeRd(rd), stderr, stdin, wd0);
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
    Pipe.Open(hr, hw);
    rd := NEW(FileRd.T).init(hr);
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
    Pipe.Open(hr, hw);
    wr := NEW(FileWr.T).init(hw);
    RETURN NEW(Reader,f:=hr,ss:=NIL,close:=TRUE);
  END GimmeWr;

PROCEDURE Stdin(): Reader =
  BEGIN
    RETURN NEW(Reader,f:=si,ss:=NIL,close:=FALSE);
  END Stdin; 

VAR
  so,si,se: File.T;
BEGIN
  Process.GetStandardFileHandles(si,so,se);
END ProcUtils.
