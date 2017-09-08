MODULE Main;
IMPORT Stdio, Rd, Wr;
IMPORT RegEx;
IMPORT Text, TextUtils;
IMPORT FileRd;
IMPORT Thread;
IMPORT Fmt; FROM Fmt IMPORT F;
IMPORT Debug;
IMPORT Pathname;
IMPORT OSError, AL;
IMPORT ParseParams;
IMPORT TextReader, TextList;
IMPORT Params;

<*FATAL Thread.Alerted*>
CONST
  Usage = "[-path <colon-separated-search-path>]";
  
CONST
  DQ = '"'; (* " *)
  WS = SET OF CHAR { ' ', '\t', '\r', '\n' };
  
VAR
  wr := Stdio.stdout;

CONST
  incTgt = "`include";

PROCEDURE DoUsage() : TEXT =
  BEGIN
    RETURN
      Params.Get(0) & ": usage: " & Usage
  END DoUsage;

PROCEDURE DoOne(rd : Rd.T; ifn : Pathname.T) =
  <*FATAL RegEx.Error*>
  VAR
    incPat := RegEx.Compile("^[ \t]*"&incTgt);
    pos : CARDINAL;
    lNo := 0;
  BEGIN
    TRY
      LOOP
        WITH line = Rd.GetLine(rd) DO
          INC(lNo);
          IF RegEx.Execute(incPat, line) # -1 THEN
            WITH haveIt = TextUtils.FindSub(line, incTgt, pos) DO
              <*ASSERT haveIt*>
            END;

            VAR
              s := pos + Text.Length(incTgt);
              e := 0;
            BEGIN
              WHILE Text.GetChar(line,s) IN WS DO INC(s) END;
              <*ASSERT Text.GetChar(line,s)=DQ*>
              INC(s);
              e := s;
              WHILE Text.GetChar(line,e) # DQ DO INC(e) END;
              <*ASSERT Text.GetChar(line,e)=DQ*>

              VAR
                rd : Rd.T;
                fn := Text.Sub(line, s, e-s);
              BEGIN
                TRY
                  rd := OpenFile(fn)
                EXCEPT
                  OSError.E(x) =>
                  Debug.Error("cant open file \"" & fn & "\" : OSError.E : " &
                    AL.Format(x))
                END;
                DoOne(rd, fn);
                Rd.Close(rd)
              END
            END
          ELSE
            TRY
              Wr.PutText(wr, line);
              Wr.PutChar(wr, '\n')
            EXCEPT
              Wr.Failure (x) => Debug.Error("Couldnt write output : Wr.Failure : " & AL.Format(x))
            END
          END
        END
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    |
      Rd.Failure =>
      Debug.Error("I/O error reading input from " & ifn & ", line " & Fmt.Int(lNo))
    END
  END DoOne;

PROCEDURE ParsePathList(t : TEXT) : TextList.T =
  BEGIN
    RETURN NEW(TextReader.T).init(t).shatter(":","")
  END ParsePathList;
  
VAR
  path := TextList.List1(".");

PROCEDURE OpenFile(pn : Pathname.T) : Rd.T RAISES { OSError.E } =
  VAR
    p := path;
  BEGIN
    WHILE p # NIL DO
      TRY
        WITH fn = Pathname.Join(p.head, pn) DO
          Debug.Out(F("Attempting to open path \"%s\"",fn));
          RETURN FileRd.Open(fn)
        END
      EXCEPT
        OSError.E(x) =>
        IF p.tail = NIL THEN RAISE OSError.E(x) ELSE p := p.tail END
      END
    END;
    <*ASSERT FALSE*>
  END OpenFile;
  
BEGIN
  TRY
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      IF pp.keywordPresent("-P") OR pp.keywordPresent("--path") THEN
        path := ParsePathList(pp.getNext())
      END;
      pp.skipParsed();
      pp.finish();
    END
  EXCEPT
    ParseParams.Error => Debug.Error("Command-line params wrong:\n" & DoUsage())
  END;
  DoOne(Stdio.stdin,"--STDIN--")
END Main.
