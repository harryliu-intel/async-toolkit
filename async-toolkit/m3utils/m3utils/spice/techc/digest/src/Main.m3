MODULE Main;

IMPORT ParseParams;
IMPORT Stdio;
IMPORT Pathname;
IMPORT Debug;
IMPORT Process;
IMPORT OSError, AL;
FROM Fmt IMPORT F;
IMPORT RefSeq;
IMPORT RegEx;
IMPORT FS;
IMPORT Rd;
IMPORT FileRd;
IMPORT CSVParse;

CONST
  DirPat = "[1-9][0-9]*.run";
  
VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  workDir : Pathname.T := ".";

TYPE
  CsvCols = { Tech, Corn, Tran, Topo, Mode, Simu, Volt, Temp, Cycl, Curr };
  Entry = ARRAY CsvCols OF TEXT;

PROCEDURE DoOneFile(rd : Rd.T) =
  VAR
              csv := NEW(CSVParse.T).init(rd);
    c := FIRST(CsvCols);
    e := NEW(REF Entry);

  BEGIN


          TRY
            LOOP
              csv.startLine();
              
              WHILE csv.cellB(e[c]) DO

                IF c = LAST(CsvCols) THEN
                  EXIT
                END;

                INC(c)
              END;
              
              entries.addhi(e)
            END
          EXCEPT
            Rd.EndOfFile => (* done *)
          END
  END DoOneFile;
  
VAR
  entries := NEW(RefSeq.T).init();
  
BEGIN
  TRY
    IF pp.keywordPresent("-d") THEN
      workDir := pp.getNext()
    END;

    pp.skipParsed();
    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  TRY
    Process.SetWorkingDirectory(workDir)
  EXCEPT
    OSError.E(e) =>
    Debug.Error(F("Couldn't set working directory to \"%s\" : OSError.E : %s",
                  workDir, AL.Format(e)))
  END;

  VAR
    iter := FS.Iterate(".");
    dn, fn : Pathname.T;
    pat := RegEx.Compile(DirPat);
    rd : Rd.T;

  BEGIN
    WHILE iter.next(dn) DO
      IF RegEx.Execute(pat, dn) # -1 THEN
        (* matches *)
        TRY
          fn := dn & "/" & "measure.dat";
          rd := FileRd.Open(fn);
          DoOneFile(rd)
          
        EXCEPT
          OSError.E, Rd.Failure =>
          Debug.Warning(F("Problem reading \"%s\", skipping", fn))
        END;
        IF rd # NIL THEN
          TRY Rd.Close(rd) EXCEPT ELSE END
        END
      END
    END
  END
  
END Main.
