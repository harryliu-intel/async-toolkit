MODULE Main;

IMPORT ParseParams;
IMPORT Stdio;
IMPORT Pathname;
IMPORT Debug;
IMPORT Process;
IMPORT OSError, AL;
FROM Fmt IMPORT F, Int;
IMPORT RefSeq;
IMPORT RegEx;
IMPORT FS;
IMPORT Rd;
IMPORT FileRd;
IMPORT CSVParse;
IMPORT CitTextUtils;
IMPORT TextRefSeqTbl;
IMPORT Entry, EntryArraySort;

CONST
  DirPat = "[1-9][0-9]*.run";
  
VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  path : Pathname.T;

TYPE
  CsvCols = Entry.CsvCols;

PROCEDURE DoOneFile(rd : Rd.T) =
  VAR
    csv := NEW(CSVParse.T).init(rd);
    c := FIRST(CsvCols);
    e := NEW(Entry.T);

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

TYPE
  Mode = { Directory, File };

PROCEDURE Tag(e : Entry.T) : TEXT =

  PROCEDURE Edit(txt : TEXT) : TEXT =
    CONST
      R = CitTextUtils.ReplaceChar;
    BEGIN
      RETURN R(R(txt, '-', 'm'), '.', 'p')
    END Edit;
    
  TYPE
    S = SET OF CsvCols;
    
  CONST
    C = S { CsvCols.Tech .. CsvCols.Simu } + S { CsvCols.Temp };

  VAR
    res := "";
    first := TRUE;
    
  BEGIN
    FOR i := FIRST(e^) TO LAST(e^) DO
      IF i IN C THEN
        IF first THEN
          first := FALSE
        ELSE
          res := res & "_"
        END;
        res := res & Edit(e[i])
      END
    END;
    RETURN res
  END Tag;
  
PROCEDURE DoDirectory(workDir : Pathname.T) =
  VAR
    iter : FS.Iterator;
    dn, fn : Pathname.T;
    pat := RegEx.Compile(DirPat);
    rd : Rd.T;

  BEGIN
    TRY
      Process.SetWorkingDirectory(workDir)
    EXCEPT
      OSError.E(e) =>
      Debug.Error(F("Couldn't set working directory to \"%s\" : OSError.E : %s",
                    workDir, AL.Format(e)))
    END;
    iter := FS.Iterate(".");
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
  END DoDirectory;

PROCEDURE DoFile(fn : Pathname.T) =
  VAR
    rd : Rd.T;
  BEGIN
    TRY
      rd := FileRd.Open(fn);
      DoOneFile(rd)
    EXCEPT
    END;
    IF rd # NIL THEN
      TRY Rd.Close(rd) EXCEPT ELSE END
    END
  END DoFile;

VAR tbl := NEW(TextRefSeqTbl.Default).init();
    
PROCEDURE Remember(tag : TEXT; rv : Entry.T) =
  VAR
    rs : RefSeq.T;
  BEGIN
    IF NOT tbl.get(tag, rs) THEN
      rs := NEW(RefSeq.T).init();
      EVAL tbl.put(tag, rs)
    END;
    rs.addhi(rv)
  END Remember;

PROCEDURE SortEm() =
  BEGIN
  END SortEm;
  
VAR
  mode : Mode;
BEGIN
  TRY
    IF pp.keywordPresent("-d") THEN
      path := pp.getNext();
      mode := Mode.Directory
    END;

    IF pp.keywordPresent("-f") THEN
      path := pp.getNext();
      mode := Mode.File
    END;

    pp.skipParsed();
    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  CASE mode OF
    Mode.Directory => DoDirectory(path)
  |
    Mode.File => DoFile(path)
  END;

  Debug.Out(F("%s entries", Int(entries.size())));

  VAR
    re : Entry.T;
  BEGIN
    FOR i := 0 TO entries.size() - 1 DO
      re := entries.get(i);
      WITH tag = Tag(re) DO
        (* split by tag, sort by voltage *)
        Remember(tag, re)
      END
    END
  END;

  SortEm()
  
END Main.
