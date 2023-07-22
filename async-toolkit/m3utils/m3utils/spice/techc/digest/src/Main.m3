MODULE Main;

IMPORT ParseParams;
IMPORT Stdio;
IMPORT Pathname;
IMPORT Debug;
IMPORT OSError, AL;
FROM Fmt IMPORT F, Int; IMPORT Fmt;
IMPORT RefSeq;
IMPORT RegEx;
IMPORT FS;
IMPORT Rd;
IMPORT FileRd;
IMPORT CSVParse;
IMPORT CitTextUtils;
IMPORT TextRefSeqTbl;
IMPORT Entry, EntryArraySort;
IMPORT Text;
IMPORT Wr, FileWr;
IMPORT Thread;
IMPORT Scan;
IMPORT Lex, FloatMode;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;
      LR = Fmt.LongReal;

      DirPat  = "[1-9][0-9]*.run";
      Verbose = TRUE;
  
VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  path : Pathname.T;

TYPE
  CsvCols = Entry.CsvCols;

PROCEDURE DoOneFile(rd : Rd.T) RAISES { Rd.Failure } =
  VAR
    csv := NEW(CSVParse.T).init(rd);
    c : CsvCols;
    e : Entry.T;
  BEGIN


    TRY
      LOOP
        csv.startLine();
        e := NEW(Entry.T);
        c := FIRST(CsvCols);

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
    C = S { CsvCols.Tech .. CsvCols.Simu } + S { CsvCols.Temp } + S { CsvCols.MoNm };

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
        res := res & Edit(Debug.UnNil(e[i]))
      END
    END;
    RETURN res
  END Tag;
  
PROCEDURE DoDirectory(workDir : Pathname.T) =
  <*FATAL RegEx.Error*>
  VAR
    iter : FS.Iterator;
    dn, fn : Pathname.T;
    pat := RegEx.Compile(DirPat);
    rd : Rd.T;

  BEGIN
    TRY
      iter := FS.Iterate(workDir);
    EXCEPT
      OSError.E(e) =>
      Debug.Error(F("Couldn't scan work directory \"%s\" : OSError.E : %s",
                    workDir, AL.Format(e)))
    END;
    
    WHILE iter.next(dn) DO
      IF RegEx.Execute(pat, dn) # -1 THEN
        (* matches *)
        TRY
          fn := workDir & "/" & dn & "/" & "measure.dat";
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
      OSError.E(e) =>
      Debug.Error(F("Couldn't open file \"%s\" : OSError.E : %s",
                    fn, AL.Format(e)))
    |
      Rd.Failure(e) =>
      Debug.Error(F("Couldn't read file \"%s\" : Rd.Failure : %s",
                    fn, AL.Format(e)))  
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
    IF Verbose THEN Debug.Out(F("Remember(%s)", tag)) END;
    IF NOT tbl.get(tag, rs) THEN
      rs := NEW(RefSeq.T).init();
      EVAL tbl.put(tag, rs)
    END;
    rs.addhi(rv)
  END Remember;

PROCEDURE SortEm() =
  TYPE
    Array = REF ARRAY OF Entry.T;
  VAR
    iter := tbl.iterate();
    k : TEXT;
    v : RefSeq.T;
    a : Array;
  BEGIN
    WHILE iter.next(k, v) DO
      IF Verbose THEN Debug.Out(F("%s : %s entries", k, Int(v.size()))) END;

      a := NEW(Array, v.size());

      FOR i := 0 TO v.size() - 1 DO
        a[i] := v.get(i)
      END;

      EntryArraySort.Sort(a^);

      FOR i := 0 TO v.size() - 1 DO
         v.put(i, a[i])
      END
      
    END
  END SortEm;

PROCEDURE Lookup(str : TEXT; READONLY a : ARRAY OF TEXT) : CARDINAL =
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      IF TE(str, a[i]) THEN RETURN i END
    END;
    VAR
      str := F("could not find %s among alternatives :");
    BEGIN
      FOR i := FIRST(a) TO LAST(a) DO
        str := str & F( " \"%s\"" , a[i])
      END;
      Debug.Error(str)
    END;
    <*ASSERT FALSE*>
  END Lookup;

PROCEDURE GraphEm() =
  VAR
    iter := tbl.iterate();
    k, fn, eflFn : TEXT;
    seq : RefSeq.T;
  TYPE
    C = CsvCols;
  CONST
    MaxCycle = 10.0d0; (* 0.1 Hz *)
  BEGIN
    WHILE iter.next(k, seq) DO
      TRY
        fn    := outDir & "/" & k & ".dat";
        eflFn := outDir & "/" & k & "_eflabels.src";
                     
        WITH wr = FileWr.Open(fn),
             eflWr = FileWr.Open(eflFn) DO
          FOR i := 0 TO seq.size() - 1 DO
            WITH entry = NARROW(seq.get(i), Entry.T),
                 
                 v = Scan.LongReal(entry[C.Volt]),
                 (* voltage *)
                 
                 c = Scan.LongReal(entry[C.Cycl]),
                 (* cycle time *)
                 
                 f = 1.0d0 / c,
                 (* frequency *)
                 
                 ai = Scan.LongReal(entry[C.Curr]),
                 (* active current *)
                 

                 li = Scan.LongReal(entry[C.Icur]),
                 (* leakage current *)

                 ap = v * ai,
                 (* active power *)

                 lp = v * li,
                 (* leakage power *)
                 
                 e = ap * c * scaleE,
                 (* energy per op *)

                 lf = lp / ap,
                 (* leakage fraction of total power *)

                 t = Scan.LongReal(entry[C.Temp])
                 (* temperature *)
             DO
              
              IF c < MaxCycle THEN
                Wr.PutText(wr,
                           F("%s %s %s %s %s\n",
                             LR(e), LR(f), LR(v), LR(lf), LR(t)));

                Wr.PutText(eflWr,
                           F("set label \"%s\" at %s, %s\n",
                             LR(v), LR(e), LR(f)))
              END
            END
          END;
          
          Wr.Close(wr);
          Wr.Close(eflWr)
        END
        
      EXCEPT
        FloatMode.Trap, Lex.Error =>
        Debug.Error("Trouble converting to floating point for " & fn);
        
      |
        OSError.E(e) =>
        Debug.Error(F("Couldn't open output file \"%s\" : OSError.E : %s",
                      fn, AL.Format(e)))
      |
        Wr.Failure(e) =>
        Debug.Error(F("Couldn't write output file \"%s\" : Rd.Failure : %s",
                      fn, AL.Format(e)))
      END
    END
  END GraphEm;

VAR
  mode : Mode;
  fix := Entry.Entry { NIL, .. };
  createOutDir : BOOLEAN;
  outDir : Pathname.T := ".";
  scaleE := 1.0d0;
  
BEGIN
  TRY
    createOutDir := pp.keywordPresent("-C");

    IF pp.keywordPresent("-o") THEN
      outDir := pp.getNext()
    END;

    IF pp.keywordPresent("-d") THEN
      path := pp.getNext();
      mode := Mode.Directory
    END;

    IF pp.keywordPresent("-f") THEN
      path := pp.getNext();
      mode := Mode.File
    END;

    IF pp.keywordPresent("-se") THEN
      scaleE := pp.getNextLongReal()
    END;
      

    WHILE pp.keywordPresent("-fix") DO
      WITH colName = pp.getNext(),
           colIdx  = VAL(Lookup(colName, Entry.CsvColNames), CsvCols),
           colValu = pp.getNext() DO
        fix[colIdx] := colValu
      END
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

  VAR
    re : Entry.T;
    skip : BOOLEAN;
    kept := 0;
  BEGIN
    FOR i := 0 TO entries.size() - 1 DO
      re := entries.get(i);
      skip := FALSE;
      FOR c := FIRST(fix) TO LAST(fix) DO
        IF fix[c] # NIL THEN
          IF NOT TE(fix[c], re[c]) THEN
            skip := TRUE
          END
        END
      END;
      IF NOT skip THEN
        WITH tag = Tag(re) DO
          (* split by tag, sort by voltage *)
          Remember(tag, re);
          INC(kept)
        END
      END
    END;

    Debug.Out(F("%s entries, %s used", Int(entries.size()), Int(kept)));


  END;

  SortEm();

  IF createOutDir THEN
    TRY FS.CreateDirectory(outDir) EXCEPT ELSE END
  END;
      
  GraphEm()
  
END Main.
