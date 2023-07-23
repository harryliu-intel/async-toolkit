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

TYPE
  S = SET OF CsvCols;

PROCEDURE Tag(e : Entry.T) : TEXT =
  CONST
    C = S { CsvCols.Tech .. CsvCols.Simu } + S { CsvCols.Temp } + S { CsvCols.MoNm };

  BEGIN
    RETURN TagAny(e, C)
  END Tag;

PROCEDURE TagLeak(e : Entry.T) : TEXT =
  CONST
    C = S { CsvCols.Tech .. CsvCols.Simu } + S { CsvCols.Volt } + S { CsvCols.MoNm };

  BEGIN
    RETURN TagAny(e, C)
  END TagLeak;
  
PROCEDURE TagAny(e : Entry.T; C : SET OF CsvCols) : TEXT =

  PROCEDURE Edit(txt : TEXT) : TEXT =
    CONST
      R = CitTextUtils.ReplaceChar;
    BEGIN
      RETURN R(R(txt, '-', 'm'), '.', 'p')
    END Edit;
    
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
  END TagAny;
  
  
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

PROCEDURE Remember(tbl : TextRefSeqTbl.T; tag : TEXT; rv : Entry.T) =
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

PROCEDURE SortEm(tbl : TextRefSeqTbl.T; by : Entry.CsvCols) =

  PROCEDURE Compare(a, b : Entry.T) : [-1 .. 1] =
    BEGIN
      RETURN(Entry.CompareLR(a, b, by))
    END Compare;
    
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

      EntryArraySort.Sort(a^, Compare);

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

TYPE
  PrintCol = { v, c, f, ai, li, lp, e, lf, t };
  PC        = PrintCol;
  PA        = ARRAY PrintCol OF LONGREAL;
  
PROCEDURE GraphEm(tbl           : TextRefSeqTbl.T;
                  READONLY cols : ARRAY OF PrintCol;
                  labcol        : PrintCol;
                  sfx := "") =
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
        fn    := outDir & "/" & k & sfx & ".dat";
        eflFn := outDir & "/" & k & sfx & "_eflabels.src";
                     
        WITH wr    = FileWr.Open(fn),
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

                 t = Scan.LongReal(entry[C.Temp]),
                 (* temperature *)

                 data = PA { v, c, f, ai, li, lp, e, lf, t }
                 (* all data *)
             DO
              
              IF c < MaxCycle THEN
                FOR i := FIRST(cols) TO LAST(cols) DO
                                    
                  Wr.PutText(wr, LR(data[cols[i]]));
                  IF i = LAST(cols) THEN
                    Wr.PutChar(wr, '\n')
                  ELSE
                    Wr.PutChar(wr, ' ')
                  END
                END;

                Wr.PutText(eflWr,
                           F("set label \"%s\" at %s, %s\n",
                             LR(data[labcol]), LR(data[cols[0]]), LR(data[cols[1]])))
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
  allTbl, leakTbl := NEW(TextRefSeqTbl.Default).init();
  
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
        (* split by tag, sort by voltage *)
        Remember(allTbl , Tag    (re), re);
        Remember(leakTbl, TagLeak(re), re);
        INC(kept)
      END
    END;

    Debug.Out(F("%s entries, %s used", Int(entries.size()), Int(kept)));


  END;

  IF createOutDir THEN
    TRY FS.CreateDirectory(outDir) EXCEPT ELSE END
  END;
      

  SortEm(allTbl, Entry.CsvCols.Volt);
  GraphEm(allTbl, ARRAY OF PC { PC.e, PC.f, PC.v, PC.lf, PC.t }, PC.v, "");

  SortEm(leakTbl, Entry.CsvCols.Temp);
  GraphEm(leakTbl, ARRAY OF PC { PC.t, PC.lp, PC.f }, PC.lf, "_leak")
END Main.
