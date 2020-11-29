MODULE Main;
IMPORT ParseParams;
IMPORT Rd, FileRd, Stdio;
IMPORT Debug;
IMPORT BraceParse;
IMPORT Text;
IMPORT AtomCellTbl;
FROM Fmt IMPORT F, Int;
IMPORT Atom, CellRec, CellRecClass;
IMPORT Wx;
IMPORT AtomMosInfoCardTblTbl;
IMPORT MosInfoCardTbl;
IMPORT MosInfo;
IMPORT AtomSet, AtomSetDef;
IMPORT CardPair;
IMPORT OSError;
IMPORT AL;
IMPORT Thread;
IMPORT OpenCharArrayRefTbl;
IMPORT Wr, FileWr;
IMPORT TextList;
IMPORT Pathname;
IMPORT AtomCardTbl;
IMPORT AtomTextTbl;
IMPORT AtomCardTripleTbl;
IMPORT CardTriple;
IMPORT LongNames;
<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;

VAR doDebug := Debug.GetLevel() >= 10;

<*UNUSED*>
PROCEDURE PutCsvHeader(wr : Wr.T)
  RAISES { Wr.Failure } =
  BEGIN
    Wr.PutText(wr, "path,cellType,level,MOS type,length/pm,count,totfins,fin*pm\n")
  END PutCsvHeader;

PROCEDURE PutCsvHeader2(wr : Wr.T)
  RAISES { Wr.Failure } =
  BEGIN
    Wr.PutText(wr, "canonpath,cellType,minlevel,multiplicity,MOS type,length/pm,count/inst,fins/inst,fins*pm/inst,totcount,totfins,totfins*pm\n")
  END PutCsvHeader2;

PROCEDURE PutConsolidatedHeader(wr : Wr.T)
  RAISES { Wr.Failure } =
  BEGIN
    Wr.PutText(wr, "canonpath,cellType,minlevel,multiplicity,MOS type,count/inst,fins/inst,fins*pm/inst,totcount,totfins,totfins*pm\n")
  END PutConsolidatedHeader;

PROCEDURE DoTransistorReports(wr       : Wr.T;
                              csvWr    : Wr.T;
                              db       : AtomCellTbl.T;
                              rootType : Atom.T;
                              memoTbl  : AtomMosInfoCardTblTbl.T;
                              level    : CARDINAL;
                              nlevels  : CARDINAL;
                              path     : TEXT;
                              longNames: LongNames.T)
  RAISES { Wr.Failure } =

  (* old recursive version, not currently used --- see DoTransistorReports2 *)
  
  VAR
    tab     : MosInfoCardTbl.T;
    iter    : MosInfoCardTbl.Iterator;
    mosInfo : MosInfo.T;
    finCnt  : CardPair.T;
    wx := Wx.New();
  BEGIN
    WITH hadIt = memoTbl.get(rootType, tab) DO
      IF NOT hadIt THEN RETURN END
    END;
    (* got the tab *)
    iter := tab.iterate();

    (* iterate thru all the transistor types in the tab *)
    WHILE iter.next(mosInfo, finCnt) DO
      MosInfo.DebugOut(mosInfo, wx);
      Wx.PutInt(wx, finCnt.k1);
      Wx.PutText(wx, " devices ");
      Wx.PutInt(wx, finCnt.k2);
      Wx.PutText(wx, " fins ");
      Wx.PutInt(wx, finCnt.k2 * mosInfo.len);
      Wx.PutText(wx, " fin*picometer\n");
    END;

    Wr.PutText(wr, F("====================  Level %s Path %s Type %s  ====================\n",
                     Int(level),
                     path,
                     Atom.ToText(rootType)));
    
    Wr.PutText(wr, Wx.ToText(wx));

    (**************************************************)

    iter := tab.iterate();
    WHILE iter.next(mosInfo, finCnt) DO
      Wr.PutText(csvWr, path);
      Wr.PutChar(csvWr, ',');
      Wr.PutText(csvWr, Atom.ToText(rootType));
      Wr.PutChar(csvWr, ',');
      Wr.PutText(csvWr, Int(level));
      Wr.PutChar(csvWr, ',');
      Wr.PutText(csvWr, Atom.ToText(mosInfo.type));
      Wr.PutChar(csvWr, ',');
      Wr.PutText(csvWr, Int(mosInfo.len));
      Wr.PutChar(csvWr, ',');
      Wr.PutText(csvWr, Int(finCnt.k1));
      Wr.PutChar(csvWr, ',');
      Wr.PutText(csvWr, Int(finCnt.k2));
      Wr.PutChar(csvWr, ',');
      Wr.PutText(csvWr, Int(finCnt.k2 * mosInfo.len));
      Wr.PutChar(csvWr, '\n');
    END;
    
    (**************************************************)

    IF level + 1 < nlevels THEN
      VAR
        cell : CellRec.T;
      BEGIN
        WITH hadIt = db.get(rootType, cell) DO
          IF NOT hadIt THEN RETURN END;
          FOR i := FIRST(cell.subcells^) TO LAST(cell.subcells^) DO
            VAR
              sub := cell.subcells[i];
              decodedSubName := LongNames.DecodeToText(longNames,
                                                         sub.instance);
            BEGIN
              DoTransistorReports(wr,
                                  csvWr,
                                  db,
                                  sub.type.nm,
                                  memoTbl,
                                  level + 1,
                                  nlevels,
                                  path & "." & decodedSubName,
                                  longNames)
            END
          END
        END
      END
    END
  END DoTransistorReports;

PROCEDURE AccumByAtom(tbl : AtomCardTripleTbl.T; atom : Atom.T; c : CardTriple.T) =
  VAR
    old : CardTriple.T;
  BEGIN
    IF tbl.get(atom, old) THEN
      EVAL tbl.put(atom, CardTriple.Add(old, c))
    ELSE
      EVAL tbl.put(atom, c)
    END
  END AccumByAtom;
  
PROCEDURE DoTransistorReports2(wr       : Wr.T;
                               csvWr    : Wr.T;
                               conWr    : Wr.T;
                               db       : AtomCellTbl.T;
                               rootType : CellRec.T;
                               memoTbl  : AtomMosInfoCardTblTbl.T;
                               nlevels  : CARDINAL;
                               longNames: LongNames.T)
  RAISES { Wr.Failure } =

  PROCEDURE Emit(nm : Atom.T; level : CARDINAL)
    RAISES { Wr.Failure } =
    
    (* write the stats for a single cell;
       assumes that CountTotalInstances was just called so that the cell.aux
       contains the total # of instances of each type in the design
    *)
    
    VAR
      tab     : MosInfoCardTbl.T;
      cell    : CellRec.T;
      mosInfo : MosInfo.T;
      finCnt  : CardPair.T;
      wx := Wx.New();
      cpath   : TEXT;
      consolidated := NEW(AtomCardTripleTbl.Default).init();
    BEGIN
      WITH hadIt = memoTbl.get(nm, tab) DO
        <*ASSERT hadIt*>
      END;
      WITH hadIt = db.get(nm, cell) DO
        <*ASSERT hadIt*>
        <*ASSERT cell.nm = nm*>
      END;
      WITH hadIt = canonNames.get(nm, cpath) DO
        <*ASSERT hadIt*>
      END;
      WITH iter = tab.iterate(),
           n    = cell.aux DO
            (* iterate thru all the transistor types in the tab *)

        WHILE iter.next(mosInfo, finCnt) DO
          MosInfo.DebugOut(mosInfo, wx);
          AccumByAtom(consolidated, mosInfo.type,
                      CardTriple.T { finCnt.k1,
                                     finCnt.k2,
                                     finCnt.k2 * mosInfo.len });
          Wx.PutInt(wx, finCnt.k1);
          Wx.PutText(wx, " devs ");
          Wx.PutInt(wx, finCnt.k2);
          Wx.PutText(wx, " fins ");
          Wx.PutInt(wx, finCnt.k2 * mosInfo.len);
          Wx.PutText(wx, " fin*pm");
          
          Wx.PutInt(wx, n * finCnt.k1);
          Wx.PutText(wx, " tot devs ");
          Wx.PutInt(wx, n * finCnt.k2);
          Wx.PutText(wx, " tot fins ");
          Wx.PutInt(wx, n * finCnt.k2 * mosInfo.len);
          Wx.PutText(wx, " tot fin*pm");

          
          Wx.PutChar(wx, '\n');
        END;
        
        Wr.PutText(wr, F("====================  Level %s Canon path %s Type %s Total instances %s  ====================\n",
                         Int(level),
                         cpath,
                         Atom.ToText(nm),
                         Int(cell.aux)));
    
        Wr.PutText(wr, Wx.ToText(wx));
      END;

      WITH iter = tab.iterate(),
           n    = cell.aux DO
        WHILE iter.next(mosInfo, finCnt) DO
          Wr.PutText(csvWr, cpath);
          Wr.PutChar(csvWr, ',');
          Wr.PutText(csvWr, Atom.ToText(nm));
          Wr.PutChar(csvWr, ',');
          Wr.PutText(csvWr, Int(level));
          Wr.PutChar(csvWr, ',');
          Wr.PutText(csvWr, Int(n));
          Wr.PutChar(csvWr, ',');
          Wr.PutText(csvWr, Atom.ToText(mosInfo.type));
          Wr.PutChar(csvWr, ',');
          Wr.PutText(csvWr, Int(mosInfo.len));
          Wr.PutChar(csvWr, ',');
          Wr.PutText(csvWr, Int(finCnt.k1));
          Wr.PutChar(csvWr, ',');
          Wr.PutText(csvWr, Int(finCnt.k2));
          Wr.PutChar(csvWr, ',');
          Wr.PutText(csvWr, Int(finCnt.k2 * mosInfo.len));
          Wr.PutChar(csvWr, ',');
          Wr.PutText(csvWr, Int(n * finCnt.k1));
          Wr.PutChar(csvWr, ',');
          Wr.PutText(csvWr, Int(n * finCnt.k2));
          Wr.PutChar(csvWr, ',');
          Wr.PutText(csvWr, Int(n * finCnt.k2 * mosInfo.len));
          Wr.PutChar(csvWr, '\n');
        END
      END;

      VAR ttype : Atom.T;
          iter := consolidated.iterate();
          n   := cell.aux;
          data : CardTriple.T;
      BEGIN
        WHILE iter.next(ttype, data) DO
          Wr.PutText(conWr, cpath);
          Wr.PutChar(conWr, ',');
          Wr.PutText(conWr, Atom.ToText(nm));
          Wr.PutChar(conWr, ',');
          Wr.PutText(conWr, Int(level));
          Wr.PutChar(conWr, ',');
          Wr.PutText(conWr, Int(n));
          Wr.PutChar(conWr, ',');
          Wr.PutText(conWr, Atom.ToText(ttype));
          Wr.PutChar(conWr, ',');
          Wr.PutText(conWr, Int(data[0]));
          Wr.PutChar(conWr, ',');
          Wr.PutText(conWr, Int(data[1]));
          Wr.PutChar(conWr, ',');
          Wr.PutText(conWr, Int(data[2]));
          Wr.PutChar(conWr, ',');
          Wr.PutText(conWr, Int(n * data[0]));
          Wr.PutChar(conWr, ',');
          Wr.PutText(conWr, Int(n * data[1]));
          Wr.PutChar(conWr, ',');
          Wr.PutText(conWr, Int(n * data[2]));
          Wr.PutChar(conWr, '\n');
        END
      END
    END Emit;
    
  PROCEDURE PrintAtLevel(lev : CARDINAL) RAISES { Wr.Failure } =
    VAR
      iter := printTypes.iterate();
      a    : Atom.T;
      clev : CARDINAL;
    BEGIN
      WHILE iter.next(a, clev) DO
        IF lev = clev THEN
          Emit(a, clev)
        END
      END
    END PrintAtLevel;
    
  VAR
    printTypes := MarkMinDepth(db, rootType, nlevels - 1);
    canonNames := FindCanonicalNames(longNames, rootType, nlevels - 1);
  BEGIN
    
    CountTotalInstances(db, rootType);

    FOR i := 0 TO nlevels - 1 DO
      PrintAtLevel(i)
    END
  END DoTransistorReports2;
  
PROCEDURE MarkMinDepth(db       : AtomCellTbl.T;
                       rootType : CellRec.T;
                       maxDepth : CARDINAL) : AtomCardTbl.T =

  VAR
    res := NEW(AtomCardTbl.Default).init();
    
  PROCEDURE Recurse(type : CellRec.T; depth : CARDINAL) =
    BEGIN
      IF depth < type.aux THEN
        type.aux := depth;
        IF depth <= maxDepth THEN
          EVAL res.put(type.nm, depth)
        END;
        FOR i := FIRST(type.subcells^) TO LAST(type.subcells^) DO
          Recurse(type.subcells[i].type, depth + 1)
        END
      END
    END Recurse;

  BEGIN
    BraceParse.InitCellTblAux(db, LAST(CARDINAL));
    Recurse(rootType, 0);
    RETURN res
  END MarkMinDepth;

TYPE Formatter = PROCEDURE() : TEXT;
  
PROCEDURE FindCanonicalNames(longNames: LongNames.T;
                             rootType : CellRec.T;
                             maxDepth : CARDINAL) : AtomTextTbl.T =
  (* when this procedure is run, MarkMinDepth must have been
     run, so that all the .aux fields contain the type's min depth *)
  VAR
    res := NEW(AtomTextTbl.Default).init();    

  PROCEDURE Recurse(type : CellRec.T; depth : CARDINAL; f : Formatter) =

    VAR
      dummy : TEXT;
    BEGIN
      IF depth = type.aux AND NOT res.get(type.nm, dummy) THEN
        (* ok we have a new canon. name. *)
        WITH canon = f() DO
          IF doDebug THEN
            Debug.Out(F("canonical name of %s is %s", Atom.ToText(type.nm), canon))
          END;
          EVAL res.put(type.nm, canon)
        END
      END;
        
      IF depth # maxDepth THEN
        FOR i := FIRST(type.subcells^) TO LAST(type.subcells^) DO

          (* using this nested procedure approach defers printing to
             the last possible moment 

             AND

             stores the necessary linkage on the stack! 
          *)
          
          PROCEDURE Formatter() : TEXT =
            BEGIN
              RETURN f() & "." & LongNames.DecodeToText(longNames, instance)
            END Formatter;
          VAR
            instance := type.subcells[i].instance;
          BEGIN
            Recurse(type.subcells[i].type,
                    depth + 1,
                    Formatter)
          END
        END
      END
    END Recurse;

  PROCEDURE RootFormatter() : TEXT = BEGIN RETURN "(ROOT)" END RootFormatter;

  BEGIN
    Recurse(rootType, 0, RootFormatter);
    RETURN res
  END FindCanonicalNames;
  

PROCEDURE CountTotalInstances(db       : AtomCellTbl.T;
                              rootType : CellRec.T) =

  PROCEDURE Recurse(type : CellRec.T; weight : CARDINAL) =
    VAR
      tbl := NEW(AtomCardTbl.Default).init();
      o : CARDINAL;
    BEGIN
      INC(type.aux, weight);

      (* count instances of each subtype here *)
      FOR i := FIRST(type.subcells^) TO LAST(type.subcells^) DO
        WITH sub = type.subcells[i] DO
          IF tbl.get(sub.type.nm, o) THEN
            EVAL tbl.put(sub.type.nm, o + 1)
          ELSE
            EVAL tbl.put(sub.type.nm, 1)
          END
        END
      END;

      VAR
        iter := tbl.iterate();
        a : Atom.T;
        cell : CellRec.T;
        cnt : CARDINAL;
      BEGIN
        WHILE iter.next(a, cnt) DO
          WITH hadIt = db.get(a, cell) DO
            <*ASSERT hadIt*>
          END;
          Recurse(cell, cnt * weight)
        END
      END
    END Recurse;
    
  BEGIN
    BraceParse.InitCellTblAux(db, 0);
    Recurse(rootType, 1)
  END CountTotalInstances;

PROCEDURE RecurseTransistors(db       : AtomCellTbl.T;
                             rootType : CellRec.T;
                             memoTbl  : AtomMosInfoCardTblTbl.T;
                             warnSet  : AtomSet.T) =
  VAR
    tab : MosInfoCardTbl.T;
  BEGIN
    (* check if already computed *)
    IF memoTbl.get(rootType.nm, tab) THEN RETURN END;

    (* not yet computed, compute it! *)
    VAR
      cellRec := rootType;
    BEGIN
      VAR
        tbl := NEW(MosInfoCardTbl.Default).init();
      BEGIN
        MosInfoAdd(tbl, cellRec.mosTbl);
        FOR i := FIRST(cellRec.subcells^) TO LAST(cellRec.subcells^) DO
          WITH sc = cellRec.subcells[i] DO
            RecurseTransistors(db, sc.type, memoTbl, warnSet);
            WITH hadIt = memoTbl.get(sc.type.nm, tab) DO
              IF hadIt THEN
                MosInfoAdd(tbl, tab)
              ELSE
                IF NOT warnSet.member(sc.type.nm) THEN
                  Debug.Warning(F("unknown cell type %s while processing %s",
                                  Atom.ToText(sc.type.nm),
                                  Atom.ToText(rootType.nm)));
                  EVAL warnSet.insert(sc.type.nm)
                END
              END
            END
          END
        END;
        EVAL memoTbl.put(rootType.nm, tbl)
      END
    END
  END RecurseTransistors;

PROCEDURE MosInfoAdd(tgt, from : MosInfoCardTbl.T) =
  VAR
    iter := from.iterate();
    info : MosInfo.T;
    fromfins, tofins : CardPair.T;
  BEGIN
    WHILE iter.next(info, fromfins) DO
      IF tgt.get(info, tofins) THEN
        EVAL tgt.put(info, CardPair.T { tofins.k1 + fromfins.k1,
                                        tofins.k2 + fromfins.k2 })
      ELSE
        EVAL tgt.put(info, fromfins)
      END
    END
  END MosInfoAdd;

PROCEDURE ProduceGoxReports(p : TextList.T) =
  BEGIN
    WHILE p # NIL DO
      VAR rootType := rootTypes.head;
          memoTbl := NEW(AtomMosInfoCardTblTbl.Default).init();
          warnSet := NEW(AtomSetDef.T).init();
          root   : CellRec.T;
          rootTypeA := Atom.FromText(rootType);
      BEGIN
        IF NOT parsed.cellTbl.get(rootTypeA, root) THEN
          Debug.Warning("Cannot find root type : " & rootType);
          EXIT
        END;
        <*ASSERT root # NIL*>
        <*ASSERT root.nm # NIL*>
        RecurseTransistors(parsed.cellTbl,
                           root,
                           memoTbl,
                           warnSet);

        TRY
          WITH pfx = F("%s.%s", p.head, transistorReportSfx),
               wr    = FileWr.Open(pfx & ".rpt"),
               csvWr = FileWr.Open(pfx & ".csv"),
               conWr = FileWr.Open(pfx & "_consolidated.csv") DO
            PutCsvHeader2(csvWr);
            PutConsolidatedHeader(conWr);
            DoTransistorReports2(wr,
                                 csvWr,
                                 conWr,
                                 parsed.cellTbl,
                                 root,
                                 memoTbl,
                                 levels,
                                 parsed.longNames);
            Wr.Close(wr);
            Wr.Close(csvWr);
            Wr.Close(conWr)
          END
        EXCEPT
          OSError.E(x) =>
          Debug.Error(F("Trouble opening/closing transistor reports file %s : OSError.E : %s",
                        transistorReportSfx,
                        AL.Format(x)))
        |
          Wr.Failure(x) =>
          Debug.Error(F("Trouble writing transistor reports file %s : Wr.Failure : %s",
                        transistorReportSfx,
                        AL.Format(x)))
        END
      END;
      p := p.tail
    END(*EHILW*)
  END ProduceGoxReports;

PROCEDURE ReadSpecialTransistorCells(transistorCellFn : Pathname.T) =
  BEGIN
    TRY
      WITH rd = FileRd.Open(transistorCellFn) DO
        LOOP
          VAR
            buf : BraceParse.Buffer;
            cnt := Rd.GetSubLine(rd, buf);
          BEGIN
            IF cnt = 0 THEN
              Rd.Close(rd);
              EXIT
            END;
            IF doDebug THEN
              Debug.Out("special transistor cell type : " &
                Text.FromChars(SUBARRAY(buf, 0, cnt - 1)))
            END;
            EVAL transistorCells.put(SUBARRAY(buf, 0, cnt - 1), NIL)
          END
        END
      END
    EXCEPT
      OSError.E(e) => Debug.Error(F("Main.m3: couldnt open transistor cell file \"%s\" : OSError.E : %s", transistorCellFn, AL.Format(e)))
      
    |
      Rd.Failure(e) => Debug.Error(F("Main.m3: couldnt open transistor cell file \"%s\" : OSError.E : %s", transistorCellFn, AL.Format(e)))
        
    END
  END ReadSpecialTransistorCells;

PROCEDURE DebugDumpCells(parsed : BraceParse.T) =
  VAR
    iter := parsed.cellTbl.iterate();
    nm : Atom.T;
    cell : CellRec.T;
  BEGIN
    Debug.Out(F("Main.m3 got %s cells", Int(parsed.cellTbl.size())));
    WHILE iter.next(nm, cell) DO
      VAR
        wx := Wx.New();
      BEGIN
        CellRec.DebugOut(cell, wx);
        Debug.Out(Wx.ToText(wx))
      END
    END
  END DebugDumpCells;

VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  rd : Rd.T := NIL;
  parsed : BraceParse.T;
  rootTypes : TextList.T := NIL;
  transistorCellFn : TEXT := NIL;
  transistorReportSfx : TEXT := "gox";
  transistorCells := NEW(OpenCharArrayRefTbl.Default).init();
  levels : CARDINAL := 1; (* report levels, default just the root *)
BEGIN
  TRY
    IF pp.keywordPresent("-Z") THEN
    END;
    IF pp.keywordPresent("-f") THEN
      WITH fn = pp.getNext() DO
        IF TE(fn,"-") THEN
          rd := Stdio.stdin
        ELSE
          TRY
            rd := FileRd.Open(fn)
          EXCEPT
            OSError.E(e) => Debug.Error(F("Main.m3: trouble opening \"%s\" : OSError.E : %s",
                                          fn,
                                          AL.Format(e)))
          END
        END
      END
    END;

    IF pp.keywordPresent("-r") OR pp.keywordPresent("-rootcell") THEN
      WITH rootType = pp.getNext() DO
        rootTypes := TextList.Cons(rootType, rootTypes)
      END
    END;

    IF pp.keywordPresent("-t") OR pp.keywordPresent("-transistorcelltypes") THEN
      transistorCellFn := pp.getNext()
    END;

    IF pp.keywordPresent("-T") OR pp.keywordPresent("-transistorreport") THEN
      transistorReportSfx := pp.getNext()
    END;

    IF pp.keywordPresent("-l") OR pp.keywordPresent("-levels") THEN
      levels := pp.getNextInt()
    END;

    pp.skipParsed()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  IF rd = NIL THEN Debug.Error("Must provide filename") END;

  IF transistorCellFn # NIL THEN
    ReadSpecialTransistorCells(transistorCellFn)
  END;
  
  TRY
    parsed := BraceParse.Parse(rd, transistorCells);
  EXCEPT
    Rd.Failure(e) => Debug.Error("Main.m3: Trouble parsing input : Rd.Failure : "&
      AL.Format(e))
  END;

  IF doDebug THEN
    DebugDumpCells(parsed)
  END;

  ProduceGoxReports(rootTypes)
END Main.

