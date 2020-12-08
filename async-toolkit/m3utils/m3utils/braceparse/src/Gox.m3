MODULE Gox;
IMPORT TextList;
IMPORT AtomMosInfoCardTblTbl;
IMPORT AtomSetDef;
IMPORT Wr;
IMPORT CellRec, CellRecClass;
IMPORT Debug;
IMPORT FileWr, OSError;
FROM Fmt IMPORT F, Int;
IMPORT Thread;
IMPORT BraceParse;
IMPORT Atom;
IMPORT AL;
IMPORT FinInfo, AtomFinInfoTbl;
IMPORT LongNames;
IMPORT AtomCellTbl;
IMPORT AtomCardTbl;
IMPORT MosInfoCardTbl;
IMPORT Wx;
IMPORT AtomTextTbl;
IMPORT AtomSet;
IMPORT MosInfo;

<*FATAL Thread.Alerted*>

VAR doDebug := Debug.GetLevel() >= 10;

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

PROCEDURE AccumByAtom(tbl : AtomFinInfoTbl.T; atom : Atom.T; c : FinInfo.T) =
  VAR
    old : FinInfo.T;
  BEGIN
    IF tbl.get(atom, old) THEN
      EVAL tbl.put(atom, FinInfo.Add(old, c))
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
      finCnt  : FinInfo.T;
      wx := Wx.New();
      cpath   : TEXT;
      consolidated := NEW(AtomFinInfoTbl.Default).init();
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
          AccumByAtom(consolidated, mosInfo.type, finCnt);
          Wx.PutInt(wx, finCnt[0]);
          Wx.PutText(wx, " devs ");
          Wx.PutInt(wx, finCnt[1]);
          Wx.PutText(wx, " fins ");
          Wx.PutInt(wx, finCnt[2]);
          Wx.PutText(wx, " fin*pm");
          
          Wx.PutInt(wx, n * finCnt[0]);
          Wx.PutText(wx, " tot devs ");
          Wx.PutInt(wx, n * finCnt[1]);
          Wx.PutText(wx, " tot fins ");
          Wx.PutInt(wx, n * finCnt[2]);
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
          Wr.PutText(csvWr, Int(finCnt[0]));
          Wr.PutChar(csvWr, ',');
          Wr.PutText(csvWr, Int(finCnt[1]));
          Wr.PutChar(csvWr, ',');
          Wr.PutText(csvWr, Int(finCnt[2]));
          Wr.PutChar(csvWr, ',');
          Wr.PutText(csvWr, Int(n * finCnt[0]));
          Wr.PutChar(csvWr, ',');
          Wr.PutText(csvWr, Int(n * finCnt[1]));
          Wr.PutChar(csvWr, ',');
          Wr.PutText(csvWr, Int(n * finCnt[2]));
          Wr.PutChar(csvWr, '\n');
        END
      END;

      VAR ttype : Atom.T;
          iter := consolidated.iterate();
          n   := cell.aux;
          data : FinInfo.T;
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
    tab, tbl : MosInfoCardTbl.T;
  BEGIN
    (* check if already computed *)
    IF memoTbl.get(rootType.nm, tab) THEN RETURN END;

    (* not yet computed, compute it! *)
    tbl := NEW(MosInfoCardTbl.Default).init();

    MosInfoAdd(tbl, rootType.mosTbl);
    FOR i := FIRST(rootType.subcells^) TO LAST(rootType.subcells^) DO
      WITH sc = rootType.subcells[i] DO
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
  END RecurseTransistors;

PROCEDURE MosInfoAdd(tgt, from : MosInfoCardTbl.T) =
  VAR
    iter := from.iterate();
    info : MosInfo.T;
    fromfins, tofins : FinInfo.T;
  BEGIN
    WHILE iter.next(info, fromfins) DO
      IF tgt.get(info, tofins) THEN
        EVAL tgt.put(info, FinInfo.Add(tofins,fromfins)) 
      ELSE
        EVAL tgt.put(info, fromfins)
      END
    END
  END MosInfoAdd;

PROCEDURE ProduceReports(p                   : TextList.T;
                         parsed              : BraceParse.T;
                         levels              : CARDINAL;
                         transistorReportSfx : TEXT) =
  BEGIN
    WHILE p # NIL DO
      VAR rootType := p.head;
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
  END ProduceReports;

BEGIN END Gox.
