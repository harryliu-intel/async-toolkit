MODULE Main;
IMPORT ParseParams;
IMPORT Rd, FileRd, Stdio;
IMPORT Debug;
IMPORT BraceParse;
IMPORT Text;
IMPORT AtomCellTbl;
FROM Fmt IMPORT F, Int;
IMPORT Atom, CellRec;
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
IMPORT Subcell;
<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;

VAR doDebug := Debug.GetLevel() >= 10;

PROCEDURE DoTransistorReports(wr       : Wr.T;
                              db       : AtomCellTbl.T;
                              rootType : Atom.T;
                              memoTbl  : AtomMosInfoCardTblTbl.T;
                              level    : CARDINAL;
                              nlevels  : CARDINAL;
                              path     : TEXT;
                              longNames: Subcell.LongNames)
  RAISES { Wr.Failure } =

  VAR
    tab : MosInfoCardTbl.T;
    iter : MosInfoCardTbl.Iterator;
    mosInfo : MosInfo.T;
    finCnt : CardPair.T;
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
      Wx.PutInt(wx, finCnt.k2);
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

    IF level < nlevels THEN
      VAR
        cell : CellRec.T;
      BEGIN
        WITH hadIt = db.get(rootType, cell) DO
          IF NOT hadIt THEN RETURN END;
          FOR i := FIRST(cell.subcells^) TO LAST(cell.subcells^) DO
            VAR
              sub := cell.subcells[i];
              decodedSubName := Subcell.DecodeNameToText(longNames,
                                                         sub.instance);
            BEGIN
              DoTransistorReports(wr,
                                  db,
                                  sub.type,
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

PROCEDURE RecurseTransistors(db       : AtomCellTbl.T;
                             rootType : Atom.T;
                             memoTbl  : AtomMosInfoCardTblTbl.T;
                             warnSet  : AtomSet.T) =
  VAR
    tab : MosInfoCardTbl.T;
  BEGIN
    (* check if already computed *)
    IF memoTbl.get(rootType, tab) THEN RETURN END;

    (* not yet computed, compute it! *)
    VAR
      cellRec : CellRec.T;
    BEGIN
      WITH hadIt = db.get(rootType, cellRec) DO
        IF NOT hadIt THEN
          RETURN
        END
      END;
      VAR
        tbl := NEW(MosInfoCardTbl.Default).init();
      BEGIN
        MosInfoAdd(tbl, cellRec.mosTbl);
        FOR i := FIRST(cellRec.subcells^) TO LAST(cellRec.subcells^) DO
          WITH sc = cellRec.subcells[i] DO
            RecurseTransistors(db, sc.type, memoTbl, warnSet);
            WITH hadIt = memoTbl.get(sc.type, tab) DO
              IF hadIt THEN
                MosInfoAdd(tbl, tab)
              ELSE
                IF NOT warnSet.member(sc.type) THEN
                  Debug.Warning(F("unknown cell type %s while processing %s",
                                  Atom.ToText(sc.type),
                                  Atom.ToText(rootType)));
                  EVAL warnSet.insert(sc.type)
                END
              END
            END
          END
        END;
        EVAL memoTbl.put(rootType, tbl)
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
  
VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  rd : Rd.T := NIL;
  parsed : BraceParse.T;
  rootType : TEXT := NIL;
  transistorCellFn : TEXT := NIL;
  transistorReportFn : TEXT := NIL;
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
      rootType := pp.getNext()
    END;

    IF pp.keywordPresent("-t") OR pp.keywordPresent("-transistorcelltypes") THEN
      transistorCellFn := pp.getNext()
    END;

    IF pp.keywordPresent("-T") OR pp.keywordPresent("-transistorreport") THEN
      transistorReportFn := pp.getNext()
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
  END;
  
  TRY
    parsed := BraceParse.Parse(rd, transistorCells);
  EXCEPT
    Rd.Failure(e) => Debug.Error("Main.m3: Trouble parsing input : Rd.Failure : "&
      AL.Format(e))
  END;

  IF doDebug THEN
    Debug.Out(F("Main.m3 got %s cells", Int(parsed.cellTbl.size())));

    VAR
      iter := parsed.cellTbl.iterate();
      nm : Atom.T;
      cell : CellRec.T;
    BEGIN
      WHILE iter.next(nm, cell) DO
        VAR
          wx := Wx.New();
        BEGIN
          CellRec.DebugOut(cell, wx);
          Debug.Out(Wx.ToText(wx))
        END
      END
    END
  END;

  IF rootType # NIL THEN
    VAR memoTbl := NEW(AtomMosInfoCardTblTbl.Default).init();
        warnSet := NEW(AtomSetDef.T).init();
        dummy   : CellRec.T;
        rootTypeA := Atom.FromText(rootType);
    BEGIN
      IF NOT parsed.cellTbl.get(rootTypeA, dummy) THEN
        Debug.Error("Cannot find root type : " & rootType)
      END;
      RecurseTransistors(parsed.cellTbl,
                         rootTypeA,
                         memoTbl,
                         warnSet);
      IF transistorReportFn # NIL THEN
        TRY
          WITH wr = FileWr.Open(transistorReportFn) DO
            DoTransistorReports(wr,
                                parsed.cellTbl,
                                rootTypeA,
                                memoTbl,
                                0,
                                levels,
                                "(ROOT)",
                                parsed.longNames);
            Wr.Close(wr)
          END
        EXCEPT
          OSError.E(x) =>
          Debug.Error(F("Trouble opening/closing transistor reports file %s : OSError.E : %s",
                        transistorReportFn,
                        AL.Format(x)))
        |
          Wr.Failure(x) =>
          Debug.Error(F("Trouble writing transistor reports file %s : Wr.Failure : %s",
                        transistorReportFn,
                        AL.Format(x)))
        END
      END
    END
  END
END Main.

