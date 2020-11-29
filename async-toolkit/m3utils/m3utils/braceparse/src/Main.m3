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
IMPORT AtomSet;
IMPORT CardPair;
IMPORT OSError;
IMPORT AL;
IMPORT Thread;
IMPORT OpenCharArrayRefTbl;
IMPORT TextList;
IMPORT Pathname;
IMPORT Gox;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;

VAR doDebug := Debug.GetLevel() >= 10;

PROCEDURE RecurseTransistors(db       : AtomCellTbl.T;
                             rootType : CellRec.T;
                             memoTbl  : AtomMosInfoCardTblTbl.T;
                             warnSet  : AtomSet.T) =
  (* this routine builds the memoTbl (memoization table) of 
     the transistor counts in the cell including all subcells 

     The output is a table 

     keyed on the Atom.T of the cell type name

     The value being a MosInfoCardTbl.T, which contains the count for
     every transistor subtype (transistor type and unique length encountered)
     for the cell in question and all its subcells.
  *)
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

  Gox.ProduceReports(rootTypes, parsed, levels, transistorReportSfx)
END Main.

