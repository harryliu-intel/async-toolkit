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
<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;

VAR doDebug := Debug.GetLevel() >= 10;

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

    pp.skipParsed()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  IF rd = NIL THEN Debug.Error("Must provide filename") END;

  TRY
    parsed := BraceParse.Parse(rd);
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
    BEGIN
      RecurseTransistors(parsed.cellTbl,
                         Atom.FromText(rootType),
                         memoTbl,
                         warnSet)
    END
  END

END Main.

