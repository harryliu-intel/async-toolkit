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
        p := cellRec.subcells;
      BEGIN
        MosInfoAdd(tbl, cellRec.mosTbl);
        WHILE p # NIL DO
          RecurseTransistors(db, p.head.type, memoTbl, warnSet);
          WITH hadIt = memoTbl.get(p.head.type, tab) DO
            IF hadIt THEN
              MosInfoAdd(tbl, tab)
            ELSE
              IF NOT warnSet.member(p.head.type) THEN
                Debug.Warning(F("unknown cell type %s while processing %s",
                                Atom.ToText(p.head.type),
                                Atom.ToText(rootType)));
                EVAL warnSet.insert(p.head.type)
              END
            END
          END;
          p := p.tail
        END;
        EVAL memoTbl.put(rootType, tbl)
      END
    END
  END RecurseTransistors;

PROCEDURE MosInfoAdd(tgt, from : MosInfoCardTbl.T) =
  VAR
    iter := from.iterate();
    info : MosInfo.T;
    fromfins, tofins : CARDINAL;
  BEGIN
    WHILE iter.next(info, fromfins) DO
      IF tgt.get(info, tofins) THEN
        EVAL tgt.put(info, tofins + fromfins)
      ELSE
        EVAL tgt.put(info, fromfins)
      END
    END
  END MosInfoAdd;
  
VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  rd : Rd.T := NIL;
  cells : AtomCellTbl.T;
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
          rd := FileRd.Open(fn)
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
  
  cells := BraceParse.Parse(rd);

  IF doDebug THEN
    Debug.Out(F("Main.m3 got %s cells", Int(cells.size())));

    VAR
      iter := cells.iterate();
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
      RecurseTransistors(cells,
                         Atom.FromText(rootType),
                         memoTbl,
                         warnSet)
    END
  END

END Main.

