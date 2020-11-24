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

CONST TE = Text.Equal;

VAR
  doDebug := Debug.GetLevel() >= 10;
  
VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  rd : Rd.T := NIL;
  cells : AtomCellTbl.T;
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
  END

END Main.

