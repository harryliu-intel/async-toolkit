MODULE Main;
IMPORT ParseParams;
IMPORT Rd, FileRd, Stdio;
IMPORT Debug;
IMPORT BraceParse;
IMPORT Text;
FROM Fmt IMPORT F, Int;
IMPORT Atom, CellRec;
IMPORT Wx;
IMPORT OSError;
IMPORT AL;
IMPORT Thread;
IMPORT OpenCharArrayRefTbl;
IMPORT TextList;
IMPORT Pathname;
IMPORT Gox;
IMPORT DrawnWidth;
FROM TextUtils IMPORT ToLower;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;

VAR doDebug := Debug.GetLevel() >= 10;
CONST debugFins = FALSE;

CONST Usage = "[-w <formula> | -W <technology>] [-f <input filename|->] [-r <root cell>] [-t <transistor cell type list>] [-T <transistor report suffix>] [-l <levels>]";
      
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
  drawnWidth : DrawnWidth.T := NIL;
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

    IF pp.keywordPresent("-w") THEN
      drawnWidth := NEW(DrawnWidth.T).init(pp.getNext());
      IF debugFins THEN
        FOR i := 1 TO 40 DO
          Debug.Out(F("fins %s drawn %s nm", Int(i), Int(drawnWidth.eval(i))))
        END
      END
    END;

    IF pp.keywordPresent("-W") THEN
      WITH tech = ToLower(pp.getNext()) DO
        IF TE(tech, "n7") OR TE(tech, "n6") THEN
          drawnWidth := NEW(DrawnWidth.T).init("(lambda(nfin) (if (= nfin 1) 30 (- (* 30 nfin) 22)))")
        ELSE
          Debug.Error(F("?unknown tech \"%s\"", tech))
        END
      END
    END;
    
    pp.skipParsed();
    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  IF drawnWidth = NIL THEN
    Debug.Error("?don't know how to convert fins to widths: must specify -w <formula> or -W <technology name>")
  END;
  
  IF rd = NIL THEN Debug.Error("Must provide filename") END;

  IF transistorCellFn # NIL THEN
    ReadSpecialTransistorCells(transistorCellFn)
  END;
  
  TRY
    parsed := BraceParse.Parse(rd, transistorCells, drawnWidth);
  EXCEPT
    Rd.Failure(e) => Debug.Error("Main.m3: Trouble parsing input : Rd.Failure : "&
      AL.Format(e))
  END;

  IF doDebug THEN
    DebugDumpCells(parsed)
  END;

  Gox.ProduceReports(rootTypes, parsed, levels, transistorReportSfx)
END Main.

