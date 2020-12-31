MODULE Main;

(* spiceanalyze <spice-deck-filename> *)

IMPORT ParseParams;
IMPORT Rd;
IMPORT SpiceCircuit;
IMPORT SpiceFormat;
IMPORT OSError;
IMPORT SpiceError;
IMPORT FileRd;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT AL;
IMPORT Pathname;
IMPORT TextSetDef;
IMPORT Stdio;
IMPORT Text;
IMPORT TextReader;
IMPORT Thread;
IMPORT SpiceAnalyze;
FROM SpiceAnalyze IMPORT Power, PowerSets, PowerNames;
IMPORT TextCktCellTbl;
FROM SpiceFlat IMPORT VisitCktNodes, CleanAssocs;
IMPORT TextTextSetTbl;
IMPORT SpiceInstance;
IMPORT TextSpiceInstanceSetTbl;
IMPORT TextTextTbl;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;


PROCEDURE TryOpenFile(fn : Pathname.T; VAR rd : Rd.T) =
  BEGIN
    IF TE(fn, "-") THEN
      rd := Stdio.stdin
    ELSE
      TRY
        rd := FileRd.Open(fn)
      EXCEPT
        OSError.E(e) =>
        Debug.Error(F("Can't open top level file %s : OSError.E : %s",
                      fn, AL.Format(e)))
      END
    END
  END TryOpenFile;

PROCEDURE ParsePower(rd : Rd.T; VAR ps : PowerSets)
  RAISES { Rd.Failure, SpiceError.E } =
  CONST
    White = " \t,";
  VAR
    lNo := 0;
    p : Power;
    success := FALSE;
  BEGIN
    TRY
      LOOP
        INC(lNo);
        VAR
          line := Rd.GetLine(rd);
          reader := NEW(TextReader.Skips).init(line);
          kw : TEXT;
        BEGIN
          IF reader.next(White, kw) THEN
            FOR i := FIRST(Power) TO LAST(Power) DO
              IF TE(kw, PowerNames[i] & ":") THEN
                p := i; success := TRUE; EXIT
              END
            END;
            IF NOT success THEN
              RAISE SpiceError.E (SpiceError.Data {
              F("unknown supply %s parsing power",kw),
              lNo := lNo })
            END;
            WHILE reader.next(White, kw) DO
              WITH hadIt = ps[p].insert(kw) DO
                IF hadIt THEN
                  RAISE SpiceError.E (SpiceError.Data {
                  F("parsing power : %s multiply defined",kw),
                  lNo := lNo })
                END
              END
            END
          END
        END
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    END
  END ParsePower;
  
VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);

  powerFn, fn : Pathname.T := NIL;
  pRd, rd : Rd.T;

  spice : SpiceFormat.T;
  powerSets := PowerSets { NEW(TextSetDef.T).init(), .. };
BEGIN
  TRY
    IF pp.keywordPresent("-power") OR pp.keywordPresent("-p") THEN
      powerFn := pp.getNext()
    END;
    IF pp.keywordPresent("-f") THEN
      fn := pp.getNext()
    END;
    IF fn = NIL THEN
      Debug.Error("must specify -f")
    END;
    pp.skipParsed();
    pp.finish();
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;
  
  IF powerFn # NIL THEN
    TryOpenFile(powerFn, pRd);
    TRY
      ParsePower(pRd, powerSets)
    EXCEPT
      Rd.Failure(x) => Debug.Error(F("Trouble reading power file \"%s\": Rd.Failure : %s", powerFn, AL.Format(x)))
    |
      SpiceError.E(e) =>
      Debug.Error(F("Parsing input : caught SpiceError.E : %s at line %s of file %s",
                    e.msg, Int(e.lNo), Debug.UnNil(powerFn)))
    END
  END;
  
  TryOpenFile(fn, rd);
  TRY
    spice := SpiceFormat.ParseSpice(rd, ".", fn);
  EXCEPT
      Rd.Failure(x) => Debug.Error(F("Trouble reading input file \"%s\" (or its descendants): Rd.Failure : %s", fn, AL.Format(x)))
    |
    SpiceError.E(e) =>
    Debug.Error(F("Parsing input : caught SpiceError.E : %s at line %s of file %s",
                  e.msg, Int(e.lNo), Debug.UnNil(e.fn)))
  END;

  Debug.Out(F("subCkts : %s", Int(spice.subCkts.size())));
  
  VAR
    topName := "X1"; (* s.b. cmd-line param *)
    symTab := NEW(TextTextSetTbl.Default).init();
    assocs := NEW(TextSpiceInstanceSetTbl.Default).init();
    topInstance := NEW(SpiceInstance.T).init("X1", NIL (* not right *), NIL);
    canonTbl := NEW(TextTextTbl.Default).init();
  BEGIN
    spice.topCkt.name := topName; (* wont be named otherwise *)
    
    VisitCktNodes(topName,
                  symTab,
                  spice.topCkt,
                  NIL,
                  assocs,
                  topInstance,
                  spice.subCkts);
    assocs := CleanAssocs(assocs, canonTbl);
  END;
  
  VAR
    ckt : SpiceCircuit.T;
    hierTbl := NEW(TextCktCellTbl.Default).init();
  BEGIN
    FOR i := 0 TO spice.subCktNames.size() - 1 DO
      WITH nm = spice.subCktNames.get(i),
           hadIt = spice.subCkts.get(nm, ckt) DO
        <*ASSERT hadIt*>
        SpiceAnalyze.Cell(nm, ckt, powerSets, hierTbl, spice.subCkts)
      END
    END
  END
      
END Main.
