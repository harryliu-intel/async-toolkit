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
IMPORT FS;

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
    END;

    FOR i := FIRST(Power) TO LAST(Power) DO
      Debug.Out("Power set " & PowerNames[i]);
      VAR str := "";
          iter := ps[i].iterate();
          nm : TEXT;
      BEGIN
        WHILE iter.next(nm) DO
          str := str & nm & " "
        END;
        Debug.Out(str)
      END
    END;
  END ParsePower;
  
VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);

  powerFn, fn : Pathname.T := NIL;
  pRd, rd : Rd.T;

  spice : SpiceFormat.T;
  powerSets := PowerSets { NEW(TextSetDef.T).init(),
                           NEW(TextSetDef.T).init() };
  outDir : Pathname.T := ".";
BEGIN
  TRY
    IF pp.keywordPresent("-power") OR pp.keywordPresent("-p") THEN
      powerFn := pp.getNext()
    END;
    IF pp.keywordPresent("-f") THEN
      fn := pp.getNext()
    END;
    IF pp.keywordPresent("-o") THEN
      outDir := pp.getNext()
    END;
    IF fn = NIL THEN
      Debug.Error("must specify -f")
    END;
    pp.skipParsed();
    pp.finish();
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  TRY FS.CreateDirectory(outDir) EXCEPT ELSE END;
  
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
    ckt : SpiceCircuit.T;
    hierTbl := NEW(TextCktCellTbl.Default).init();
  BEGIN
    FOR i := 0 TO spice.subCktNames.size() - 1 DO

      (* this loop walks through all the SUBCKT regions in the input,
         in order *)
      
      WITH nm    = spice.subCktNames.get(i),
           hadIt = spice.subCkts.get(nm, ckt) DO
        <*ASSERT hadIt*>
        (* first find all the aliases *)
        SpiceAnalyze.Cell(nm,
                          ckt, powerSets, hierTbl, spice.subCkts, outDir)
      END
    END;

    SpiceAnalyze.Cell("TOP",
                      spice.topCkt, powerSets, hierTbl, spice.subCkts, outDir)
  END
      
END Main.
