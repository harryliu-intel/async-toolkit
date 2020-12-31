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
IMPORT TextSpiceObjectTbl;
IMPORT Pathname;
IMPORT TextSet, TextSetDef;
IMPORT Stdio;
IMPORT Text;
IMPORT TextReader;
IMPORT Thread;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;


PROCEDURE DoOne( nm   : TEXT;
                 ckt  : SpiceCircuit.T ) =
  VAR
    symtab := NEW(TextSpiceObjectTbl.Default).init();
    
  BEGIN
    Debug.Out("Working on " & nm);
    Debug.Out(F("name %s", ckt.name));
    FOR i := 0 TO ckt.params.size() - 1 DO
      Debug.Out(F("param[%s] %s", Int(i), ckt.params.get(i)))
    END;
    FOR i := 0 TO ckt.elements.size() - 1 DO
      WITH obj = ckt.elements.get(i) DO
        <*ASSERT obj.name # NIL*>
        Debug.Out(F("elem[%s] %s", Int(i), obj.name));
        EVAL symtab.put(obj.name, obj)
      END
    END
  END DoOne;

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

TYPE
  Power      = { GND, Vdd };
  PowerSets  = ARRAY Power OF TextSet.T;

CONST
  PowerNames = ARRAY Power OF TEXT { "GND", "Vdd" };

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
    iter := spice.subCkts.iterate();
    t : TEXT;
    ckt : SpiceCircuit.T;
  BEGIN
    WHILE iter.next(t, ckt) DO
      DoOne(t, ckt)
    END
  END
      
END Main.
