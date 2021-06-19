MODULE Fsdb;

IMPORT TextSeq;
IMPORT TextSet;
IMPORT RegExList;
IMPORT Pathname;
IMPORT Rd;
IMPORT Wr;
IMPORT ProcUtils;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT TextReader;
IMPORT Text;
IMPORT TextUtils, Lex, FloatMode;
IMPORT Scan;

FROM Tr0 IMPORT ShortRead, SyntaxError;

CONST TE = Text.Equal;

PROCEDURE Parse(wd, ofn       : Pathname.T;
                names         : TextSeq.T;
                maxFiles      : CARDINAL;
                VAR nFiles    : CARDINAL;
                MaxMem        : CARDINAL;

                timeScaleFactor,
                timeOffset,
                voltageScaleFactor,
                voltageOffset : LONGREAL;

                dutName       : TEXT;
                 
                fsdbPath      : Pathname.T;
                wait          : BOOLEAN;
                restrictNodes : TextSet.T;
                restrictRegEx : RegExList.T;
                cmdPath       : Pathname.T)
  RAISES { Rd.Failure, ShortRead, SyntaxError } =
  VAR
    wr : Wr.T;
    stdin := ProcUtils.GimmeWr(wr);

    rd : Rd.T;
    stdout := ProcUtils.GimmeRd(rd);
    
    completion := ProcUtils.RunText(cmdPath & " " & fsdbPath,
                                    stdin := stdin,
                                    stderr := ProcUtils.Stderr(),
                                    stdout := stdout);
  PROCEDURE PutCommand(cmd : TEXT) =
    BEGIN
      Debug.Out(F("Fsdb.Parse.PutCommand \"%s\"", cmd));
      Wr.PutText(wr, cmd);
      Wr.PutChar(wr, '\n');
      Wr.Flush(wr);
    END PutCommand;

  PROCEDURE GetResponse(matchKw : TEXT) : TextReader.T =
    VAR
      kw : TEXT;
    BEGIN
      LOOP
        WITH line    = Rd.GetLine(rd),
             reader  = NEW(TextReader.T).init(line) DO
          Debug.Out(F("Fsdb.Parse.GetResponse \"%s\"", line));
          IF reader.next(" ", kw, TRUE) THEN
            IF TE(kw, matchKw) THEN
              RETURN reader
            END
          END
        END
      END
    END GetResponse;

  VAR
    loId, hiId : CARDINAL;
    unit : LONGREAL;
  BEGIN
    PutCommand("S");
    WITH reader    = GetResponse("SR") DO
      loId   := reader.getInt();
      hiId   := reader.getInt();
      WITH unitStr = reader.get() DO
        unit   := ParseUnitStr(unitStr);
      
        Debug.Out(F("Got query response lo=%s hi=%s unitStr=\"%s\"",
                    Int(loId), Int(hiId), unitStr))
      END
    END;

    
  END Parse;

PROCEDURE ParseUnitStr(unitSpec : TEXT) : LONGREAL =
  BEGIN
    unitSpec := TextUtils.ToLower(unitSpec);
    FOR i := FIRST(Units) TO LAST(Units) DO
      WITH u = Units[i] DO
        IF TextUtils.HaveSuffix(unitSpec, u.sfx) THEN
          TRY
            WITH val = Scan.LongReal(TextUtils.RemoveSuffix(unitSpec, u.sfx)) DO
              RETURN val * u.val
            END
          EXCEPT
            Lex.Error, FloatMode.Trap => (* try again *)
          END
        END
      END
    END;
    Debug.Error("UnitSpec not understood : " & unitSpec)
  END ParseUnitStr;

TYPE
  UnitSuffix = RECORD sfx : TEXT; val : LONGREAL END;

CONST
  Units = ARRAY OF UnitSuffix {
  UnitSuffix { "E" , 1.0d+18 },
  UnitSuffix { "P" , 1.0d+15 },
  UnitSuffix { "T" , 1.0d+12 },
  UnitSuffix { "G" , 1.0d+09 }, 
  UnitSuffix { "M" , 1.0d+06 },
  UnitSuffix { "k" , 1.0d+03 },
  UnitSuffix { "h" , 1.0d+02 },
  UnitSuffix { "da", 1.0d+01 },
  UnitSuffix { ""  , 1.0d-00},
  UnitSuffix { "d" , 1.0d-01 },
  UnitSuffix { "c" , 1.0d-02 },
  UnitSuffix { "m" , 1.0d-03 },
  UnitSuffix { "u" , 1.0d-06 }, (* Greek mu *)
  UnitSuffix { "n" , 1.0d-09 },
  UnitSuffix { "p" , 1.0d-12 },
  UnitSuffix { "f" , 1.0d-15 },
  UnitSuffix { "a" , 1.0d-19 } };
 

BEGIN
  <*ASSERT ParseUnitStr("12") = 12.0d0*>
  <*ASSERT ParseUnitStr("1da") = 10.0d0*>
  <*ASSERT ParseUnitStr("1M") = 1.0d6*>
END Fsdb.
