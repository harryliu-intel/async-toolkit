MODULE Fsdb;

IMPORT TextSeq;
IMPORT TextSet;
IMPORT RegExList;
IMPORT Pathname;
IMPORT Rd;
IMPORT Wr;
IMPORT ProcUtils;
IMPORT Debug;
FROM Fmt IMPORT F, Int, LongReal;
IMPORT TextReader;
IMPORT Text;
IMPORT TextUtils, Lex, FloatMode;
IMPORT Scan;
IMPORT LongRealSeq AS LRSeq;
IMPORT Word;
IMPORT Process;
IMPORT NameControl; FROM NameControl IMPORT MakeIdxMap;

FROM Tr0 IMPORT ShortRead, SyntaxError;

CONST TE = Text.Equal;
      LR = LongReal;

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

  PROCEDURE GetLineUntil(term : TEXT; VAR line : TEXT) : BOOLEAN =
    BEGIN
      WITH this    = Rd.GetLine(rd) DO
        IF TE(this, term) THEN
          RETURN FALSE
        ELSE
          line := this;
          RETURN TRUE
        END
      END
    END GetLineUntil;
    
  VAR
    loId, hiId : CARDINAL;
    unit : LONGREAL;
    line : TEXT;
    timesteps := NEW(LRSeq.T).init();
  CONST
    TwoToThe32 = FLOAT(Word.Shift(1, 32), LONGREAL);
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

    (* load first node *)
    PutCommand(F("R %s %s", Int(loId), Int(loId)));
    EVAL GetResponse("RR");

    (* get timesteps *)
    PutCommand(F("I %s", Int(loId)));
    WHILE GetLineUntil("IR", line) DO
      WITH reader = NEW(TextReader.T).init(line),
           h = reader.getInt(),
           l = reader.getInt(),
           s = FLOAT(h, LONGREAL) * TwoToThe32 + FLOAT(l, LONGREAL),
           t = s * unit DO
        IF FALSE THEN Debug.Out("timestep " & LR(t)) END;
        timesteps.addhi(t)
      END
    END;

    Debug.Out(F("timesteps %s min %s max %s",
                Int(timesteps.size()),
                LR(timesteps.get(0)),
                LR(timesteps.get(timesteps.size()-1))));

    PutCommand("U");
    EVAL GetResponse("UR");

    PutCommand(F("N %s %s", Int(loId), Int(hiId)));
    WHILE GetLineUntil("NR", line) DO
      TRY
        WITH reader = NEW(TextReader.T).init(line),
             idx    = reader.getInt(),
             nm     = reader.get(),
             type   = reader.get() DO
          (*Debug.Out(F("name %s id %s", nm, Int(idx)));*)
          names.addlo(nm)
        END
      EXCEPT
        Lex.Error, FloatMode.Trap =>
        Debug.Error(F("Cant parse N response \"%s\"", line))
      END
    END;
    names.addlo("TIME"); (* implicit #0 *)
        

    Debug.Out(F("names : %s first %s last %s ",
                Int(names.size()),
                names.get(0),
                names.get(names.size()-1)));

    (* now we have all names loaded up *)

    
    Process.Exit(0);
  END Parse;

PROCEDURE ParseUnitStr(unitSpec : TEXT) : LONGREAL =
  BEGIN
    FOR i := FIRST(Units) TO LAST(Units) DO
      WITH u = Units[i] DO
        IF TextUtils.HaveSuffix(unitSpec, u.sfx) THEN
          TRY
            WITH val = Scan.LongReal(TextUtils.RemoveSuffix(unitSpec, u.sfx)),
                 res = val * u.val DO
              Debug.Out(F("ParseUnitStr \"%s\" : val %s u.val %s res %s",
                          unitSpec,
                          LR(val),
                          LR(u.val),
                          LR(res)));
              RETURN res
            END
          EXCEPT
            Lex.Error, FloatMode.Trap => (* try again *)
          END
        END
      END
    END;
    Debug.Error("UnitSpec not understood : " & unitSpec);
    <*ASSERT FALSE*>
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
