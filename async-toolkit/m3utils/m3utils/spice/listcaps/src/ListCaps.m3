MODULE ListCaps EXPORTS Main;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Debug;
IMPORT Params;
IMPORT Pathname;
IMPORT SpiceFormat;
IMPORT SpiceError;
IMPORT AL;
IMPORT Rd, FileRd;
IMPORT OSError;
FROM Fmt IMPORT F, Int, LongReal, Bool;
IMPORT SpiceCircuit;
IMPORT SpiceObject;
IMPORT Text;
IMPORT CitTextUtils;

CONST
  Usage = "";

VAR
  pp                          := NEW(ParseParams.T).init(Stdio.stderr);
  spiceFn    : Pathname.T     ;
  spice      : SpiceFormat.T;
  rootType   : TEXT := NIL;
  rootCkt    : SpiceCircuit.T;

BEGIN
  TRY
    IF pp.keywordPresent("-i") THEN
      spiceFn := pp.getNext()
    END;
    IF pp.keywordPresent("-root") THEN
      rootType := pp.getNext()
    END;
    pp.skipParsed();
    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  TRY
    WITH spiceRd = FileRd.Open(spiceFn) DO
      spice := SpiceFormat.ParseSpice(spiceRd, ".", spiceFn);
      Rd.Close(spiceRd)
    END
  EXCEPT
    OSError.E(e) =>
    Debug.Error(F("Can't open top level file %s : OSError.E : %s",
                  spiceFn, AL.Format(e)))
  |
    SpiceError.E(e) =>
    Debug.Error(F("Parsing input : caught SpiceError.E : %s at line %s of file %s",
                  e.msg, Int(e.lNo), Debug.UnNil(e.fn)))
  END;
  
  IF rootType = NIL THEN
    rootCkt := spice.topCkt
  ELSE
    WITH hadIt = spice.subCkts.get(rootType, rootCkt) DO
      IF NOT hadIt THEN
        Debug.Error(F("Unknown root type %s", rootType))
      END
    END
  END;

END ListCaps.
