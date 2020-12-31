MODULE Main;

(* spiceanalyze <spice-deck-filename> *)

IMPORT Params;
IMPORT Rd;
IMPORT SpiceCircuit;
IMPORT SpiceFormat;
IMPORT OSError;
IMPORT SpiceError;
IMPORT FileRd;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT AL;

PROCEDURE DoOne( t   : TEXT;
                 ckt : SpiceCircuit.T ) =
  BEGIN
  END DoOne;

VAR
  fn := Params.Get(1);
  rd : Rd.T;

  spice : SpiceFormat.T;

BEGIN
  TRY
    rd := FileRd.Open(fn);
    spice := SpiceFormat.ParseSpice(rd, ".", fn);
  EXCEPT
    OSError.E(e) =>
    Debug.Error(F("Can't open top level file %s : OSError.E : %s",
                  fn, AL.Format(e)))
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
