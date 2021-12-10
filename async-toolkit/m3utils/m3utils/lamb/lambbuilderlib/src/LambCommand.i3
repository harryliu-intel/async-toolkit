INTERFACE LambCommand;  (* should probably be a generic interface *)
IMPORT LambVerb AS Verb;


TYPE
  T = RECORD
    v          : Verb.T;
    p0, p1, p2 : INTEGER := LAST(INTEGER); (* should probably be "Integer.T" *)
  END;

CONST Brand = "Command(" & Verb.Brand & ")";

END LambCommand.

