INTERFACE LambCommand;  (* should probably be a generic interface *)
IMPORT LambVerb AS Verb;
IMPORT BitInteger;

TYPE
  T = RECORD
    v          : Verb.T;
    p0, p1, p2 : BitInteger.T :=  NIL;
  END;

CONST Brand = "Command(" & Verb.Brand & ")";

END LambCommand.

