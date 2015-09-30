INTERFACE Command;
IMPORT Verb;

TYPE
  T = RECORD
    v      : Verb.T;
    p0, p1 : INTEGER := LAST(INTEGER); (* should probably be "Integer.T" *)
  END;

CONST Brand = "Command";

END Command.

