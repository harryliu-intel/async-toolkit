INTERFACE Subcell;
IMPORT Atom;

TYPE
  T = RECORD
    type     : Atom.T;
    instance : REF ARRAY OF CHAR;
  END;

CONST Brand = "Subcell";

CONST Equal : PROCEDURE(a, b : T) : BOOLEAN = NIL;

END Subcell.
