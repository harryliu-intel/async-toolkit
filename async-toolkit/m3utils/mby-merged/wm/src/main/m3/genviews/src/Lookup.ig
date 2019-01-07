GENERIC INTERFACE Lookup(Type);
IMPORT ParseError;

PROCEDURE Parse(str : TEXT) : Type.T RAISES { ParseError.E };

CONST Brand = "Lookup(" & Type.Brand & ")";

END Lookup.
