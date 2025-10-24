INTERFACE Arith;
IMPORT Word, Refany;

TYPE T = ROOT BRANDED OBJECT id : Word.T := 0 END;

CONST Equal = Refany.Equal;

PROCEDURE Hash(a : T) : Word.T;

CONST Brand = "Arith";

END Arith.
