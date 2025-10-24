INTERFACE BDDSetHash;
IMPORT BDDSetDef, BDDSet;
IMPORT Word;

TYPE T <: BDDSetDef.T;

PROCEDURE Hash(a : T) : Word.T;

CONST Equal = BDDSet.Equal;

CONST Brand = "BDDSetHash";

END BDDSetHash.
