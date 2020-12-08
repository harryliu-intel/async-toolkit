INTERFACE FinInfo;

TYPE T = ARRAY [0..2] OF CARDINAL;

CONST Brand = "FinInfo";

PROCEDURE Add(READONLY a, b : T) : T;

END FinInfo.
