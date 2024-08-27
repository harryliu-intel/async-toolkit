INTERFACE Robust;
FROM NewUOAs IMPORT Output;
IMPORT LRVector, LRScalarField;

PROCEDURE Minimize(p              : LRVector.T;
                   func           : LRScalarField.T;
                   rhobeg, rhoend : LONGREAL;
                   extraDirs      : CARDINAL;
                   ftarget     := FIRST(LONGREAL)) : Output;

CONST Brand = "Robust";

END Robust.
