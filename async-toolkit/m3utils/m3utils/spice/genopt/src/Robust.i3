INTERFACE Robust;
FROM NewUOAs IMPORT Output;
IMPORT LRVector, LRScalarField;

(* this is the Robust parallel minimizer.

   It's somewhat based on (classic) Powell's method, but it is parallel. 
*)

PROCEDURE Minimize(p              : LRVector.T;
                   func           : LRScalarField.T;
                   rhobeg, rhoend : LONGREAL (* same as Powell *)) : Output;

CONST Brand = "Robust";

END Robust.
