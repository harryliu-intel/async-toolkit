(* $Id$ *)
INTERFACE AdGrid;
IMPORT LRScalarField, LRPoint, AdGridQSet;

TYPE
  T <: Public;

  Public = LRScalarField.T OBJECT METHODS
    init(f : LRScalarField.T; ll, ur : LRPoint.T; initLevels := 0) : T;
    evalP(READONLY at : LRPoint.T; prec : LONGREAL := 0.01d0) : LONGREAL;
    setPrec(prec : LONGREAL);
    getQuadsContainingLevel(level : LONGREAL) : AdGridQSet.T;
  END;

CONST Brand = "AdGrid";

END AdGrid.
