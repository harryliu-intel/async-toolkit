(* $Id$ *)
INTERFACE AdGrid;
IMPORT LRScalarField, LRPoint;

TYPE
  T <: Public;

  Public = LRScalarField.T OBJECT METHODS
    init(f : LRScalarField.T; ll, ur : LRPoint.T) : T;
    evalP(READONLY at : LRPoint.T; prec : LONGREAL := 0.01d0) : LONGREAL;
    setPrec(prec : LONGREAL);
  END;

CONST Brand = "AdGrid";

END AdGrid.
