INTERFACE LRScalarFieldPll;
IMPORT LRScalarField;

(* simple parallel version (wrapper) of LRScalarField.T *)

TYPE
  T <: Public;

  Public = LRScalarField.T OBJECT METHODS
    init(from : LRScalarField.T) : T;
  END;

CONST Brand = "LRScalarFieldPll";

END LRScalarFieldPll.
