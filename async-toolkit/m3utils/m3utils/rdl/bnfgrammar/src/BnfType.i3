INTERFACE BnfType;

TYPE
  T <: Public;

  Public = BRANDED OBJECT END;

CONST Brand = "BnfType";

PROCEDURE Equal(a, b : T) : BOOLEAN;

END BnfType.
