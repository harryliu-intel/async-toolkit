INTERFACE LibertyNumber;
IMPORT LibertyComponent;

TYPE
  T <: LibertyComponent.T;

  Integer = T OBJECT
    val : INTEGER;
  END;

  Floating = T OBJECT
    val : LONGREAL;
  END;

CONST Brand = "LibertyNumber";

END LibertyNumber.
