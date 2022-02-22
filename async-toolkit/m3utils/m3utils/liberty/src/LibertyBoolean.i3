INTERFACE LibertyBoolean;
IMPORT LibertyComponent;

TYPE
  T <: Public;

  Public = LibertyComponent.T OBJECT
    val : BOOLEAN;
  END;

CONST Brand = "LibertyBoolean";

END LibertyBoolean.
