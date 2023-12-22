INTERFACE LibertyComplexAttr;
IMPORT LibertyComponent;
IMPORT LibertyHead;

TYPE
  T <: Public;

  Public = LibertyComponent.T OBJECT
    head : LibertyHead.T;
    semi : BOOLEAN;
  END;

CONST Brand = "LibertyComplexAttr";

END LibertyComplexAttr.
 
