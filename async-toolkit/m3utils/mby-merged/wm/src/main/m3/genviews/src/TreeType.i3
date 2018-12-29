INTERFACE TreeType;
IMPORT RegComponent;

TYPE
  T = OBJECT
    sz : CARDINAL;
    comp : RegComponent.T;
   END;

  Array = T OBJECT
    n : CARDINAL;
    elem : T;
  END;

  Struct <: T; (* see TreeTypeStruct for more details *)

  Field = T BRANDED OBJECT END;

PROCEDURE Format(type : T) : TEXT;
  
CONST Brand = "TreeType";

END TreeType.
