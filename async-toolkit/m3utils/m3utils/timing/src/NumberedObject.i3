INTERFACE NumberedObject;

TYPE 
  T = BRANDED Brand OBJECT num : CARDINAL END;

CONST Brand = "NumberedObject";

PROCEDURE Compare(a, b : T) : [-1..1];

END NumberedObject.
