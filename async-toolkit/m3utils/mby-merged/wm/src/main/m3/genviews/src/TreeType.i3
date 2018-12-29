INTERFACE TreeType;
IMPORT RegComponent;
IMPORT Word;

TYPE
  T = OBJECT
    tag      : TEXT;
    sz       : CARDINAL;
    comp     : RegComponent.T;
    offset   : CARDINAL; (* offset from parent -- only valid for instance *)
    address  : CARDINAL; (* global field address of first instance *)
    addrBits : Word.T;   (* global bit address of first instance *)
   END;

  Array = T OBJECT
    n          : CARDINAL;
    elem       : T;
    stride     : CARDINAL;
    strideBits : Word.T;
  END;

  Struct <: T; (* see TreeTypeStruct for more details *)

  Field = T BRANDED OBJECT END;

PROCEDURE Format(type : T) : TEXT;

PROCEDURE ComputeAddresses(tree : T; base : CARDINAL; ac : AddressConverter);
  
CONST Brand = "TreeType";

TYPE
  AddressConverter = OBJECT METHODS
    field2bit(field : CARDINAL) : Word.T;
  END;

END TreeType.
