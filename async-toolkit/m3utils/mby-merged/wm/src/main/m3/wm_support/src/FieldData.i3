INTERFACE FieldData;
FROM ContainerData IMPORT Pos, Neg;
IMPORT Word; 

TYPE
  T = RECORD
    id   : Pos;
    up   : Neg;
    byte : Word.T;  (* limits us to 2^64 bytes *)
    lsb  : [0..8-1];
    wid  : CARDINAL; 
  END;

  AP = REF ARRAY OF T;
  
CONST Brand = "FieldData";

END FieldData.
