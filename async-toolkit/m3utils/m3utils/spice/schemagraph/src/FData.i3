INTERFACE FData;
IMPORT Field;

TYPE
  T =  RECORD
    type    : Field.T;
    name    : TEXT;
    formula : TEXT;
  END;
  
CONST Brand = "FData";

PROCEDURE Format(READONLY t : T) : TEXT;
      
END FData.
