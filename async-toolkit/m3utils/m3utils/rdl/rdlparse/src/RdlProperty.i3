INTERFACE RdlProperty;
IMPORT RdlPredefProperty;
IMPORT Word;

TYPE
  T = ROOT BRANDED Brand OBJECT END;

  Predef = T BRANDED Brand & " Predef" OBJECT
    x : RdlPredefProperty.T;
  END;

  Userdef = T BRANDED Brand & "Userdef" OBJECT
    nm : TEXT;
  END;

CONST Brand = "RdlProperty";

PROCEDURE Equal(a, b : T) : BOOLEAN;

PROCEDURE Make(nm : TEXT) : T;

PROCEDURE Format(t : T) : TEXT;

PROCEDURE Hash(t : T) : Word.T;
  
END RdlProperty.
  
