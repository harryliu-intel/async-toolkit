INTERFACE RdlInstanceRef;
IMPORT RdlInstanceRefElemSeq;
IMPORT RdlProperty;
IMPORT Word;

TYPE
  T = OBJECT
    dotted   : RdlInstanceRefElemSeq.T;
    property : RdlProperty.T;
  END;

CONST Brand = "RdlInstanceRef";

PROCEDURE Equal(a, b : T) : BOOLEAN;

PROCEDURE Hash(a : T) : Word.T;

PROCEDURE Format(a : T) : TEXT;

END RdlInstanceRef.
