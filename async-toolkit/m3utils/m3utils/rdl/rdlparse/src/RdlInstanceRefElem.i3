INTERFACE RdlInstanceRefElem;
IMPORT RdlNum;
IMPORT Word;

TYPE
  T = ROOT BRANDED Brand OBJECT END;

  Id = T BRANDED Brand & " Id" OBJECT id : TEXT END;

  Brack = T BRANDED Brand & " Brack" OBJECT id : TEXT; idx : RdlNum.T END;

CONST Brand = "RdlInstanceRefElem";

PROCEDURE Equal(a, b : T) : BOOLEAN;

PROCEDURE Hash(a : T) : Word.T;

PROCEDURE Format(a : T) : TEXT;

END RdlInstanceRefElem.
