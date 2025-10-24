INTERFACE LibData;
IMPORT Word, Refany;
IMPORT TextPairSeq;

TYPE
  T = BRANDED OBJECT END;

  LibRef = T OBJECT nm : TEXT END;

  ModelRef = T OBJECT
    type, nm : TEXT;
    params : TextPairSeq.T;
  END;

CONST Brand = "LibData";

PROCEDURE Hash(a : T) : Word.T;

CONST Equal = Refany.Equal;
      
END LibData.
