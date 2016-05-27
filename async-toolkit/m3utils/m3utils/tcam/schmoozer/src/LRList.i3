INTERFACE LRList;
IMPORT LongrealList AS Type;
IMPORT Word;

TYPE T = Type.T;

PROCEDURE Hash(a : T) : Word.T;

PROCEDURE Equal(a, b : T) : BOOLEAN;

CONST Nth = Type.Nth;
CONST Length = Type.Length;
CONST Cons = Type.Cons;
CONST ReverseD = Type.ReverseD;
      
CONST Brand = "LRList";

END LRList.
