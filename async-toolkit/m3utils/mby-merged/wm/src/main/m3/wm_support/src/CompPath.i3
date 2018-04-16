INTERFACE CompPath;
IMPORT CompRange;

TYPE T = TEXT;

PROCEDURE Cat(a, b : T) : T;

PROCEDURE CatArray(a, b : T; i : CARDINAL) : T;

PROCEDURE Debug(reg : T; at : CompRange.T);
  
END CompPath.
