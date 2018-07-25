INTERFACE CompPath;
IMPORT CompRange;
IMPORT TextList;

TYPE T = TextList.T; (* do not use -- hack *)

PROCEDURE Cat(a : T; b : TEXT) : T;

PROCEDURE CatArray(a : T; b : TEXT; i : CARDINAL) : T;

PROCEDURE Debug(reg : T; at : CompRange.T);

PROCEDURE Empty() : T;

PROCEDURE ToText(t : T) : TEXT;

PROCEDURE One(txt : TEXT) : T;
  
END CompPath.
