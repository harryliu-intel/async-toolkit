MODULE MosInfo;
IMPORT Word;
IMPORT Atom;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN
    RETURN a.type = b.type AND a.len = b.len
  END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN
    RETURN Word.Plus(Atom.Hash(a.type), a.len)
  END Hash;

BEGIN END MosInfo.
  
