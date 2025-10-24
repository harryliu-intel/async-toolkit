MODULE LibData;
IMPORT Word;
IMPORT Text;

PROCEDURE Hash(a : T) : Word.T =
  VAR
    res : Word.T := TYPECODE(a);
  BEGIN
    TYPECASE a OF
      LibRef(lr) => INC(res, Text.Hash(lr.nm))
    |
      ModelRef(mr) =>
      res := Word.Plus(res, Text.Hash(mr.type));
      res := Word.Plus(res, Text.Hash(mr.nm))
    ELSE
      (* skip *)
    END;

    RETURN res
  END Hash;
  
BEGIN END LibData.
