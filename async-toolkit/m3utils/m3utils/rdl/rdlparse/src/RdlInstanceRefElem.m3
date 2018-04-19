MODULE RdlInstanceRefElem;
IMPORT Text, BigInt;
IMPORT Fmt;
IMPORT Word;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN
    TYPECASE a OF
      Id(aa) =>
      TYPECASE b OF Id(bb) => RETURN Text.Equal(aa.id, bb.id)
      ELSE RETURN FALSE END
    |
      Brack(aa) =>
      TYPECASE b OF Brack(bb) =>
        RETURN Text.Equal(aa.id, bb.id) AND
               BigInt.Equal(aa.idx.x,bb.idx.x)
      ELSE RETURN FALSE END
    ELSE
      <*ASSERT FALSE*>
    END
  END Equal;

PROCEDURE Hash(a : T) : Word.T =
  BEGIN
    TYPECASE a OF
      Id(id) => RETURN Text.Hash(id.id)
    |
      Brack(br) => RETURN Word.Plus(Text.Hash(br.id),BigInt.Hash(br.idx.x))
    ELSE
      <*ASSERT FALSE*>
    END
  END Hash;

PROCEDURE Format(a : T) : TEXT =
  BEGIN
    TYPECASE a OF
      Id(id) => RETURN id.id
    |
      Brack(br) => RETURN Fmt.F("%s[%s]",br.id,Fmt.Int(BigInt.ToInteger(br.idx.x)))
    ELSE
      <*ASSERT FALSE*>
    END
  END Format;

BEGIN END RdlInstanceRefElem.
