MODULE PicText;
IMPORT Text;
IMPORT Word;
IMPORT PicPoint;

CONST TE = Text.Equal;
      
PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  VAR
    c := b;
  BEGIN
    (* work around ref eq. of texts *)
    IF TE(a.txt, b.txt) THEN c.txt := a.txt END;

    RETURN a = c
  END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  VAR
    w := ARRAY [0..4] OF Word.T { PicPoint.Hash(a.ll),
                                  Text.Hash(a.txt),
                                  a.size,
                                  0(*LongReal.Hash(a.width)*),
                                  ORD(a.fontType) };
    x : Word.T := 0;
  BEGIN
    FOR i := FIRST(w) TO LAST(w) DO
      x := Word.Plus(x, w[i])
    END;
    RETURN x
  END Hash;

BEGIN END PicText.
