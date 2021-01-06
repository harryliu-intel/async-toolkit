MODULE PicText;
IMPORT Text;
IMPORT Word;
IMPORT PicPoint;
IMPORT PicExtent;
IMPORT LongReal;

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
    w := ARRAY [ 0 .. 3 ] OF Word.T { PicPoint.Hash(a.ll),
                                  Text.Hash(a.txt),
                                  LongReal.Hash(a.size+a.width),
                                  ORD(a.fontType) };
    x : Word.T := 0;
  BEGIN
    FOR i := FIRST(w) TO LAST(w) DO
      x := Word.Plus(x, w[i])
    END;
    RETURN x
  END Hash;

PROCEDURE Extent(READONLY a : T) : PicExtent.T =
  BEGIN
    RETURN PicExtent.T { a.ll,
                         PicPoint.T { a.ll.x + a.width,
                                      a.ll.y + a.size } }
  END Extent;

PROCEDURE Translate(READONLY a : T; READONLY by : PicPoint.T) : T =
  VAR
    res := a;
  BEGIN
    res.ll := PicPoint.Plus(a.ll, by);
    RETURN res
  END Translate;
  
BEGIN END PicText.
