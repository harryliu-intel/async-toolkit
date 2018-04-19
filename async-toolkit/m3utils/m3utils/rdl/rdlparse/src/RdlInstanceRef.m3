MODULE RdlInstanceRef;
IMPORT Word;
IMPORT RdlInstanceRefElem AS Elem;
IMPORT RdlInstanceRefElemSeq AS ElemSeq;
IMPORT RdlProperty;

PROCEDURE HashSeq(dotted : ElemSeq.T) : Word.T =
  VAR
    x : Word.T := 0;
  BEGIN
    FOR i := 0 TO dotted.size()-1 DO
      x := Word.Plus(x, Elem.Hash(dotted.get(i)))
    END;
    RETURN x
  END HashSeq;

PROCEDURE EqSeq(d0, d1 : ElemSeq.T) : BOOLEAN =
  BEGIN
    IF d0.size() # d1.size() THEN RETURN FALSE END;
    FOR i := 0 TO d0.size()-1 DO
      IF NOT Elem.Equal(d0.get(i),d1.get(i)) THEN
        RETURN FALSE
      END
    END;
    RETURN TRUE
  END EqSeq;

PROCEDURE FormatSeq(seq : ElemSeq.T) : TEXT =
  VAR
    res := "";
  BEGIN
    FOR i := 0 TO seq.size()-1 DO
      IF i # 0 THEN res := res & "." END;
      res := res & Elem.Format(seq.get(i))
    END;
    RETURN res
  END FormatSeq;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN
    RETURN EqSeq(a.dotted,b.dotted) AND RdlProperty.Equal(a.property,b.property)
  END Equal;

PROCEDURE Hash(a : T) : Word.T =
  BEGIN
    RETURN Word.Plus(HashSeq(a.dotted),RdlProperty.Hash(a.property))
  END Hash;

PROCEDURE Format(a : T) : TEXT =
  BEGIN
    RETURN FormatSeq(a.dotted) & "->" & RdlProperty.Format(a.property)
  END Format;
  
BEGIN END RdlInstanceRef.
