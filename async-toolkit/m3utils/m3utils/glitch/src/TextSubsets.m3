MODULE TextSubsets;
IMPORT Word;
IMPORT TextSetDef AS ElemSetDef;
IMPORT Debug;

REVEAL
  Iterator = PubIterator BRANDED Brand & " Iterator" OBJECT
    a : REF ARRAY OF Elem;
    q : Word.T;
  OVERRIDES
    next := Next;
  END;

PROCEDURE Iterate(s : Set) : Iterator =
  VAR
    walker := s.iterate();
    e : Elem;
    a := NEW(REF ARRAY OF Elem, BITSIZE(Word.T));
    i : CARDINAL := 0;
  BEGIN
    WHILE walker.next(e) DO
      a[i] := e;
      INC(i);
      IF i = BITSIZE(Word.T) THEN
        Debug.Error("too many objects in TextSubset")
      END
    END;
    WITH aa = NEW(REF ARRAY OF Elem, i) DO
      aa^ := SUBARRAY(a^, 0, i);
      RETURN NEW(Iterator, a := aa, q := 0)
    END
  END Iterate;

PROCEDURE Next(iter : Iterator; VAR ss : Set) : BOOLEAN =
  BEGIN
    IF iter.q = Word.Minus(Word.Shift(1, NUMBER(iter.a^)), 1) THEN
      RETURN FALSE
    ELSE
      VAR
        res := NEW(ElemSetDef.T).init();
      BEGIN
        FOR i := 0 TO NUMBER(iter.a^) - 1 DO
          IF Word.Extract(iter.q, i, 1) = 1 THEN
            EVAL res.insert(iter.a[i])
          END
        END;
        ss := res;
        iter.q := Word.Plus(iter.q, 1);
        RETURN TRUE
      END
    END
  END Next;
  
BEGIN END TextSubsets.
