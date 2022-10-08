MODULE HnnHrep;
IMPORT Word;

PROCEDURE SetHash(t : T) =
  VAR
    h : Word.T := 0;
  BEGIN
    FOR i := FIRST(t.bits^) TO LAST(t.bits^) DO
      h := Word.Plus(h, t.bits[i])
    END;
    t.hashV := h;
    t.hashValid := TRUE
  END SetHash;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN
    IF a.hashValid AND b.hashValid AND a.hashV # b.hashV THEN
      RETURN FALSE
    ELSIF NUMBER(a.bits^) # NUMBER(b.bits^) OR a.sz # b.sz THEN
      RETURN FALSE
    ELSE
      RETURN a.bits^ = b.bits^
    END
  END Equal;

PROCEDURE Hash(a : T) : Word.T =
  BEGIN
    IF NOT a.hashValid THEN
      SetHash(a)
    END;
    RETURN a.hashV
  END Hash;

PROCEDURE Length(t : T) : CARDINAL = BEGIN RETURN t.sz END Length;

PROCEDURE New(READONLY a : ARRAY OF BOOLEAN) : T =
  VAR
    sz   := NUMBER(a);
    bits := NEW(REF ARRAY OF Word.T, (sz - 1) DIV Word.Size + 1);
    res  := NEW(T, sz := sz, bits := bits, hashValid := FALSE);
    w : Word.T;
  BEGIN
    FOR i := FIRST(bits^) TO LAST(bits^) - 1 DO
      w := 0;
      FOR j := 0 TO Word.Size - 1 DO
        WITH idx = i * Word.Size + j DO
          w := Word.Insert(w,
                           ARRAY BOOLEAN OF [0..1] { 0, 1 }[a[idx]],
                           j,
                           1)
        END
      END;
      bits[i] := w;
    END;
    
    w := 0;
    WITH i = LAST(bits^) DO
      FOR j := 0 TO MIN(Word.Size, sz MOD Word.Size) - 1 DO
        WITH idx = i * Word.Size + j DO
          w := Word.Insert(w,
                           ARRAY BOOLEAN OF [0..1] { 0, 1 }[a[idx]],
                           j,
                           1)
        END
      END;
      bits[i] := w
    END
  END New;

PROCEDURE ToArray(t : T; VAR a : ARRAY OF BOOLEAN) =
  BEGIN
    <*ASSERT NUMBER(a) = t.sz*>
    FOR i := FIRST(a) TO LAST(a) DO
      a[i] := Word.Extract(t.bits[i DIV t.sz], i MOD t.sz, 1) = 1
    END
  END ToArray;
  
BEGIN END HnnHrep.
