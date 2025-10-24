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
    END;

    RETURN res
  END New;

PROCEDURE ToArray(t : T; VAR a : ARRAY OF BOOLEAN) =
  BEGIN
    <*ASSERT NUMBER(a) = t.sz*>
    FOR i := FIRST(a) TO LAST(a) DO
      a[i] := Word.Extract(t.bits[i DIV Word.Size], i MOD Word.Size, 1) = 1
    END
  END ToArray;

PROCEDURE GetBits(t : T; from, n : CARDINAL) : Word.T =
  BEGIN
    <*ASSERT n <= Word.Size*>
    WITH sw = from DIV Word.Size,
         sb = from MOD Word.Size,
         eb = from + n,
         ew = eb   DIV Word.Size,
         eb = eb   MOD Word.Size DO
      IF sw = ew THEN
        RETURN Word.Extract(t.bits[sw], sb, n)
      ELSE
        <*ASSERT ew = sw + 1*>
        WITH lowidth = Word.Size - sb,
             lopart  = Word.Extract(t.bits[sw], sb, lowidth),
             hipart  = Word.Extract(t.bits[ew],  0, eb) DO
          RETURN Word.Or(lopart, Word.Shift(hipart, lowidth)) 
        END
      END
    END
  END GetBits;

PROCEDURE Distance(a, b : T) : CARDINAL =
  VAR
    res : CARDINAL := 0;
  BEGIN
    <*ASSERT a.sz = b.sz*>
    FOR i := FIRST(a.bits^) TO LAST(a.bits^) DO
      INC(res, PopCount(Word.Xor(a.bits[i],b.bits[i])))
    END;
    RETURN res
  END Distance;

PROCEDURE DistanceLessEqual(a, b : T; maxdist : CARDINAL) : CARDINAL =
  VAR
    res : CARDINAL := 0;
  BEGIN
    <*ASSERT a.sz = b.sz*>
    FOR i := FIRST(a.bits^) TO LAST(a.bits^) DO
      INC(res, PopCount(Word.Xor(a.bits[i],b.bits[i])));
      IF res > maxdist THEN RETURN LAST(CARDINAL) END
    END;
    RETURN res
  END DistanceLessEqual;

PROCEDURE PopCount(w : Word.T) : CARDINAL =
  BEGIN
    (*
    w -= (w >> 1) & 0x5555555555555555ULL;
    w = (w & 0x3333333333333333ULL) + ((w >> 2) & 0x3333333333333333ULL);
    w = (w + (w >> 4)) & 0x0f0f0f0f0f0f0f0fULL;
    return int((w * 0x0101010101010101ULL) >> 56);
    *)
    
    w := Word.Minus(w,
                    Word.And(Word.RightShift(w, 1),
                             16_5555555555555555));
    w := Word.Plus(Word.And(w, 16_3333333333333333),
                   Word.And(Word.RightShift(w, 2),
                            16_3333333333333333));

    w := Word.And(Word.Plus(w, Word.RightShift(w, 4)),
                            16_0f0f0f0f0f0f0f0f);
    RETURN Word.RightShift(Word.Times(w, 16_0101010101010101), 56)

  END PopCount;

BEGIN END HnnHrep.
