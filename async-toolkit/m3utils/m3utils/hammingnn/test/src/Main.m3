MODULE Main;
IMPORT Random;
IMPORT Word;
IMPORT Hnn;
IMPORT Debug;
FROM Fmt IMPORT F, Int;

CONST
  Len       = 1000;        (* length in bits of words *)
  ErrorRate = 0.1d0;       (* rate of errors in bits *)
  Members   = 1000 ;       (* how big a set to use as universe *)
  Nn        = 10;          (* how many nearest neighbors to seek *)
  Iters     = 20;          (* how many tests to run *)
  
TYPE
  Vec = ARRAY [ 0 .. Len - 1 ] OF BOOLEAN;
  
VAR
  theSet := NEW(Hnn.T).init(Len);
  a : Vec;
  rand := NEW(Random.Default).init();

PROCEDURE RandomVec(VAR v : Vec) =
  CONST
    Step = 16;
  BEGIN
    FOR i := 0 TO Len - 1 BY Step DO
      WITH s = rand.integer(0, Word.Shift(1, Step)) DO
        FOR j := 0 TO Step - 1 DO
          WITH idx = i + j DO
            IF i + j = Len THEN EXIT END;
            v[idx] := Word.Extract(s, j, 1) = 1
          END
        END
      END
    END
  END RandomVec;

PROCEDURE InitSet(set : Hnn.T) =
  BEGIN
    Debug.Out(F("Initializing set, %s members", Int(Members)));
    
    FOR i := 0 TO Members - 1 DO
      RandomVec(a);
      EVAL set.put(a)
    END
  END InitSet;

PROCEDURE RunTests(set : Hnn.T) =
  VAR
    sz       := set.size();
    len      := set.getLen();
    q, r, s  := NEW(REF ARRAY OF BOOLEAN, len);
    errs, k  : CARDINAL;
  BEGIN

    FOR i := 0 TO Iters - 1 DO
      (* pick entry at random *)
      WITH a = rand.integer(0, sz - 1) DO
        set.get(a, q^)
      END;

      (* corrupt the bits *)
      errs := 0;
      FOR i := FIRST(q^) TO LAST(q^) DO
        VAR
          x := rand.longreal(0.0d0, 1.0d0);
        BEGIN
          IF x < ErrorRate THEN
            r[i] := NOT q[i];
            INC(errs)
          ELSE
            r[i] := q[i]
          END
        END
      END;

      (* search for corrupted entry *)
      WITH iter = set.iterNnOrdered(r^, Nn) DO
        Debug.Out(F("Searching for q of weight %s, using r weight %s dist %s errs %s",
                    Int(Weight(q^)),
                    Int(Weight(r^)),
                    Int(Dist(q^,r^)),
                    Int(errs)));
        k := 0;
        WHILE iter.next(s^) DO
          WITH d = Dist(q^, s^) DO
            IF k = 0 THEN
              IF d = 0 THEN
                Debug.Out("search successful!")
              ELSE
                Debug.Out("SEARCH FAILURE!")
              END
            END;
            Debug.Out(F("Found s[%s] dist %s", Int(k), Int(d)));
            INC(k)
          END
        END
      END
    END
  END RunTests;

PROCEDURE Weight(READONLY a : ARRAY OF BOOLEAN) : CARDINAL =
  VAR
    w := 0;
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      IF a[i] THEN INC(w) END
    END;
    RETURN w
  END Weight;
  
PROCEDURE Dist(READONLY a, b : ARRAY OF BOOLEAN) : CARDINAL =
  (* Hamming distance *)
  VAR
    d := 0;
  BEGIN
    <*ASSERT NUMBER(a) = NUMBER(b)*>
    FOR i := FIRST(a) TO LAST(a) DO
      IF a[i] # b[i] THEN INC(d) END
    END;
    RETURN d
  END Dist;
  
BEGIN
  InitSet(theSet);

  RunTests(theSet)
END Main.
