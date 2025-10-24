(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE VaryBits;
IMPORT BigInt;
IMPORT BigIntRep; 
FROM BigInt IMPORT Sign, GetMsb;
FROM BigIntRep IMPORT GetTheBits;
IMPORT FiniteInterval;
IMPORT Debug;
IMPORT Word;
IMPORT CardSet;
IMPORT CardSetDef;
IMPORT CardArraySort;
FROM Fmt IMPORT F, Int;

TYPE Interval = FiniteInterval.T;

VAR Zero := BigInt.New(0);

(*
PROCEDURE FromInterval(fi : FiniteInterval.T) : T =
  BEGIN
    IF BigInt.Sign(fi.lo) # BigInt.Sign(fi.hi) THEN
      RETURN Union(FromIntervalNeg(Interval { fi.lo, Zero }),
                   FromIntervalPos(Interval { Zero, fi.hi }))
    ELSIF BigInt.Sign(fi.lo) = -1 THEN
      RETURN FromIntervalNeg(Interval { fi.lo, fi.hi })
    ELSE
      RETURN FromIntervalPos(Interval { fi.lo, fi.hi })
    END
  END FromInterval;
*)

PROCEDURE IntBits(big : BigInt.T) : T =
  VAR
    repBits  := MAX(GetMsb(big) + 1, 1); (* even for 0 and -1 we need a word *)
    repWords := (repBits - 1) DIV BITSIZE(Word.T) + 1;
    w        := NEW(REF ARRAY OF Word.T, repWords);
    res      : T;

  BEGIN
    CASE Sign(big) OF
      -1 => res.sign := Bit.One
    |
      0..+1 => res.sign := Bit.Zero 
    END;

    FOR b := FIRST(res.x) TO LAST(res.x) DO
      res.x[b] := NEW(CardSetDef.T).init()
    END;
    
    GetTheBits(big, w^);
    FOR i := FIRST(w^) TO LAST(w^) DO
      VAR x := w[i]; BEGIN
        FOR j := 0 TO Word.Size - 1 DO
          IF Word.And(x, 1) = 1 THEN
            EVAL res.x[Bit.One].insert(Word.Size * i + j)
          ELSE
            EVAL res.x[Bit.Zero].insert(Word.Size * i + j)
          END;
          x := Word.Shift(x, -1)
        END
      END
    END;
    RETURN CleanMsbs(res)
  END IntBits;

PROCEDURE SetMax(c : CardSet.T) : [-1 .. LAST(CARDINAL) ] =
  (* really dumb way to do it *)
  VAR
    b : CARDINAL;
    max := -1;
  BEGIN
    WITH iter = c.iterate() DO
      WHILE iter.next(b) DO
        IF b > max THEN max := b END
      END
    END;
    RETURN max
  END SetMax;

PROCEDURE MaxDefinedBit(t : T) : [ -1 .. LAST(CARDINAL) ] =
  VAR
    max := -1;
  BEGIN
    FOR i := FIRST(Bit) TO LAST(Bit) DO
      max := MAX(max, SetMax(t.x[i]))
    END;
    RETURN max
  END MaxDefinedBit;

PROCEDURE FromInterval(fi : FiniteInterval.T) : T =
  BEGIN
    WITH loT   = IntBits(fi.lo),
         hiT   = IntBits(fi.hi),
         union = Union(loT, hiT) DO
      (* find the largest varying bit and set all the lower-order bits to
         vary *)
      WITH msb = SetMax(union.x[Bit.Vary]) DO
        FOR i := 0 TO msb - 1 DO
          EVAL union.x[Bit.Vary].insert(i);
          FOR b := Bit.Zero TO Bit.One DO
            EVAL union.x[b].delete(i)
          END
        END
      END;
      RETURN union
    END
  END FromInterval;

PROCEDURE ToInterval(t : T) : FiniteInterval.T =
  BEGIN
  END ToInterval;

PROCEDURE MaxVarying(t : T) : CARDINAL =
  BEGIN
  END MaxVarying;


PROCEDURE Union(a, b : T) : T =
  VAR
    c : T;
  BEGIN
    IF a.sign = b.sign THEN c.sign := a.sign ELSE c.sign := Bit.Vary END;

    (* bits already varying are still varying *)
    c.x[Bit.Vary] := a.x[Bit.Vary].union(b.x[Bit.Vary]);

    (* bits that are different are now varying *)
    WITH diffBits0 = a.x[Bit.Zero].intersection(b.x[Bit.One]),
         diffBits1 = a.x[Bit.One].intersection(b.x[Bit.Zero]),

         diffBits  = diffBits0.union(diffBits1) DO
      c.x[Bit.Vary] := c.x[Bit.Vary].union(diffBits)
    END;

    (* bits that exist in one but not in the other and are different 
       from the sign bit are varying *)

    VAR
      amax := MaxDefinedBit(a);
      bmax := MaxDefinedBit(b);
    PROCEDURE DoLeading(sml, big : T) =
      BEGIN
        FOR i := MaxDefinedBit(sml) + 1 TO MaxDefinedBit(big) DO
          IF big.x[Flip[a.sign]].member(i) THEN
            EVAL c.x[Bit.Vary].insert(i)
          END
        END
      END DoLeading;
    BEGIN
      IF amax > bmax THEN
        DoLeading(b, a);
      ELSIF bmax > amax THEN
        DoLeading(a, b)
      END
    END;
      
    FOR i := Bit.Zero TO Bit.One DO
      c.x[i] := a.x[i].union(b.x[i]).diff(c.x[Bit.Vary])
    END;

    RETURN CleanMsbs(c)
  END Union;

PROCEDURE AllDefinedBits(t : T) : CardSet.T =
  VAR
    res := NEW(CardSetDef.T).init();
  BEGIN
    FOR b := FIRST(Bit) TO LAST(Bit) DO
      res := res.unionD(t.x[b])
    END;
    RETURN res
  END AllDefinedBits;

PROCEDURE CleanMsbs(t : T) : T =
  (* delete leading bits that match the sign bit *)
  VAR
    diffSet := NEW(CardSetDef.T).init();
  BEGIN
    FOR b := FIRST(Bit) TO LAST(Bit) DO
      IF b # t.sign THEN
        diffSet := diffSet.union(t.x[b])
      END
    END;
    
    FOR i := SetMax(diffSet) + 1 TO SetMax(t.x[t.sign]) DO
      EVAL t.x[t.sign].delete(i)
    END;

    RETURN t

  END CleanMsbs;

PROCEDURE FormatSet(set : CardSet.T) : TEXT =
  VAR
    arr := NEW(REF ARRAY OF CARDINAL, set.size());
    c : CARDINAL;
    i := 0;
    res := "{ ";
    iter := set.iterate();
  BEGIN
    WHILE iter.next(c) DO
      arr[i] := c;
      INC(i)
    END;
    CardArraySort.Sort(arr^);
    FOR i := FIRST(arr^) TO LAST(arr^) DO
      res := res & Int(arr[i]) & " "
    END;
    RETURN res & "}"
  END FormatSet;
  
PROCEDURE Format(t : T) : TEXT =
  VAR
    res := F("[ VaryBits sign=%s ", BitName[t.sign]);
    
  BEGIN
    FOR i := FIRST(Bit) TO LAST(Bit) DO
      res := res & F("%s=%s ", BitName[i], FormatSet(t.x[i]));
    END;

    res := res & "]";
    RETURN res
  END Format;

PROCEDURE Min(t : T) : T =
  VAR
    res : T;
  BEGIN
    RETURN ForceX(t, Bit.Zero)
  END Min;

PROCEDURE Max(t : T) : T =
  VAR
    res : T;
  BEGIN
    RETURN ForceX(t, Bit.One)
  END Max;

PROCEDURE ForceX(t : T; to : Bit) : T =
  VAR
    res : T;
  BEGIN
    <*ASSERT to # Bit.Vary*>
    res.sign := t.sign;
    IF res.sign = Bit.Vary THEN res.sign := Flip[to] END;


    res.x[to] := t.x[to].union(t.x[Bit.Vary]);
    res.x[Bit.Vary] := NEW(CardSetDef.T).init(); (* empty *)
    res.x[Flip[to]] := t.x[Flip[to]];
    RETURN CleanMsbs(res)
  END ForceX;
  
BEGIN END VaryBits.
