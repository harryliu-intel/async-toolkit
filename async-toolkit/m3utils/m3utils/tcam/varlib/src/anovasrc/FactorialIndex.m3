(* $Id: FactorialIndex.m3,v 1.2 2005/04/05 09:09:52 mika Exp $ *)
MODULE FactorialIndex;
IMPORT Word, Integer;

REVEAL
  T = Public BRANDED Brand OBJECT
    data : REF ARRAY OF CARDINAL;
    hashV : Word.T;
  OVERRIDES
    init := Init;
    indices := Indices;
    index := Index;
  END;

PROCEDURE Indices(t : T) : REF ARRAY OF CARDINAL =
  VAR
    res := NEW(REF ARRAY OF CARDINAL, NUMBER(t.data^));
  BEGIN
    res^ := t.data^;
    RETURN res
  END Indices;

PROCEDURE Init(t : T; READONLY vi : ARRAY OF CARDINAL) : T =
  VAR
    hashV : Word.T := 0;
  BEGIN
    FOR i := FIRST(vi) TO LAST(vi) DO
      hashV := Word.Plus(hashV, Integer.Hash(vi[i]))
    END;
    t.hashV := hashV;
    
    t.data := NEW(REF ARRAY OF CARDINAL, NUMBER(vi));
    t.data^ := vi;
    RETURN t
  END Init;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN a.hashV END Hash;

PROCEDURE Equal(a, b : T) : BOOLEAN = 
  BEGIN RETURN a.hashV = b.hashV AND a.data^ = b.data^ END Equal;

PROCEDURE Index(t : T; idx : CARDINAL) : CARDINAL = 
  BEGIN RETURN t.data[idx] END Index;

BEGIN END FactorialIndex.
