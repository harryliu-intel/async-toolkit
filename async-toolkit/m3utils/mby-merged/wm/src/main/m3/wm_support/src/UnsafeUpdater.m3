UNSAFE MODULE UnsafeUpdater;
IMPORT Debug;
IMPORT Fmt;
IMPORT Word;
IMPORT CompPath;
IMPORT UpdateTracing;
IMPORT IO;

REVEAL
  T = Public BRANDED Brand OBJECT
    base : REFANY;
    off  : ADDRESS;
    wid  : CARDINAL;
    nm   : CompPath.T;
  OVERRIDES
    init   := Init;
    update := Update;
  END;

PROCEDURE Init(t : T; base : REFANY; fieldAddr : ADDRESS; width : CARDINAL; nm : CompPath.T) : T =
  BEGIN
    t.base := base;
    t.nm := nm;
    t.off :=
        LOOPHOLE(LOOPHOLE(fieldAddr,INTEGER) - LOOPHOLE(base,INTEGER),ADDRESS);
    IF doDebug THEN
      Debug.Out(Fmt.F("UnsafeUpdater init @ 16_%s [ dec %s ] w %s",
                      Fmt.Int(LOOPHOLE(t.off,INTEGER),base := 16),
                      Fmt.Int(LOOPHOLE(t.off,INTEGER),base := 10),
                      Fmt.Int(width)))
    END;
    t.wid := width;
    RETURN t
  END Init;

PROCEDURE Value(t : T) : Word.T =
  VAR
    pin := t.base;
    ptr := LOOPHOLE(LOOPHOLE(t.base,INTEGER) + LOOPHOLE(t.off,INTEGER),ADDRESS);
    res : Word.T;
  BEGIN
    CASE t.wid OF
      1..8 =>
      res := LOOPHOLE(ptr, UNTRACED REF [0..16_ff])^ 
    |
      9..16 =>
      res := LOOPHOLE(ptr, UNTRACED REF [0..16_ffff])^
    |
      17..32 =>
      res := LOOPHOLE(ptr, UNTRACED REF [0..16_ffffffff])^ 
    |
      33..64 =>
      res := LOOPHOLE(ptr, UNTRACED REF Word.T)^ 
    ELSE
      <*ASSERT FALSE*>
    END;

    <*ASSERT pin # NIL *>
    RETURN res
  END Value;
  
PROCEDURE Update(t : T; to : Word.T) =
  VAR
    pin := t.base;
    ptr := LOOPHOLE(LOOPHOLE(t.base,INTEGER) + LOOPHOLE(t.off,INTEGER),ADDRESS);
  BEGIN

    IF UpdateTracing.Enabled THEN
      IO.Put(CompPath.ToText(t.nm) & " <- 16_" & Fmt.Unsigned(to) & "\n")
    END;
      
    CASE t.wid OF
      1..8 =>
      LOOPHOLE(ptr, UNTRACED REF [0..16_ff])^ := to
    |
      9..16 =>
      LOOPHOLE(ptr, UNTRACED REF [0..16_ffff])^ := to
    |
      17..32 =>
      LOOPHOLE(ptr, UNTRACED REF [0..16_ffffffff])^ := to
    |
      33..64 =>
      LOOPHOLE(ptr, UNTRACED REF Word.T)^ := to;
    ELSE
      <*ASSERT FALSE*>
    END;

    <*ASSERT pin # NIL *>
  END Update;

BEGIN
  doDebug := FALSE;
END UnsafeUpdater.
  
