UNSAFE MODULE UnsafeUpdater;
IMPORT Debug;
IMPORT Fmt;
IMPORT Word;

REVEAL
  T = Public BRANDED Brand OBJECT
    base : REFANY;
    off  : ADDRESS;
    wid  : CARDINAL;
  OVERRIDES
    init   := Init;
    update := Update;
  END;

PROCEDURE Init(t : T; base : REFANY; fieldAddr : ADDRESS; width : CARDINAL) : T =
  BEGIN
    t.base := base;
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
  
PROCEDURE Update(t : T; to : Word.T) =
  VAR
    pin := t.base;
    ptr := LOOPHOLE(LOOPHOLE(t.base,INTEGER) + LOOPHOLE(t.off,INTEGER),ADDRESS);
  BEGIN
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
  
