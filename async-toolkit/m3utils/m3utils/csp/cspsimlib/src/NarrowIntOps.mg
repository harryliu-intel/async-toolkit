GENERIC MODULE NarrowIntOps(Type);
IMPORT Word;
IMPORT Debug;
FROM Fmt IMPORT F, Int, Unsigned;

CONST doDebug = FALSE;
      
PROCEDURE SignExtend(w : Word.T) : INTEGER =
  BEGIN
    IF doDebug THEN
      Debug.Out(Brand & ".SignExtend(16_" & Unsigned(w) & ")")
    END;
    IF Type.Signed THEN
      VAR
        sb := Word.Extract(w, Type.Width - 1, 1);
      BEGIN
        IF doDebug THEN
          Debug.Out("SignExtend(sb = " & Unsigned(sb) & ")")
        END;
        IF sb = 1 THEN
          WITH res = Word.Or(Type.NotMask, w) DO
            IF doDebug THEN
              Debug.Out(F("SignExtend(res = %s [16_%s]",
                          Int(res), Unsigned(res)))
            END;
            RETURN res
          END
        ELSE
          RETURN w
        END
      END
    ELSE
      RETURN w
    END
  END SignExtend;

BEGIN END NarrowIntOps.
