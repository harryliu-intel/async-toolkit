INTERFACE Prec;
CONST
  Brand = "Prec";
TYPE
  Kind = {Left, Right, None};
  T = REF RECORD
    kind: Kind;
    val: INTEGER := 0;
    used: BOOLEAN := FALSE;
  END;

PROCEDURE Format(a: T): TEXT;
END Prec.
