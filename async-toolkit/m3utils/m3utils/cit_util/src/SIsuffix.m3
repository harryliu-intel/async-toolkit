(* $Id$ *)

MODULE SIsuffix;
IMPORT Text;
IMPORT Scan;
IMPORT SuffixTbl;
IMPORT Lex, FloatMode;

PROCEDURE Int(text : TEXT; mode : Mode) : INTEGER RAISES { OutOfRange, UnknownSuffix, FloatMode.Trap, Lex.Error } =
  CONST
    firstInt = FLOAT(FIRST(INTEGER), LONGREAL);
    lastInt =  FLOAT(LAST(INTEGER),  LONGREAL);
  VAR
    longReal := LongReal(text,mode);
  BEGIN
    IF longReal < firstInt OR longReal > lastInt THEN RAISE OutOfRange END;
    RETURN ROUND(longReal)
  END Int;

PROCEDURE Real(text : TEXT; mode : Mode) : REAL RAISES { UnknownSuffix, FloatMode.Trap, Lex.Error } = 
  VAR
    longReal := LongReal(text,mode);
  BEGIN
    RETURN FLOAT(longReal, REAL);
  END Real;

PROCEDURE LongReal(text : TEXT; mode : Mode) : LONGREAL RAISES { UnknownSuffix, FloatMode.Trap, Lex.Error } =
  VAR
    len := Text.Length(text);
    suf := Text.GetChar(text, len - 1);
    pre := Text.Sub(text, 0, len - 1);
    val : LONGREAL;
    mult : T;
  BEGIN

    (* first check for no suffix *)
    IF suf = '.' OR (suf >= '0' AND suf <= '9') THEN
      RETURN Scan.LongReal(text)
    END;

    (* has a suffix.  Scan prefix as a number *)
    val := Scan.LongReal(pre);
    IF NOT tbl.get(suf,mult) THEN
      RAISE UnknownSuffix
    ELSE
      IF mode = Mode.Base10 THEN
        RETURN mult.size * val
      ELSIF mode = Mode.Base2 AND mult.geeky > 0.0d0 THEN
        RETURN mult.geeky * val
      ELSE
        RAISE UnknownSuffix
      END
    END
  END LongReal;

VAR tbl := NEW(SuffixTbl.Default).init();

BEGIN 
  FOR i := FIRST(List) TO LAST(List) DO
    VAR
      x : BOOLEAN;
    BEGIN
      x := tbl.put(List[i].char, List[i]);
      <* ASSERT NOT x *>
    END
  END
END SIsuffix.
