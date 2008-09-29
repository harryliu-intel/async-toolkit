(* $Id$ *)

MODULE SchemePair;
IMPORT Wx;
IMPORT SchemeObject, SchemeUtils, SchemeSymbol;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    init          :=   Init;
    equals        :=   Equals;
    format        :=   Format;
    stringifyPair := StringifyPair;
  END;

PROCEDURE Init(t : T; first, rest : SchemeObject.T) : T =
  BEGIN t.first := first; t.rest := rest; RETURN t END Init;

PROCEDURE Equals(t : T; x : SchemeObject.T) : BOOLEAN =
  BEGIN
    IF    t = x THEN
      RETURN TRUE
    ELSIF NOT ISTYPE(x,T) OR x = NIL THEN
      RETURN FALSE
    ELSE
      WITH that = NARROW(x,T) DO
        RETURN  SchemeUtils.Equal(t.first, that.first) AND
                SchemeUtils.Equal(t.rest, that.rest) 
      END       
    END
  END Equals;

PROCEDURE Format(t : T) : TEXT =
  BEGIN RETURN SchemeUtils.StringifyQ(t, TRUE) END Format;

PROCEDURE StringifyPair(t : T; quoted : BOOLEAN; buf : Wx.T) =

  CONST SymEq      = SchemeSymbol.SymEq;
        StringifyB = SchemeUtils.StringifyB;
        Rest       = SchemeUtils.Rest;
        Second     = SchemeUtils.Second;

  VAR special : TEXT := NIL;

  BEGIN
    IF t.rest # NIL AND ISTYPE(t.rest,T) AND Rest(t.rest) = NIL THEN

      IF    SymEq(t.first, "quote") THEN             special := "'"
      ELSIF SymEq(t.first, "quasiquote") THEN        special := "`"
      ELSIF SymEq(t.first, "unquote") THEN           special := ","
      ELSIF SymEq(t.first, "unquote-splicing") THEN  special := ",@"
      END

    END;

    IF special # NIL THEN
      Wx.PutText(buf, special);
      StringifyB(Second(t), quoted, buf)
    ELSE
      Wx.PutChar(buf, '(');
      StringifyB(t.first, quoted, buf);
      VAR tail := t.rest; 
      BEGIN
        WHILE tail # NIL AND ISTYPE(tail,T) DO
          Wx.PutChar(buf, ' ');
          StringifyB(NARROW(tail,T).first, quoted, buf);
          tail := NARROW(tail,T).rest
        END;
        IF tail # NIL THEN
          Wx.PutText(buf, " . ");
          StringifyB(tail, quoted, buf)
        END;
        Wx.PutChar(buf, ')')
      END
    END
  END StringifyPair;

BEGIN END SchemePair.
