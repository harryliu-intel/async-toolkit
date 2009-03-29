(* $Id$ *)

MODULE ValueTranslator;
IMPORT SchemePair, SchemeObject;
IMPORT Value;
FROM Value IMPORT Ordinal, Float, LongFloat, Extended, Array, Set, Record,
                  Txt, Null;
IMPORT SchemeString, SchemeBoolean, SchemeLongReal, SchemeSymbol;

(* things here can't loop, since we're representing M3 constants! *)

PROCEDURE Translate(value : Value.T) : SchemeObject.T =
  BEGIN
    TYPECASE value OF
      NULL => RETURN NIL
    | Ordinal(o)    => RETURN P("Ordinal",   LRI(o.ord))
    | Float(f)      => RETURN P("Float",     LR(FLOAT(f.val,LONGREAL)))
    | LongFloat(lf) => RETURN P("LongFloat", LR(lf.val))
    | Extended(x)   => RETURN P("Extended",  LR(FLOAT(x.val,LONGREAL)))
    | Array(r)      => RETURN P("Array",     ConvertArray(r.elements^))
    | Set(s)        => RETURN P("Set",       ConvertSetArray(s.elements^))
    | Record(r)     => RETURN P("Record",    ConvertArray(r.elements^))
    | Txt(t)        => RETURN P("Txt",       SchemeString.FromText(t.val))
    | Null          => RETURN P("Null",      NIL)
    ELSE
      <*ASSERT FALSE *>
    END
  END Translate;

PROCEDURE ConvertArray(READONLY a : ARRAY OF Value.T) : SchemeObject.T =
  VAR res : SchemePair.T := NIL;
  BEGIN
    FOR i := LAST(a) TO FIRST(a) BY -1 DO
      res := NEW(SchemePair.T, 
                 first := Translate(a[i]),
                 rest := res)
    END;
    RETURN res
  END ConvertArray;

PROCEDURE ConvertSetArray(READONLY a : ARRAY OF BOOLEAN) : SchemeObject.T =
  VAR res : SchemePair.T := NIL;  BEGIN
    FOR i := LAST(a) TO FIRST(a) BY -1 DO
      res := NEW(SchemePair.T, 
                 first := SchemeBoolean.Truth(a[i]),
                 rest := res)
    END;
    RETURN res
  END ConvertSetArray;

PROCEDURE LR(f : LONGREAL) : SchemeObject.T = 
  BEGIN RETURN SchemeLongReal.FromLR(f) END LR;

PROCEDURE LRI(i : INTEGER) : SchemeObject.T = 
  BEGIN RETURN SchemeLongReal.FromI(i) END LRI;

PROCEDURE P(tag : TEXT; what : SchemeObject.T) : SchemePair.T =
  BEGIN
    RETURN NEW(SchemePair.T, 
               first := SchemeSymbol.FromText(tag),
               rest := what)
  END P;

BEGIN END ValueTranslator.
