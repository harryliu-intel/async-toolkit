(* $Id$ *)

MODULE ValueTranslator;

(* 
 * Copyright (c) 2009, Generation Capital Ltd.  All rights reserved.
 * Author : Mika Nystrom <mika@alum.mit.edu> 
 *)

IMPORT SchemePair, SchemeObject;
IMPORT Value;
FROM Value IMPORT Ordinal, Float, LongFloat, Extended, ArrayOrRecord, Set,
                  Txt, Null, Propagate, Proc;
IMPORT SchemeString, SchemeLongReal, SchemeSymbol;
IMPORT TypeTranslator;
IMPORT Type;

(* things here can't loop, since we're representing M3 constants! *)

PROCEDURE Translate(value : Value.T) : SchemeObject.T =
  BEGIN
    TYPECASE value OF
      NULL =>RETURN NIL
    | Ordinal(o)    => RETURN P("Ordinal",   LRI(o.ord))
    | Float(f)      => RETURN P("Float",     LR(FLOAT(f.val,LONGREAL)))
    | LongFloat(lf) => RETURN P("LongFloat", LR(lf.val))
    | Extended(x)   => RETURN P("Extended",  LR(FLOAT(x.val,LONGREAL)))
    | ArrayOrRecord(r)=> 
      RETURN P("ArrayOrRecord",     ConvertArray(r.elements^))
    | Set(s)        => RETURN P("Set",       ConvertSetArray(s.elements^))
    | Txt(t)        => RETURN P("Txt",       SchemeString.FromText(t.val))
    | Null          => RETURN P("Null",      NIL)
    | Propagate     => RETURN P("Propagate", NIL)
    | Proc(p)       => RETURN P("Proc",      ProcedureName(p))
    ELSE
      <*ASSERT FALSE *>
    END
  END Translate;

PROCEDURE ProcedureName(p : Proc) : SchemeObject.T =
  BEGIN
    RETURN TypeTranslator.TranslateQid(
               NEW(Type.Qid, 
                   intf := p.intf,
                   item := p.item))
  END ProcedureName;

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

PROCEDURE ConvertSetArray(READONLY a: ARRAY OF Value.Ordinal):SchemeObject.T =
  VAR res : SchemePair.T := NIL;  BEGIN
    FOR i := LAST(a) TO FIRST(a) BY -1 DO
      res := NEW(SchemePair.T, 
                 first := Translate(a[i]),
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
