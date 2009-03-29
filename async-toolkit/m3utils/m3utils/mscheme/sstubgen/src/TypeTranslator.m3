(* $Id$ *)

MODULE TypeTranslator;
IMPORT Type, SchemeObject;
IMPORT SchemeLongReal, Atom, Value, SchemePair, SchemeSymbol;
IMPORT SchemeBoolean;
FROM Type IMPORT Qid, Field, Method, Signature, Formal, Exception, Mode;
FROM Type IMPORT MethodDefault, MethodDefault1, MethodDefault2;
IMPORT SchemeUtils;
IMPORT ValueTranslator;

PROCEDURE F(name : TEXT;
            what : SchemeObject.T) : SchemeObject.T =
  BEGIN 
    RETURN NEW(SchemePair.T,
               first := SchemeSymbol.FromText(name),
               rest := what) 
  END F;

PROCEDURE B(b : BOOLEAN) : SchemeObject.T =
  BEGIN RETURN SchemeBoolean.Truth(b) END B;

PROCEDURE LRI(i : INTEGER) : SchemeLongReal.T =
  BEGIN RETURN SchemeLongReal.FromI(i) END LRI;

TYPE 
  Rec = REF RECORD
    t : Type.T;
    u : SchemeObject.T;
    n : Rec := NIL;
  END;

VAR recs : Rec := NIL;

PROCEDURE GotIt(t : Type.T; VAR o : SchemeObject.T) : BOOLEAN =
  VAR p := recs;
  BEGIN
    WHILE p # NIL DO
      IF p.t = t THEN o := p.u; RETURN TRUE END;
      p := p.n
    END;
    RETURN FALSE
  END GotIt;

PROCEDURE Insert(t : Type.T; o : SchemeObject.T) =
  BEGIN
    recs := NEW(Rec, t := t, u := o, n := recs)
  END Insert;
    
PROCEDURE Translate(t : Type.T) : SchemeObject.T =

  PROCEDURE M(named : TEXT;
              w0, w1, w2, w3, w4, w5, w6 : SchemeObject.T := NIL) : SchemeObject.T =

    PROCEDURE Cons(what : SchemeObject.T) =
      BEGIN
        res := NEW(SchemePair.T,
                   first := what, rest := res)
      END Cons;

    VAR
      res : SchemePair.T := NIL;
    BEGIN
      head.first := SchemeSymbol.FromText(named);

      Cons(F("name",TranslateQid(t.name)));
      Cons(F("visited",B(t.visited)));
      Cons(F("brandsOK",B(t.brandsOK)));
      IF w0 # NIL THEN 
        Cons(w0);
        IF w1 # NIL THEN
          Cons(w1);
          IF w2 # NIL THEN
            Cons(w2);
            IF w3 # NIL THEN
              Cons(w3);
              IF w4 # NIL THEN
                Cons(w4);
                IF w5 # NIL THEN
                  Cons(w5);
                  IF w6 # NIL THEN
                    Cons(w6)
                  END
                END
              END
            END
          END
        END
      END;

      head.rest := SchemeUtils.Reverse(res);

      RETURN head
    END M;

  VAR 
    r : SchemeObject.T;
    head : SchemePair.T;
  BEGIN
    IF GotIt(t, r) THEN RETURN r END;

    head := NEW(SchemePair.T);
    
    Insert(t, head);

    TYPECASE t OF
      NULL => RETURN NIL
    |
      Type.Procedure(p) => RETURN M("Procedure", 
                                    F("sig",TranslateSignature(p.sig)))
    |
      Type.Set(s)       => RETURN M("Set", F("range",Translate(s.range)))
    |
      Type.Record(r)    => RETURN M("Record", F("fields",
                                                TranslateFieldArray(r.fields^)))
    |
      Type.Packed(p)    => RETURN M("Packed", 
                                    F("size",LRI(p.size)), 
                                    F("base",Translate(p.base)))
    |
      Type.Object(o)    => RETURN M("Object",
                                    F("traced",B(o.traced)),
                                    F("branded", B(o.branded)),
                                    F("brand", o.brand),
                                    F("revIntf", o.revIntf),
                                    F("super", Translate(o.super)),
                                    F("fields", TranslateFieldArray(o.fields^)),
                                    F("methods", TranslateMethodArray(o.methods^)))
    |
      Type.Ref(o)       => RETURN M("Ref",
                                    F("traced",B(o.traced)),
                                    F("branded", B(o.branded)),
                                    F("brand", o.brand),
                                    F("revIntf", o.revIntf),
                                    F("target", Translate(o.target)))
    |
      Type.Opaque(o) => RETURN M("Opaque",
                                    F("traced",B(o.traced)),
                                    F("branded", B(o.branded)),
                                    F("brand", o.brand),
                                    F("revIntf", o.revIntf),
                                    F("revealedSuperType", Translate(o.revealedSuperType)))
    | 
      Type.OpenArray(a) => RETURN M("OpenArray",
                                    F("index", Translate(a.index)),
                                    F("element", Translate(a.element)),
                                    F("refArray", Translate(a.refArray)),
                                    F("openDimensions", LRI(a.openDimensions)))
      
    | 
      Type.Array(a) => RETURN M("Array",
                                    F("index", Translate(a.index)),
                                    F("element", Translate(a.element)))
      
    |
      Type.Reference(o) => RETURN M("Reference",
                                    F("traced",B(o.traced)),
                                    F("branded", B(o.branded)),
                                    F("brand", o.brand),
                                    F("revIntf", o.revIntf))
    |
      Type.Extended     => RETURN M("Extended")
    |
      Type.LongReal     => RETURN M("LongReal")
    |
      Type.Real         => RETURN M("Real")
    |
      Type.Subrange(s)  => RETURN M("Subrange",
                                    F("base", Translate(s.base)),
                                    F("min", ValueTranslator.Translate(s.min)),
                                    F("max", ValueTranslator.Translate(s.max)))
    |
      Type.UserDefined(u)=>RETURN M("UserDefined",
                                    F("elts", TranslateAtomArray(u.elts^)))
    |
      Type.WideChar     => RETURN M("WideChar")
    |
      Type.Char         => RETURN M("WideChar")
    |
      Type.Enumeration  => RETURN M("WideChar")
    |
      Type.Ordinal      => RETURN M("WideChar")
    |
      Type.T            => RETURN M("T")
    END
  END Translate;

PROCEDURE TranslateQid(q : Qid) : SchemeObject.T =
  BEGIN
    IF q = NIL THEN RETURN NIL END;
    RETURN SchemeUtils.MakeList(
               ARRAY OF SchemeObject.T {
    SchemeSymbol.FromText("Qid"),
    F("intf", q.intf),
    F("item", q.item) })
  END TranslateQid;

PROCEDURE TranslateFieldArray(READONLY f : ARRAY OF Field) : SchemeObject.T =
  VAR res : SchemePair.T := NIL;
  BEGIN 
    FOR i := LAST(f) TO FIRST(f) BY -1 DO
      res := NEW(SchemePair.T,
                 first := SchemeUtils.MakeList(ARRAY OF SchemeObject.T {
      SchemeSymbol.FromText("Field"),
      F("name",f[i].name),
      F("type",Translate(f[i].type)),
      F("default", ValueTranslator.Translate(f[i].default)) }),
                 rest := res)
    END;
    RETURN res
  END TranslateFieldArray;

PROCEDURE TranslateMethodArray(READONLY f : ARRAY OF Method) : SchemeObject.T =
  VAR res : SchemePair.T := NIL;
  BEGIN 
    FOR i := LAST(f) TO FIRST(f) BY -1 DO
      res := NEW(SchemePair.T,
                 first := SchemeUtils.MakeList(ARRAY OF SchemeObject.T {
      SchemeSymbol.FromText("Method"),
      F("name",f[i].name),
      F("sig",TranslateSignature(f[i].sig)),
      F("default", TranslateMethodDefault(f[i].default)) }),
                 rest := res)
    END;
    RETURN res
  END TranslateMethodArray;

PROCEDURE TranslateMethodDefault(d : MethodDefault) : SchemeObject.T =
  BEGIN 
    TYPECASE d OF
      NULL => RETURN NIL
    |
      MethodDefault1(d1) =>
      RETURN SchemeUtils.MakeList( ARRAY OF SchemeObject.T {
      SchemeSymbol.FromText("MethodDefault1"),
      F("qid", TranslateQid(d1.qid)) })
    |
      MethodDefault2(d2) =>
      RETURN SchemeUtils.MakeList( ARRAY OF SchemeObject.T {
      SchemeSymbol.FromText("MethodDefault2"),
      F("obType", Translate(d2.obType)),
      F("method", d2.method) })
    ELSE
      <*ASSERT FALSE*>
    END
  END TranslateMethodDefault;

PROCEDURE TranslateAtomArray(READONLY f : ARRAY OF Atom.T) : SchemeObject.T =
  VAR res : SchemePair.T := NIL;
  BEGIN
    FOR i := LAST(f) TO FIRST(f) BY -1 DO
      res := NEW(SchemePair.T,
                 first := f[i],
                 rest := res)
    END;
    RETURN res
  END TranslateAtomArray;

PROCEDURE TranslateSignature(sig : Signature) : SchemeObject.T =
  BEGIN 
    RETURN SchemeUtils.MakeList( ARRAY OF SchemeObject.T {
    SchemeSymbol.FromText("Signature"),
    F("formals", TranslateFormalArray(sig.formals^)),
    F("result", Translate(sig.result)),
    F("raises", TranslateExceptionArray(sig.raises (* sic! *)) 
    (* careful here! -- distinguish between RAISES {} and RAISES ANY*)) })
  END TranslateSignature;

PROCEDURE TranslateFormalArray(READONLY f : ARRAY OF Formal) : SchemeObject.T=
  VAR res : SchemePair.T := NIL;
  BEGIN 
    FOR i := LAST(f) TO FIRST(f) BY -1 DO
      res := NEW(SchemePair.T,
                 first := SchemeUtils.MakeList(ARRAY OF SchemeObject.T {
      SchemeSymbol.FromText("Formal"),
      F("mode",TranslateMode(f[i].mode)),
      F("outOnly",B(f[i].outOnly)),
      F("name", f[i].name),
      F("type", Translate(f[i].type)),
      F("default", ValueTranslator.Translate(f[i].default)) }),
      rest := res)
    END;
    RETURN res
  END TranslateFormalArray;

PROCEDURE TranslateExceptionArray(f : REF ARRAY OF Exception) : SchemeObject.T=
  VAR
    res : SchemePair.T := NIL;
  BEGIN
    IF f = NIL THEN RETURN NIL END;
    FOR i := LAST(f^) TO FIRST(f^) BY -1 DO
      res := NEW(SchemePair.T,
                 first := SchemeUtils.MakeList(ARRAY OF SchemeObject.T {
      SchemeSymbol.FromText("Exception"),
      F("qid",TranslateQid(f[i].qid)),
      F("arg",Translate(f[i].arg)) }),
                 rest := res)
    END;
    RETURN SchemeUtils.List1(res)
  END TranslateExceptionArray;

PROCEDURE TranslateMode(m : Mode) : SchemeObject.T =
  BEGIN
    CASE m OF 
      Mode.Value => RETURN SchemeSymbol.FromText("Mode.Value")
    | Mode.Var => RETURN SchemeSymbol.FromText("Mode.Var")
    | Mode.Readonly => RETURN SchemeSymbol.FromText("Mode.Readonly")
    END
  END TranslateMode;

BEGIN END TypeTranslator.

