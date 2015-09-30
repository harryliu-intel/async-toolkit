MODULE ArithRep;
IMPORT Arith;
IMPORT SchemeObject;
IMPORT SchemeSymbol, SchemeLongReal, ArithR, ArithP;
IMPORT SchemePair, SchemeUtils;
IMPORT ArithRefTbl;

CONST S      = SchemeSymbol.FromText;
      Number = SchemeLongReal.FromLR;

PROCEDURE L(s : SchemeSymbol.T; 
            a, b, c, d : SchemeObject.T := NIL) : SchemePair.T =
  VAR res : SchemePair.T := NIL;
  BEGIN
    IF d # NIL THEN res := NEW(SchemePair.T, first := d, rest := res) END;
    IF c # NIL THEN res := NEW(SchemePair.T, first := c, rest := res) END;
    IF b # NIL THEN res := NEW(SchemePair.T, first := b, rest := res) END;
    IF a # NIL THEN res := NEW(SchemePair.T, first := a, rest := res) END;
    IF s # NIL THEN res := NEW(SchemePair.T, first := s, rest := res) END;
    RETURN res
  END L;

PROCEDURE LLR(a : REF ARRAY OF ArithR.T) : SchemeObject.T =
  VAR
    res : SchemePair.T := NIL;
  BEGIN
    FOR i := LAST(a^) TO FIRST(a^) BY -1 DO
      res := SchemeUtils.Cons(ToScheme(a[i]),res)
    END;
    RETURN res
  END LLR;

PROCEDURE LLP(a : REF ARRAY OF ArithP.T) : SchemeObject.T =
  VAR
    res : SchemePair.T := NIL;
  BEGIN
    FOR i := LAST(a^) TO FIRST(a^) BY -1 DO
      res := SchemeUtils.Cons(ToScheme(a[i]),res)
    END;
    RETURN res
  END LLP;

PROCEDURE ToScheme(t : Arith.T) : SchemeObject.T =

  PROCEDURE Recurse(t : Arith.T) : SchemeObject.T =
    VAR 
      res : REFANY;
    BEGIN
      IF NOT tbl.get(t,res) THEN 
        TYPECASE t OF
          NULL => res := NIL
        |
          RLiteral   (l) =>
          res := L(S("literal"),l.name)
        |
          RConstant  (c) =>
          res := L(S("constant"), Number(c.value))
        |
          RRange     (r) =>
          res := L(S("range"), Number(r.x1), Number(r.x2))
        |
          RPlus      (p) =>
          res := L(S("+"), Recurse(p.a), Recurse(p.b))
        |
          RFromPair  (f) =>
          TYPECASE f.p OF
            PPair(pp) =>
            IF f.whch = XY.X THEN 
              res := Recurse(pp.x) 
            ELSE 
              res := Recurse(pp.y) 
            END
          ELSE
            res := L(S("frompair"), 
                   Recurse(f.p), 
                   ARRAY XY OF SchemeSymbol.T { S("X"),S("Y") }[f.whch])
          END
        |
          PPair      (p) =>
          res := L(S("pair"), Recurse(p.x), Recurse(p.y))
        |
          RSelectP   (s) =>
          IF NUMBER(s.by^) = 1 THEN
            res := Recurse(s.val[0])
          ELSE
            res := L(S("select"), LLR(s.by), LLP(s.val))
          END
        |
          RFunc      (f) =>
          res := L(S("func"), f, Recurse(f.of))
        ELSE
          <*ASSERT FALSE*>
        END;
        EVAL tbl.put(t,res)
      END;
      RETURN res
    END Recurse;

  VAR 
    tbl := NEW(ArithRefTbl.Default).init();
  BEGIN
    RETURN Recurse(t)
  END ToScheme;

PROCEDURE ToSchemePlain(t : Arith.T) : SchemeObject.T =
  (* plain unoptimized version *)
  BEGIN
    TYPECASE t OF
      NULL => RETURN NIL
    |
      RLiteral   (l) =>
      RETURN L(S("literal"),l.name)
    |
      RConstant  (c) =>
      RETURN L(S("constant"), Number(c.value))
    |
      RRange     (r) =>
      RETURN L(S("range"), Number(r.x1), Number(r.x2))
    |
      RPlus      (p) =>
      RETURN L(S("+"), ToScheme(p.a), ToScheme(p.b))
    |
      RFromPair  (f) =>
      RETURN L(S("frompair"), 
               ToScheme(f.p), 
               ARRAY XY OF SchemeSymbol.T { S("X"),S("Y") }[f.whch])
    |
      PPair      (p) =>
      RETURN L(S("pair"), ToScheme(p.x), ToScheme(p.y))
    |
      RSelectP   (s) =>
      RETURN L(S("select"), LLR(s.by), LLP(s.val))
    |
      RFunc      (f) =>
      RETURN L(S("func"), f, ToScheme(f.of))
    ELSE
      <*ASSERT FALSE*>
    END
  END ToSchemePlain;

BEGIN END ArithRep.
