(* $Id$ *)

MODULE BDDOpsH;
IMPORT BDDSet, BDD;
IMPORT Word;
IMPORT SopBDD;
IMPORT TextSet;

PROCEDURE AccumulateBDD(s  : BDDSet.T; 
                        op : PROCEDURE(a, b : BDD.T) : BDD.T;
                        init : BDD.T) : BDD.T =
  VAR
    q    := init;
    iter := s.iterate();
    b : BDD.T;
  BEGIN
    WHILE iter.next(b) DO q := op(q,b) END;
    RETURN q
  END AccumulateBDD;

PROCEDURE MakeCntrBdd(READONLY a : ARRAY OF BDD.T; cntr : Word.T) : BDD.T =
  VAR bdd := BDD.True(); BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      IF Word.Extract(cntr, i, 1) = 1 THEN
        bdd := BDD.And(bdd, a[i])
      ELSE
        bdd := BDD.And(bdd, BDD.Not(a[i]))
      END
    END;
    RETURN bdd
  END MakeCntrBdd;

PROCEDURE PfxFormat(x : BDD.T; 
                    pfx : TEXT; 
                    inQuotes : BOOLEAN;
                    aliasMapper : SopBDD.AliasMapper) : TEXT =
  BEGIN
    IF x = NIL THEN RETURN "**NIL**" END;
    WITH tr  = BDD.True(),
         sop = SopBDD.ConvertBool(x) DO
      RETURN sop.invariantSimplify(tr,tr,tr)
                .format(NIL,pfx:=pfx,inQuotes:=inQuotes,
                        aliasMapper:=aliasMapper)
    END
  END PfxFormat;

PROCEDURE XFormat(x : BDD.T; inQuotes : BOOLEAN) : TEXT =
  BEGIN RETURN PfxFormat(x, "", inQuotes, aliasMapper := NIL) END XFormat;

PROCEDURE Substitute(in, v, by : BDD.T) : BDD.T =
  BEGIN
    WITH tx = BDD.MakeTrue (in, v),
         tf = BDD.MakeFalse(in, v) DO
      RETURN BDD.Or(BDD.And(by,tx),BDD.And(BDD.Not(by),tf))
    END
  END Substitute;

PROCEDURE InfixFormatSet(x           : BDDSet.T; 
                         pfx         : TEXT; 
                         inQuotes    : BOOLEAN;
                         aliasMapper : SopBDD.AliasMapper;
                         op          : TEXT) : TEXT =
  VAR res := "";
      iter := x.iterate();
      p : BDD.T;
  BEGIN
    WITH one = iter.next(p) DO
      <*ASSERT one*>
      res := PfxFormat(p, pfx, inQuotes, aliasMapper)
    END;
    WHILE iter.next(p) DO
      res := res & op;
      res := res & PfxFormat(p, pfx, inQuotes, aliasMapper)
    END;
    RETURN res
  END InfixFormatSet;

PROCEDURE InfixFormatNSet(x           : TextSet.T; 
                          pfx         : TEXT; 
                          inQuotes    : BOOLEAN;
                          aliasMapper : SopBDD.AliasMapper;
                          op          : TEXT) : TEXT =
  VAR res := "";
      iter := x.iterate();
      p : TEXT;
  BEGIN
    WITH one = iter.next(p) DO
      <*ASSERT one*>
      res := PfxNFormat(p, pfx, inQuotes, aliasMapper)
    END;
    WHILE iter.next(p) DO
      res := res & op;
      res := res & PfxNFormat(p, pfx, inQuotes, aliasMapper)
    END;
    RETURN res
  END InfixFormatNSet;

PROCEDURE PfxNFormat(x : TEXT;
                     pfx : TEXT; 
                     inQuotes : BOOLEAN;
                     am : SopBDD.AliasMapper) : TEXT = 
  VAR q := "";
  BEGIN
    IF am = NIL THEN am := SopBDD.IdentityMapper END;
    IF inQuotes THEN q := "\"" END;

    RETURN q & am.canon(pfx & x) & q

  END PfxNFormat;

BEGIN END BDDOpsH.
