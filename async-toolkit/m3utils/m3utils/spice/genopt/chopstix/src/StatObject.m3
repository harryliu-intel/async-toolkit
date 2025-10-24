MODULE StatObject;
IMPORT SymbolLRTbl;
IMPORT SchemeSymbol;

REVEAL
  DefaultPoint = PubDefaultPoint BRANDED Brand & " DefaultPoint" OBJECT
    nomTbl, muTbl, sigmaTbl : SymbolLRTbl.T;
  OVERRIDES
    init := PSOInit;
    define := PSODefine;
    nom        := PSOnom;
    mu         := PSOmu;
    sigma      := PSOsigma;
  END;

PROCEDURE PSOnom(me : DefaultPoint; nm : SchemeSymbol.T) : LONGREAL =
  VAR
    x : LONGREAL;
  BEGIN
    WITH hadIt = me.nomTbl.get(nm, x) DO
      <*ASSERT hadIt*>
      RETURN x
    END
  END PSOnom;

PROCEDURE PSOmu(me : DefaultPoint; nm : SchemeSymbol.T) : LONGREAL =
  VAR
    x : LONGREAL;
  BEGIN
    WITH hadIt = me.muTbl.get(nm, x) DO
      <*ASSERT hadIt*>
      RETURN x
    END
  END PSOmu;

PROCEDURE PSOsigma(me : DefaultPoint; nm : SchemeSymbol.T) : LONGREAL =
  VAR
    x : LONGREAL;
  BEGIN
    WITH hadIt = me.sigmaTbl.get(nm, x) DO
      <*ASSERT hadIt*>
      RETURN x
    END
  END PSOsigma;

PROCEDURE PSOInit(me : DefaultPoint) : DefaultPoint =
  BEGIN
    me.nomTbl := NEW(SymbolLRTbl.Default).init();
    me.muTbl := NEW(SymbolLRTbl.Default).init();
    me.sigmaTbl := NEW(SymbolLRTbl.Default).init();
    RETURN me
  END PSOInit;

PROCEDURE PSODefine(me : DefaultPoint;
                    nm : SchemeSymbol.T;
                    nom, mu, sigma : LONGREAL) =
  BEGIN
    EVAL me.nomTbl.put  (nm, nom);
    EVAL me.muTbl.put   (nm, mu);
    EVAL me.sigmaTbl.put(nm, sigma);
  END PSODefine;

BEGIN END StatObject.
