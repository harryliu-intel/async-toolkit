INTERFACE StatObject;
IMPORT SchemeSymbol;

TYPE
  T = OBJECT METHODS
    nom  (nm : SchemeSymbol.T) : LONGREAL;
    mu   (nm : SchemeSymbol.T) : LONGREAL;
    sigma(nm : SchemeSymbol.T) : LONGREAL;
  END;

CONST Brand = "StatObject";

END StatObject.
