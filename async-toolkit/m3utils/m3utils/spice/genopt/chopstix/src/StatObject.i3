INTERFACE StatObject;
IMPORT SchemeSymbol;

TYPE
  T = OBJECT METHODS
    nom  (nm : SchemeSymbol.T) : LONGREAL;
    mu   (nm : SchemeSymbol.T) : LONGREAL;
    sigma(nm : SchemeSymbol.T) : LONGREAL;
  END;

  Point = T OBJECT METHODS
    samplesize () : CARDINAL;
  END;

  DefaultPoint <: PubDefaultPoint;

  PubDefaultPoint = Point OBJECT METHODS
    init() : DefaultPoint;
    define(nm : SchemeSymbol.T; nom, mu, sigma : LONGREAL);
  END;
  
CONST Brand = "StatObject";

END StatObject.
