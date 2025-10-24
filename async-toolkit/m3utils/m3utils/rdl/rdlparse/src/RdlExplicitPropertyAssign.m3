MODULE RdlExplicitPropertyAssign;
FROM Fmt IMPORT F, Bool;
IMPORT RdlProperty;
IMPORT RdlPropertyAssignRhs;
IMPORT RdlPropertyModifier;

PROCEDURE Format(t : T) : TEXT =
  VAR
    modS := "";
  BEGIN
    IF t.haveModifier THEN modS := RdlPropertyModifier.Names[t.modifier] END;
    
    RETURN F("haveModifier=%s modifier=%s default=%s property=%s rhs=%s",
             Bool(t.haveModifier),
             modS,
             Bool(t.default),
             RdlProperty.Format(t.property),
             RdlPropertyAssignRhs.Format(t.rhs))
  END Format;

BEGIN END RdlExplicitPropertyAssign.
