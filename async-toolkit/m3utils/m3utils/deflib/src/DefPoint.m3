MODULE DefPoint;
IMPORT RecursiveParser;
FROM DefNumbers IMPORT MustBeInt;
FROM RecursiveParser IMPORT MustBeChar, GetChar;
FROM ParseError IMPORT E;

PROCEDURE MustBe(t : RecursiveParser.T; VAR p : T) RAISES { E } =
  BEGIN
    MustBeChar(t,'(');
    MustBeInt(t, p.x);
    MustBeInt(t, p.y);
    MustBeChar(t,')');
  END MustBe;

PROCEDURE Get(t : RecursiveParser.T; VAR p : T) : BOOLEAN RAISES { E } =
  (* a bit of a hack because of the LL(1) capability here *)
  BEGIN
    IF NOT GetChar(t, '(') THEN
      RETURN FALSE
    END;
    MustBeInt(t, p.x);
    MustBeInt(t, p.y);
    MustBeChar(t,')');
    RETURN TRUE
  END Get;

BEGIN END DefPoint.

