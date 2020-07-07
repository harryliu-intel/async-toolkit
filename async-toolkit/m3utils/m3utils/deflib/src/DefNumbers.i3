INTERFACE DefNumbers;
IMPORT RecursiveParser;
FROM ParseError IMPORT E;

PROCEDURE MustBeInt(t : RecursiveParser.T; VAR i : INTEGER) RAISES { E };
PROCEDURE MustBeCard(t : RecursiveParser.T; VAR c : CARDINAL) RAISES { E }; 
PROCEDURE GetCard(t : RecursiveParser.T; VAR c : CARDINAL) : BOOLEAN;

END DefNumbers.
