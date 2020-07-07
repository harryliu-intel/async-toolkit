MODULE DefName;
IMPORT RecursiveParser;
IMPORT RecursiveParserRep;
FROM RecursiveParser IMPORT BrackOrEmpty, GetChar, MustBeChar;
IMPORT DefLexer;
FROM ParseError IMPORT E;
IMPORT DefIdent;
IMPORT DefInt;

PROCEDURE Get(t : RecursiveParser.T; VAR name : T) : BOOLEAN RAISES { E } =
  VAR
    res := FALSE;
    id : TEXT;
    idx : INTEGER;
  BEGIN
    LOOP
      (* identifier *)
      IF    DefIdent.Get(t, id) THEN
        res := TRUE
      ELSE 
        RETURN res
      END;

      (* array index, is optional after any identifier *)
      IF GetChar(t, NARROW(t.lexer,DefLexer.T).busbitChars[0]) THEN
        DefInt.MustBe(t, idx);
        MustBeChar(t, NARROW(t.lexer,DefLexer.T).busbitChars[1])
      END;

      (* separator -- is trailing separator OK, probably not? *)
      (* separator MUST precede next arc *)
      IF NOT GetChar(t, NARROW(t.lexer,DefLexer.T).divChar) THEN
        name := T {};
        RETURN TRUE
      END
    END
  END Get;

PROCEDURE MustBe(t : RecursiveParser.T; VAR name : T) RAISES { E } = 
  BEGIN
    IF NOT Get(t, name) THEN
      RAISE E("DefName.MustBe: "&BrackOrEmpty(t.lately.nm)&" expected name")
    END
  END MustBe;

PROCEDURE MustGet(t : RecursiveParser.T) : T RAISES { E } = 
  VAR 
    name : T;
  BEGIN
    IF Get(t, name) THEN
      RETURN name
    ELSE
      RAISE E("DefName.MustBe: "&BrackOrEmpty(t.lately.nm)&" expected name")
    END
  END MustGet;

BEGIN END DefName.
