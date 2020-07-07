MODULE DefIdent;
IMPORT RecursiveParser;
FROM RecursiveParser IMPORT Next, S2T, BrackOrEmpty;
IMPORT RecursiveParserRep;
IMPORT DefLexer;
FROM DefLexer IMPORT Digit;
FROM ParseError IMPORT E;
FROM DefFormat IMPORT D;
IMPORT Text;

PROCEDURE Get(t : RecursiveParser.T; VAR ident : T) : BOOLEAN =
  (* we could use a char buffer instead of TEXT here to reduce mem alloc *)

  (* this needs to handle multiple arcs and arraying! *)

  VAR
    ok := FALSE;
  BEGIN
    (* check its not a special character or a number *)
    IF    t.token.n = 0 THEN 
      RETURN FALSE
    ELSIF t.buff[t.token.start] IN NARROW(t.lexer,DefLexer.T).special THEN
      <*ASSERT t.token.n = 1*>
      RETURN FALSE
    ELSE
      FOR i := t.token.start TO t.token.start + t.token.n - 1 DO
        IF NOT t.buff[i] IN Digit THEN 
          ok := TRUE
        END
      END
    END;

    IF NOT ok THEN RETURN FALSE END;
      
    ident := Text.FromChars(SUBARRAY(t.buff, t.token.start, t.token.n));
    Next(t);
    D("Identifier"); 
    RETURN TRUE
  END Get;

PROCEDURE MustBe(t : RecursiveParser.T; VAR ident : T) RAISES { E } =
  BEGIN
    IF NOT Get(t, ident) THEN
      RAISE E ("DefIdent.MustBe: " & BrackOrEmpty(t.lately.nm) & " expected identifier here : " & S2T(t.buff, t.token))
    END;
  END MustBe;

PROCEDURE MustGet(t : RecursiveParser.T) : T RAISES { E } =
  VAR
    ident : T;
  BEGIN
    IF Get(t, ident) THEN
      RETURN ident
    ELSE
      RAISE E ("DefIdent.MustBe: " & BrackOrEmpty(t.lately.nm) & " expected identifier here : " & S2T(t.buff, t.token))
    END;
  END MustGet;

BEGIN END DefIdent.

