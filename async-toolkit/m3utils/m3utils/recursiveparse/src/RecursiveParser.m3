MODULE RecursiveParser;

IMPORT RecursiveParserRep;
FROM RecursiveLexer IMPORT Buffer, String;
FROM ParseError IMPORT E;
IMPORT Text;
IMPORT Debug;
IMPORT Rd, Thread;
IMPORT Fmt;

VAR doDebug := Debug.DebugThis("RecursiveParser");

CONST FC = Text.FromChars;

PROCEDURE S2T(READONLY buff : Buffer; s : String) : TEXT =
  BEGIN RETURN Text.FromChars(SUBARRAY(buff, s.start, s.n)) END S2T;

(**********************************************************************)

PROCEDURE GetToken(t : T; READONLY tok : ARRAY OF CHAR) : BOOLEAN
  RAISES { E } =
  BEGIN
    IF SUBARRAY(t.buff, t.token.start, t.token.n) = tok THEN
      Next(t);
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END GetToken;

PROCEDURE PeekToken(t : T; READONLY tok : ARRAY OF CHAR) : BOOLEAN =
  BEGIN
    RETURN SUBARRAY(t.buff, t.token.start, t.token.n) = tok
  END PeekToken;

PROCEDURE Next(t : T) RAISES { E } =
  BEGIN
    TRY
      t.eop := NOT t.lexer.getToken(t.buff, t.state, t.token) ;
      IF t.eop THEN
        t.state.s := t.state.e;
        t.token.n := 0;
        Debug.Out("RecursiveParser.Next: parsing done")
      END;
      IF doDebug THEN
        Debug.Out(Fmt.F("EOP=%s, token=\"%s\"",
                        Fmt.Bool(t.eop),
                        FC(SUBARRAY(t.buff, t.token.start, t.token.n))))
      END;

    EXCEPT
      Rd.Failure => RAISE E ("Got Rd.Failure from Lexer")
    |
      Thread.Alerted => RAISE E ("Got Thread.Alerted from Lexer")
    END;                                  
      
    (*Debug.Out("Token \"" & S2T(t.buff, t.token) & "\"")*)
  END Next;

PROCEDURE MustBeToken(t : T; READONLY tok : ARRAY OF CHAR) RAISES { E } =
  BEGIN
    IF NOT GetToken(t, tok) THEN
      RAISE E("MustBeToken: "&BrackOrEmpty(t.lately.nm)&" expected '" & A2T(tok) & 
        "' but got \"" & S2T(t.buff, t.token) & "\"")
    END
  END MustBeToken;

PROCEDURE MustNotBeChar(t : T; c : CHAR) RAISES { E } =
  BEGIN
    IF t.token.n = 1 AND t.buff[t.token.start] = c THEN
      RAISE E("MustNotBeChar: "&BrackOrEmpty(t.lately.nm)&" illegal \"" & 
            S2T(t.buff, t.token) & "\"")
    ELSE
      Next(t)
    END
  END MustNotBeChar;

PROCEDURE MustBeChars(t : T; READONLY a : ARRAY OF CHAR) RAISES { E } =
  BEGIN
    IF t.token.n # NUMBER(a) OR 
       SUBARRAY(t.buff, t.token.start, t.token.n) # a THEN
      RAISE E("MustBeChars: "&BrackOrEmpty(t.lately.nm)&" expected '" & Text.FromChars(a) & 
        "' but got \"" & S2T(t.buff, t.token) & "\"")
    END;
    Next(t)
  END MustBeChars;

PROCEDURE MustBeCharSet(t : T; s : SET OF CHAR; VAR c : CHAR) RAISES { E } =
  BEGIN
    IF t.token.n # 1 OR NOT t.buff[t.token.start] IN s THEN
      RAISE E("MustBeCharSet: unexpected \"" & S2T(t.buff, t.token) & "\"")
    END;
    c := t.buff[t.token.start];
    Next(t)
  END MustBeCharSet;

PROCEDURE BrackOrEmpty(txt : TEXT) : TEXT =
  BEGIN
    IF txt = NIL THEN RETURN "" ELSE RETURN "["&txt&"]" END
  END BrackOrEmpty;

PROCEDURE GetChar(t : T; c : CHAR) : BOOLEAN RAISES { E } =
  BEGIN
    IF t.token.n = 1 AND t.buff[t.token.start] = c THEN
      Next(t);
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END GetChar;

PROCEDURE MustBeChar(t : T;  c : CHAR) RAISES { E } =
  BEGIN
    IF NOT GetChar(t, c) THEN
      RAISE E("MustBeChar: "&BrackOrEmpty(t.lately.nm)&" expected '"&Text.FromChar(c)&"' but got \"" & S2T(t.buff, t.token) & "\"")
    END
  END MustBeChar;

PROCEDURE GetCharSet(t : T; s : SET OF CHAR; VAR c : CHAR) : BOOLEAN RAISES { E }  =
  BEGIN
    IF t.token.n = 1 AND t.buff[t.token.start] IN s THEN
      c := t.buff[t.token.start];
      Next(t);
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END GetCharSet;

PROCEDURE MustBeSingle(t : T; VAR c : CHAR) RAISES { E } =
  BEGIN
    IF NOT GetSingle(t, c) THEN
      RAISE E("MustBeSingle: "&BrackOrEmpty(t.lately.nm)&" expected single but got \"" & S2T(t.buff, t.token) & "\"")
    END
  END MustBeSingle;

PROCEDURE GetSingle(t : T; VAR c : CHAR) : BOOLEAN RAISES { E } =
  BEGIN
    IF t.token.n = 1 THEN
      c := t.buff[t.token.start];
      Next(t);
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END GetSingle;

BEGIN END RecursiveParser.
