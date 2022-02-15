MODULE Aliases;
IMPORT Rd, Text;
IMPORT Thread;

REVEAL
  Tokenizer = PubTokenizer BRANDED Brand OBJECT
    buf : TEXT;
    rd  : Rd.T;
    lNo : CARDINAL;
  METHODS
    tokenOrNull(VAR tok : TEXT; VAR sep : CHAR) : BOOLEAN
      RAISES { Rd.Failure, Thread.Alerted } := TToken;
  OVERRIDES
    init := TInit;
    token := RToken;
    whatLine := TWhatLine;
  END;

PROCEDURE TWhatLine(t : Tokenizer) : CARDINAL =
  BEGIN RETURN t.lNo END TWhatLine;

PROCEDURE TInit(t : Tokenizer; rd : Rd.T) : Tokenizer =
  BEGIN
    t.rd  := rd;
    t.lNo := 0;
    t.buf := NIL;
    RETURN t
  END TInit;

CONST Seps = SET OF CHAR { ' ', ':' };
      
PROCEDURE RToken(t : Tokenizer; VAR tok : TEXT; VAR sep : CHAR) : BOOLEAN
  RAISES { Rd.Failure, Thread.Alerted } =
  VAR
    lTok : TEXT;
    lSep : CHAR;
    res  : BOOLEAN;
  BEGIN
    REPEAT
      res := t.tokenOrNull(lTok, lSep)
    UNTIL NOT res OR Text.Length(lTok) # 0;
    
    IF res THEN tok := lTok; sep := lSep END;
    RETURN res
  END RToken;

PROCEDURE TToken(t : Tokenizer; VAR tok : TEXT; VAR sep : CHAR) : BOOLEAN
  RAISES { Rd.Failure, Thread.Alerted } =
  BEGIN
    WHILE t.buf = NIL OR Text.Length(t.buf) = 0 DO
      TRY
        t.buf := Rd.GetLine(t.rd);
        INC(t.lNo);
      EXCEPT
        Rd.EndOfFile => RETURN FALSE
      END
    END;
      
    (* buf is non-NIL and length > 0 *)

    FOR i := 0 TO Text.Length(t.buf) - 1 DO
      (* scan for the first separator *)
      WITH c = Text.GetChar(t.buf, i) DO
        IF c IN Seps THEN
          sep   := c;
          tok   := Text.Sub(t.buf, 0     , i);
          t.buf := Text.Sub(t.buf, i + 1 , LAST(CARDINAL));
          RETURN TRUE
        END
      END
    END;

    (* if we get here, there is no separator, so the separator is the NL *)
    sep   := '\n';
    tok   := t.buf;
    t.buf := NIL;
    RETURN TRUE
  END TToken;
  
BEGIN END Aliases.
