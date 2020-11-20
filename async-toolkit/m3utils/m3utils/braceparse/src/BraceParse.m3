MODULE BraceParse;
IMPORT Rd, Thread;
FROM Fmt IMPORT F, Int;
IMPORT Debug;
IMPORT Text;

CONST BufSiz = 80;
TYPE  Buffer = ARRAY [ 0 .. BufSiz-1 ] OF CHAR;

TYPE SC = SET OF CHAR;
     
CONST WhiteSpace = SC { ' ', '\t', '\n', '\r' };
      Special    = SC { '{', '}', '=' };
      Digit      = SC { '0' .. '9' };
      Lower      = SC { 'a' .. 'z' };
      Upper      = SC { 'A' .. 'Z' };
      Letter     = Lower + Upper;
      Ident1     = Letter + SC { '_' };
      Ident      = Ident1 + Digit;

VAR doDebug := TRUE;
    
PROCEDURE Parse(rd : Rd.T)
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =

  VAR
    buf       : Buffer;
    totBytes  : CARDINAL := 0;

    b : CARDINAL := 0;         (* buffer pointer  *)
    e : CARDINAL := 0;         (* end of buffer   *)
    s : CARDINAL := BufSiz;    (* start of token  *)

    lev := 0;

  PROCEDURE Refill() RAISES { Rd.EndOfFile }  =
    BEGIN
      (* existing token starts at s, b is at end *)
      
      (* shift down the old token *)
      WITH tokSoFar = BufSiz - s DO
        SUBARRAY(buf, 0, tokSoFar) := SUBARRAY(buf, s, tokSoFar);
        s := 0;
        b := tokSoFar
      END;

      (* refill buffer *)
      WITH len = Rd.GetSub(rd, SUBARRAY(buf, b, BufSiz - b)) DO
        IF len = 0 THEN RAISE Rd.EndOfFile END;
        INC(totBytes, len);
        e := b + len;
      END;

      <*ASSERT b # e*>
    END Refill;

  VAR haveTok := FALSE;

  PROCEDURE NextToken() 
    RAISES { Rd.EndOfFile } =
    BEGIN
      (* we should have consumed previous token *)
      <*ASSERT NOT haveTok*>
      
      (* ensure we have text *)
      IF b = e THEN Refill() END;
      
      (* read token from b onwards *)
      WHILE buf[b] IN WhiteSpace DO
        INC(b); (* skip *)
        
        IF b = e THEN Refill() END;
      END;

      (* buf[b] is NOT whitespace : we are at start of token *)
      s := b;

      (* check for single character token *)
      <*ASSERT b # e*>
      IF    buf[b] IN Special THEN
        IF    buf[b] = '{' THEN INC(lev)
        ELSIF buf[b] = '}' THEN DEC(lev)
        END;
        
        INC(b);
        haveTok := TRUE;
        RETURN 
      END;

      <*ASSERT b # e*>
      WHILE NOT buf[b] IN Special + WhiteSpace DO
        INC(b); 

        IF b = e THEN Refill() END
      END;

      (* we are at the end of a token *)
      haveTok := TRUE
    END NextToken;

  PROCEDURE GetToken(VAR tok : Token) : BOOLEAN
    RAISES { Rd.EndOfFile } =
    BEGIN
      IF NOT haveTok THEN NextToken() END;
      
      tok.s := s;
      tok.b := b;
      haveTok := FALSE;
      RETURN NOT haveTok
    END GetToken;

  VAR
    dummy : Token;
  BEGIN
    TRY
      WHILE GetToken(dummy) DO
        IF doDebug THEN
          Debug.Out(F("Lev %s got token %s",
                      Int(lev),
                      Text.FromChars(SUBARRAY(buf,
                                              dummy.s,
                                              dummy.b - dummy.s))))
        END
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    END;

    Debug.Out(F("Read %s bytes", Int(totBytes)))
  END Parse;

TYPE Token = RECORD s, b : CARDINAL END;
     
BEGIN
END BraceParse.
