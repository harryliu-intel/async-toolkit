MODULE BraceParse;
IMPORT Rd, Thread;
FROM Fmt IMPORT F, Int;
IMPORT Debug;
IMPORT Text;

CONST BufSiz = 80;
TYPE  Buffer = ARRAY [ 0 .. BufSiz-1 ] OF CHAR;

CONST WhiteSpace = SET OF CHAR { ' ', '\t', '\n', '\r' };
      Special    = SET OF CHAR { '{', '}', '=' };
      
PROCEDURE Parse(rd : Rd.T)
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =

  VAR
    buf       : Buffer;
    totBytes  : CARDINAL := 0;

    b : CARDINAL := 0;         (* buffer pointer  *)
    e : CARDINAL := 0;         (* end of buffer   *)
    s : CARDINAL := BufSiz;    (* start of token  *)

  PROCEDURE Refill() : BOOLEAN  =
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
        IF len = 0 THEN RETURN FALSE END;
        INC(totBytes, len);
        e := b + len;
      END;

      <*ASSERT b # e*>
      RETURN TRUE
    END Refill;

  PROCEDURE GetToken() : BOOLEAN =
    BEGIN
      (* ensure we have text *)
      IF b = e AND NOT Refill() THEN RETURN FALSE END;
      
      (* read token from b onwards *)
      WHILE buf[b] IN WhiteSpace DO
        INC(b); (* skip *)
        
        IF b = e AND NOT Refill() THEN RETURN FALSE END
      END;

      (* buf[b] is NOT whitespace : we are at start of token *)
      s := b;

      (* check for single character token *)
      <*ASSERT b # e*>
      IF    buf[b] IN Special THEN
        INC(b); 
        RETURN TRUE
      END;

      <*ASSERT b # e*>
      WHILE NOT buf[b] IN Special + WhiteSpace DO
        INC(b); 

        IF b = e AND NOT Refill() THEN RETURN FALSE END
      END;

      (* we are at the end of a token *)
      RETURN TRUE
    END GetToken;

  BEGIN
    WHILE GetToken() DO
      Debug.Out(F("Got token %s",
                  Text.FromChars(SUBARRAY(buf, s, b - s))))
    END;

    Debug.Out(F("Read %s bytes", Int(totBytes)))
  END Parse;

BEGIN
END BraceParse.
