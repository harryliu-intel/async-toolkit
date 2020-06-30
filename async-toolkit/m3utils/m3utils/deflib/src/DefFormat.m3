MODULE DefFormat;
IMPORT Rd;
FROM DefLexer IMPORT GetToken, String, State, Buffer, BufSize;
IMPORT Debug;
IMPORT Text;

TYPE Name = REFANY;

PROCEDURE Parse(rd : Rd.T) =

  PROCEDURE GetCard(VAR c : CARDINAL) : BOOLEAN =
    VAR res := 0;
    BEGIN
      FOR i := token.start TO token.start+token.n-1 DO
        WITH num = ORD(buff[i])-ORD('0') DO
          IF num < 0 OR num > 9 THEN RETURN FALSE END;
          res := 10*res+num
        END
      END;
      c := res;
      Next();
      D("Card"); RETURN TRUE
    END GetCard;

  PROCEDURE Error() =
    BEGIN
      Debug.Error("PARSE ERROR, lately reading: " & Text.FromChars(SUBARRAY(buff,token.start,token.n)))
    END Error;

  PROCEDURE Next() =
    BEGIN eop := NOT GetToken(buff,state,token) END Next;

  PROCEDURE GetIdentifier(VAR name : Name) : BOOLEAN =
    BEGIN
      name := NIL;
      Next();
      D("Identifier"); RETURN TRUE
    END GetIdentifier;

  <*NOWARN*>PROCEDURE D(what : TEXT) = BEGIN (*IO.Put(what & "\n")*) END D;

  VAR
    (* parsing *)
    buff : Buffer;
    eop  := FALSE; (* done parsing *)
    token : String;
    state  : State;
  BEGIN
    
    state.rd := rd;

    Next(); (* establish lookahead *)

    LOOP
      Debug.Out("Token \"" & S2T(buff, token) & "\"");
      Next()
    END

  END Parse;

PROCEDURE S2T(READONLY buff : Buffer; s : String) : TEXT =
  BEGIN RETURN Text.FromChars(SUBARRAY(buff, s.start, s.n)) END S2T;

BEGIN END DefFormat.
