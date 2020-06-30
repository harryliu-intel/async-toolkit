MODULE DefLexer;
IMPORT Rd;
IMPORT Debug, Fmt;

CONST DQ='"';

CONST CC = '#'; (* comment character *)
CONST NL = SET OF CHAR { '\n', '\r' };
CONST WhiteSpace    = SET OF CHAR { ' ', '\n', '\r', '\t' };
CONST BS = '\\';


PROCEDURE GetToken(VAR buff  : Buffer;
                   VAR state : State;
                   VAR res   : String) : BOOLEAN =

  PROCEDURE Fill() =
    (* refill buffer *)
    BEGIN
      WITH space = NUMBER(buff)-state.e,
           chars = Rd.GetSub(state.rd, SUBARRAY(buff, state.e, space)) DO
        state.e := state.e + chars;

        (* add known whitespace at end, makes finding tokens easier *)
        IF chars < space THEN buff[state.e] := '\n'; INC(state.e); state.eof := TRUE END
      END
    END Fill;

  PROCEDURE Shift() : CARDINAL = 
    VAR old := state.s;
    BEGIN
      SUBARRAY(buff, 0, state.e-state.s) := SUBARRAY(buff, state.s, state.e-state.s);
      state.e := state.e-state.s; state.s := state.s-state.s;
      RETURN old
    END Shift;

    PROCEDURE Char() =

      (* move 1 char *)
      BEGIN
        lastIsBs := buff[res.start + res.n] = BS;

        IF lastIsBs THEN
          (* shift buff 1 char to left *)
          VAR
            cur := res.start + res.n;
          BEGIN
            IF cur # state.e THEN
              SUBARRAY(buff, cur, state.e - cur - 1) :=
                  SUBARRAY(buff, cur + 1, state.e - cur - 1)
            END;
            DEC(state.e)
          END
        ELSE
          INC(res.n)
        END;

        IF res.start + res.n = state.e THEN 
          res.start := res.start - Shift(); Fill() 
        END;
        <*ASSERT res.start+res.n <= state.e OR state.eof *>
      END Char;

    VAR 
      inComment := FALSE;
      lastIsBs  := FALSE;
    BEGIN
      WHILE state.s = state.e OR 
            buff[state.s] IN WhiteSpace OR
            buff[state.s] = CC OR
            inComment DO 
        IF    state.s = state.e AND state.eof THEN 
          RETURN FALSE
        ELSIF state.s = state.e         THEN
          EVAL Shift(); Fill()
        ELSIF inComment AND NOT buff[state.s] IN NL THEN
          INC(state.s)
        ELSIF inComment AND buff[state.s] IN NL THEN
          inComment := FALSE;
          INC(state.s)
        ELSIF buff[state.s] IN WhiteSpace THEN
          INC(state.s)
        ELSIF buff[state.s] = CC THEN
          inComment := TRUE;
          INC(state.s)
        END
      END;
      (* start of token *)
      res.start := state.s; res.n := 0; 
      IF    buff[state.s] = DQ       THEN
        Char(); 
        WHILE buff[state.s+res.n] # DQ DO 
          Char() ;
          IF state.s + res.n = BufSize THEN
            Debug.Error("DefLexer.GetToken : string too long (n >= " & Fmt.Int(res.n) & ", endPos >= "& Fmt.Int(state.s + res.n)& " )")
          END;
        END; 
        Char()
      ELSIF buff[state.s] IN state.special THEN
        Char()
      ELSE  (* identifier *)
        Char();
        WHILE NOT buff[state.s + res.n] IN WhiteSpace     AND
              NOT buff[state.s + res.n] IN state.special  AND
                  buff[state.s + res.n] # CC              AND
                  buff[state.s + res.n] # DQ                   DO
          Char()
        END
      END;
      state.s := state.s+res.n; (* advance s *)
(*
      IO.Put("token: " & Text.FromChars(SUBARRAY(buff,res.start,res.n)) &"\n");      
*)
      RETURN TRUE 
  END GetToken;

PROCEDURE DividerChar(VAR s : State; c : CHAR) =
  BEGIN
    s.divChar := c;
    s.special := BaseSpecial + SET OF CHAR { s.divChar } + SET OF CHAR { s.busbitChars[0] } + SET OF CHAR { s.busbitChars[1] };
  END DividerChar;

PROCEDURE BusbitChars(VAR s : State; c : ARRAY [0..1] OF CHAR) =
  BEGIN
    s.busbitChars := c;
    s.special := BaseSpecial + SET OF CHAR { s.divChar } + SET OF CHAR { s.busbitChars[0] } + SET OF CHAR { s.busbitChars[1] };
  END BusbitChars;

BEGIN END DefLexer.
