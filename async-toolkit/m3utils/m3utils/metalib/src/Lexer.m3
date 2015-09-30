MODULE Lexer;
IMPORT Rd;

CONST DQ='"';

  PROCEDURE GetToken(VAR buff : ARRAY OF CHAR; 
                     VAR state : State;
                     VAR res : String) : BOOLEAN =

    CONST WhiteSpace = SET OF CHAR { ' ', '\n', '\r', '\t' };
    CONST Special    = SET OF CHAR { '(', ')', '{', '}', '-', '+', ',', '~', '>' };

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
        INC(res.n);
        IF res.start+res.n = state.e THEN 
          res.start := res.start - Shift(); Fill() 
        END;
        <*ASSERT res.start+res.n <= state.e OR state.eof *>
      END Char;

    BEGIN
      WHILE state.s = state.e OR buff[state.s] IN WhiteSpace DO 
        IF    state.s = state.e AND state.eof THEN 
          RETURN FALSE
        ELSIF state.s = state.e         THEN
          EVAL Shift(); Fill()
        ELSIF buff[state.s] IN WhiteSpace THEN
          INC(state.s)
        END
      END;
      (* start of token *)
      res.start := state.s; res.n := 0; 
      IF    buff[state.s] = DQ       THEN
        Char(); WHILE buff[state.s+res.n] # DQ DO Char() END; Char()
      ELSIF buff[state.s] IN Special THEN
        Char()
      ELSE  (* identifier *)
        Char();
        WHILE NOT buff[state.s+res.n] IN WhiteSpace AND
              NOT buff[state.s+res.n] IN Special    AND
                  buff[state.s+res.n] # DQ                   DO
          Char()
        END
      END;
      state.s := state.s+res.n; (* advance s *)
(*
      IO.Put("token: " & Text.FromChars(SUBARRAY(buff,res.start,res.n)) &"\n");      
*)
      RETURN TRUE 
  END GetToken;

BEGIN END Lexer.
