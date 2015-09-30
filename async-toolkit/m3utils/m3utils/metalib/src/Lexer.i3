INTERFACE Lexer;
IMPORT Rd;

CONST BufSize = 2048;

TYPE 
  State = RECORD
    rd : Rd.T;
    s    := 0;     (* next token to parse starts here *)
    e    := 0;     (* end of currently valid chars in buffer *)
    eof  := FALSE; (* done parsing *)
  END;

TYPE String = RECORD start, n : [0..BufSize-1] END;

PROCEDURE GetToken(VAR buff : ARRAY OF CHAR; 
                   VAR state : State;
                   VAR res : String) : BOOLEAN;

END Lexer.
