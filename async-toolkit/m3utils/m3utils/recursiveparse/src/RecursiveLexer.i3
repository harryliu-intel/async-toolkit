INTERFACE RecursiveLexer;
IMPORT Rd;
IMPORT Thread;

CONST 
  BufSize = 4096; 
  (* could make this interface generic and get this from elsewhere *)

TYPE
  T = OBJECT METHODS
    getToken(VAR buff : Buffer; VAR state : State; VAR token : String) : BOOLEAN RAISES { Thread.Alerted, Rd.Failure };
  END;

  State = RECORD
    rd : Rd.T;
    s    := 0;     (* next token to parse starts here *)
    e    := 0;     (* end of currently valid chars in buffer *)
    eof  := FALSE; (* done parsing *)
    line := 1;
  END;

TYPE Buffer = ARRAY [0..BufSize-1] OF CHAR;

TYPE String = RECORD start : [0..BufSize-1]; n : [0..BufSize] END;


CONST Brand = "RecursiveLexer";

END RecursiveLexer.
