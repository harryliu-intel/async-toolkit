INTERFACE RecursiveParserRep;
IMPORT RecursiveParser;
IMPORT Rd;

CONST 
  BufSize = 4096; 
  (* could make this interface generic and get this from elsewhere *)


TYPE
  Rep = RecursiveParser.Public OBJECT 
    buff  : Buffer;
    token : String;
    state : State;
    eop   := FALSE; (* done parsing *)
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

REVEAL 
  RecursiveParser.T = Rep BRANDED Brand OBJECT END;

CONST Brand = "RecursiveParserRep";

END RecursiveParserRep.
